# Libraries
library(tidyverse)
library(forestploter)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

source("functions.R")

# 0 Data ----
## Matched data
data.list = readRDS("matched-data.list.Rds")

data = data.list %>%
    map_df(as.data.frame,
           .id = ".imp")

# 1 Forest plot ----
## Regression models
reg.centre = data.list %>%
    map(~ nnet::multinom(relevel(ecmo_centre_type, ref = "1") ~ ecmo_episode,
                         data = .,
                         weights =.$match.weights,
                         model = TRUE)) %>%
    mice::pool() %>%
    summary(conf.int = TRUE,
            exponentiate = TRUE) %>%
    mutate(var = "ECMO Centre Type",
           y.level = y.level %>%
               recode_factor("0" = "Non-ECMO Centre",
                             "2" = "Major ECMO Centre"))


reg.state = data.list %>%
    map(~ nnet::multinom(relevel(jurisdictionname, "NSW") ~ ecmo_episode,
                         data = .,
                         weights =.$match.weights,
                         model = TRUE)) %>%
    mice::pool() %>%
    summary(conf.int = TRUE,
            exponentiate = TRUE) %>%
    mutate(var = "Region")


reg.aria = data.list %>%
    map(~ filter(., remoteness_cat != "")) %>%
    map(~ droplevels(.)) %>%
    map(~ nnet::multinom(relevel(remoteness_cat, "Major City") ~ ecmo_episode,
                         data = .,
                         weights =.$match.weights,
                         model = TRUE)) %>%
    mice::pool() %>%
    summary(conf.int = TRUE,
            exponentiate = TRUE) %>%
    mutate(var = "Remoteness Classification")


reg.data = rbind(reg.centre, reg.state, reg.aria) %>%
    filter(term == "ecmo_episodeTRUE") %>%
    rename(ci.lo = `2.5 %`,
           ci.hi = `97.5 %`) %>%
    # Make reference categories
    full_join(data.frame(var = c("ECMO Centre Type", "Region", "Remoteness Classification"),
                         y.level = c("Minor ECMO Centre", "NSW", "Major City"),
                         # Setting confidence limits at 0 hides the reference category from the forst plot
                         ci.lo = 0, ci.hi = 0)) %>%
    # Sort them (reference levels are NA for term so we can use this to put them first)
    arrange(var, !is.na(term), y.level)


reg.table = reg.data %>%
    mutate(blank = paste(rep(" ", 40), collapse = " "),
           p.value = ifelse(is.na(p.value),
                            "-",
                            format.pval(p.value, digits = 2, eps = 0.001)),
           across(where(is.numeric), round, digits = 2),
           or = ifelse(is.na(estimate),
                       "Reference",
                       paste0(estimate, " (", ci.lo, " - ", ci.hi, ")")),
           var = ifelse(duplicated(var), "", var)) %>%
    select(var, y.level, blank, or, p.value) %>%
    rename("Covariate" = var,
           "Level" = y.level,
           " " = blank,
           "OR (95% CI)" = or,
           "p" = p.value)
    
forest = forest(data = reg.table,
                est = reg.data$estimate,
                lower = reg.data$ci.lo,
                upper = reg.data$ci.hi,
                sizes = reg.data$std.error,
                # Styling
                ref_line = 1,
                arrow_lab = c("ECMO less likely", "ECMO more likely"),
                xlim = c(0, 2.4),
                ticks_at = seq(0.2, 2.4, 0.4),
                ci_column = 3) %>%
    add_border(part = "header",
               where = "bottom")
forest

ggsave("outputs/figures/forest.jpg", forest,
       dpi = 600, width = 10, height = 8, units = "in")


# 2 Process plot
process = grViz("
    digraph G {
    graph [layout = dot, rankdir = LR, fontname = 'Helvetica']
    node [shape = rectangle, fontname = 'Helvetica']
 
    d [label = <Data <BR/><BR/> Table: Pre-imputed>];
 
    subgraph cluster_1 {
        label = 'Imputation';
        graph[style = dashed];
        a0 [label=<I<SUB>1</SUB>>];
        a1 [label=<I<SUB>2</SUB>>];
        a2 [label=<I<SUB>n</SUB>>];
        a3 [label=<I<SUB>20</SUB>>];
        a4 [label=<Figure: Convergence>, shape = none];
    }

    subgraph cluster_2 {
        label = 'Matching';
        graph[style = dashed];
        b0 [label=<M<SUB>1</SUB>>];
        b1 [label=<M<SUB>2</SUB>>];
        b2 [label=<M<SUB>n</SUB>>];
        b3 [label=<M<SUB>20</SUB>>];
        b4 [label=<Table: Balance>, shape = none];
    }
  
    subgraph cluster_3 {
        label = 'Modelling';
        graph[style = dashed];
        c0 [label=<R<SUB>1</SUB>>];
        c1 [label=<R<SUB>2</SUB>>];
        c2 [label=<R<SUB>n</SUB>>];
        c3 [label=<R<SUB>20</SUB>>];
        #c4 [label=<Figure D>, shape = none];
    }
  
    d -> a0 [headport = 'w'];
    d -> a1 [headport = 'w'];
    d -> a2 [headport = 'w', style = 'dotted'];
    d -> a3 [headport = 'w'];
  
    a0 -> b0 [dir = 'right'];
    a1 -> b1 [dir = 'right'];
    a2 -> b2 [dir = 'right', style = 'dotted'];
    a3 -> b3 [dir = 'right'];
  
    b0 -> c0 [dir = 'right'];
    b1 -> c1 [dir = 'right'];
    b2 -> c2 [dir = 'right', style = 'dotted'];
    b3 -> c3 [dir = 'right'];
    
    p [label = <Pooled Results <BR/><BR/> Figure: Forest Plot>];

    c0 -> p [tailport = 'e'];
    c1 -> p [tailport = 'e'];
    c2 -> p [tailport = 'e', style = 'dotted'];
    c3 -> p [tailport = 'e'];

}")