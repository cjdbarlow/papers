# Plots the true effect from each simulation to demonstrate the range of effect sizes for each simulation setting
# Setup ----
library(tidyverse)
library(arrow)

simData = read_parquet("data/simData.parquet")

priorData = simData |>
    select(pInt, cv, trueBeta1)

# Empirical null/harm proportion per effect size x CV, straight from the draws
priorLabels = priorData |>
    group_by(pInt, cv) |>
    summarise(
        propNullHarm = mean(trueBeta1 >= 0),
        .groups = "drop"
    )

priorPlot = ggplot(
    priorData,
    aes(
        x = trueBeta1,
        fill = trueBeta1 >= 0
    )
) +
    geom_histogram(bins = 50) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey30") +
    # Null/harm proportion, top-right of each panel (Inf placement survives free scales)
    geom_text(
        data = priorLabels,
        aes(x = Inf, y = Inf, label = scales::percent(propNullHarm, accuracy = 0.1)),
        inherit.aes = FALSE,
        hjust = 1.1,
        vjust = 1.4,
        size = 2.5,
        colour = "#B2182B"
    ) +
    facet_grid(
        pInt ~ cv,
        labeller = labeller(
            pInt = \(x) paste0("pInt = ", x),
            cv = \(x) paste0("CV = ", x)
        ),
        scales = "free"
    ) +
    scale_fill_manual(
        values = c(`FALSE` = "grey80", `TRUE` = "#D6604D"),
        guide = "none"
    ) +
    labs(
        x = "True Effect",
        y = "Count",
        title = "Design prior: True effect size by CV",
        subtitle = "Red = simulated trials with no benefit (β₁ ≥ 0); % = proportion of simulations with no benefit"
    ) +
    theme_minimal()

ggsave(
    "results/priorPlot.png",
    priorPlot,
    width = 36,
    height = 16,
    units = "cm",
    dpi = 320
)
