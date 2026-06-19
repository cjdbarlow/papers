# Bayesian power: MCMC vs normal approximation
# Setup ----
library(tidyverse)
library(arrow)
library(scam)

## MCMC summaries (one row per simulated trial)
mcmcSummaries = open_dataset("data/mcmc/summaries") |>
    collect()

## Normal-approximation results
resultsNormalApprox = readRDS("data/resultsNormalApprox.rds")

## MCMC scenario CVs
mcmcCvs = c(0.1, 0.5, 1.19, 1.5)

## Target power thresholds (match the main power plots)
powerTargets = c(0.8, 0.9)

# Prepare data ----
## MCMC Bayesian power
mcmcCurve = mcmcSummaries |>
    group_by(pInt, cv, nArm) |>
    summarise(
        nTotal = first(nArm) * 2,
        power = mean(success, na.rm = TRUE),
        .groups = "drop"
    ) |>
    select(pInt, cv, nTotal, power) |>
    mutate(method = "MCMC")

## Normal-approximation Bayesian power (same cv subset)
naCurve = resultsNormalApprox |>
    filter(cv %in% mcmcCvs) |>
    group_by(pInt, cv, nTrialSize) |>
    summarise(
        power = mean(success, na.rm = TRUE),
        .groups = "drop"
    ) |>
    select(pInt, cv, nTotal = nTrialSize, power) |>
    mutate(method = "Normal approximation")

## Trim x-axis per effect size to the informative region (same as plotPower.R)
xMaxLookup = c("0.4" = 10000, "0.45" = 20000, "0.48" = 50000, "0.49" = 100000)

curves = bind_rows(mcmcCurve, naCurve) |>
    filter(nTotal <= xMaxLookup[as.character(pInt)]) |>
    mutate(
        cv = factor(cv, levels = mcmcCvs),
        method = factor(method, levels = c("MCMC", "Normal approximation"))
    )

# Plot ----
methodCols = c("MCMC" = "#C0504D", "Normal approximation" = "#4472C4")

plotGrid = ggplot(curves, aes(x = nTotal, y = power, colour = method)) +
    geom_hline(
        yintercept = powerTargets,
        linetype = "dashed",
        colour = "grey55",
        linewidth = 0.4
    ) +
    geom_smooth(
        method = "scam",
        formula = y ~ s(x, k = 8, bs = "mpi"),
        se = FALSE,
        alpha = 0.5,
        linewidth = 0.7
    ) +
    facet_grid(
        cv ~ pInt,
        scales = "free_x",
        labeller = labeller(
            pInt = \(x) sprintf("p_int = %.2f", as.numeric(x)),
            cv = \(x) sprintf("CV = %.2f", as.numeric(x))
        )
    ) +
    scale_colour_manual(values = methodCols, name = NULL) +
    scale_y_continuous(limits = c(0, 1.01), breaks = seq(0, 1, 0.25)) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 3),
        labels = scales::comma
    ) +
    labs(
        x = "Total sample size",
        y = "Bayesian power",
        title = "Bayesian power: MCMC vs normal approximation (γ = 0.975)"
    ) +
    theme_classic(base_size = 10) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(
    "results/mcmcVsNormalPower.png",
    plotGrid,
    width = 24,
    height = 20,
    units = "cm",
    dpi = 320
)
