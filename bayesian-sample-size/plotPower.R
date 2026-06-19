# Power curve plot: Frequentist Assurance vs Bayesian Power vs conventional power
# Setup ----
library(tidyverse)
library(arrow)
library(scam)

simSettings = read_parquet("data/simSettings.parquet")
resultsNormalApprox = readRDS("data/resultsNormalApprox.rds")
resultsAssurance = read_parquet("data/resultsAssurance.parquet")
resultsFreq = read_parquet("data/resultsFreq.parquet")

## Target power thresholds (match the simulation setup)
powerTargets = c(0.8, 0.9)

## One row per risk difference scenario
scenarios = simSettings |>
    distinct(pCon, pInt, riskDiff, OR, nTrialSizeCap)

# Prepare data ----
## Bayesian Power curve: proportion of simulated trials declaring benefit
bayesCurve = resultsNormalApprox |>
    group_by(pInt, cv, nTrialSize) |>
    summarise(
        power = mean(success, na.rm = TRUE),
        .groups = "drop"
    ) |>
    mutate(method = "Bayesian Power")

## Frequentist Assurance curve: Closed-form power averaged over the design prior
assuranceCurve = resultsAssurance |>
    select(pInt, cv, nTrialSize, power = assurancePower) |>
    mutate(method = "Frequentist Assurance")

## One panel per method, ordered assurance then Bayesian
powerCurves = bind_rows(assuranceCurve, bayesCurve) |>
    mutate(
        cv = factor(cv),
        method = factor(method, levels = c("Frequentist Assurance", "Bayesian Power"))
    )

# Plot power curves ----
## One figure for each RD threshold with facets for assurance + Bayesian power
plotPowerCurves = function(scenario) {
    # Trim x axis
    xMaxLookup = c("0.49" = 100000, "0.48" = 50000, "0.45" = 20000, "0.4" = 10000)
    xMax = xMaxLookup[[as.character(scenario$pInt)]]

    ggplot() +
        # Target power thresholds (plot first so they don't overwrite curves)
        geom_hline(
            yintercept = powerTargets,
            linetype = "dashed",
            colour = "grey40",
            linewidth = 0.6
        ) +
        geom_text(
            data = tibble(
                x = xMax * 0.005,
                y = powerTargets + 0.015,
                label = paste0(powerTargets * 100, "%")
            ),
            aes(x = x, y = y, label = label),
            colour = "grey40",
            size = 3,
            hjust = 0
        ) +

        # Assurance / Bayesian curves
        geom_smooth(
            data = powerCurves |>
                filter(
                    pInt == scenario$pInt,
                    nTrialSize <= xMax
                ),
            aes(
                x = nTrialSize,
                y = power,
                colour = cv
            ),
            # Monotone increasing P-spline: power can only rise with sample size
            method = "scam",
            formula = y ~ s(x, k = 8, bs = "mpi"),
            se = FALSE,
            linewidth = 0.7
        ) +

        # Conventional power curve
        ## No method column, so it is drawn in both panels
        ## Mapping linetype gives it its own legend entry
        geom_line(
            data = resultsFreq |>
                filter(
                    pInt == scenario$pInt,
                    nTrialSize <= xMax
                ),
            aes(
                x = nTrialSize,
                y = freqPower,
                linetype = "Frequentist (α=0.025, one-sided,\nassumed effect size)"
            ),
            colour = "black",
            linewidth = 0.9,
        ) +

        # Style
        facet_wrap(vars(method), ncol = 1) +
        scale_colour_manual(
            # David's kustom colour scheme
            values = colorRampPalette(c("#08306B", "#016C59", "#41AE76"))(n_distinct(powerCurves$cv)),
            labels = ~ sprintf("CV = %.2f", as.numeric(.x))
        ) +
        scale_linetype_manual(values = "dotted", name = NULL) +
        scale_y_continuous(
            limits = c(0, 1.01),
            breaks = seq(0, 1, 0.1),
        ) +
        scale_x_continuous(
            breaks = scales::pretty_breaks(n = 4),
            labels = scales::comma
        ) +
        labs(
            x = "Total sample size",
            y = "Power",
            colour = "Design prior CV (SD/|β₁|)",
            title = sprintf(
                "Power vs sample size: RD = %.2f (OR = %.2f)",
                -scenario$riskDiff,
                scenario$OR
            )
        ) +
        theme_classic(base_size = 11) +
        theme(
            legend.position = "right",
            legend.key.width = unit(1.5, "cm"),
            plot.title = element_text(face = "bold")
        )
}

for (i in seq_len(nrow(scenarios))) {
    scenario = scenarios[i, ]
    ggsave(
        sprintf("results/powerPlot_rd%02.0f.png", scenario$riskDiff * 100),
        plotPowerCurves(scenario),
        width = 20,
        height = 22,
        units = "cm",
        dpi = 320
    )
}
