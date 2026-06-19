# Trial summaries ----
# Reads the simulation outputs and tabulates decision outcomes against the true simulated effects.
library(tidyverse)
library(arrow)

source("functions.R")

resultsNormalApprox = readRDS("data/resultsNormalApprox.rds")
resultsAssurance = read_parquet("data/resultsAssurance.parquet")
resultsFreq = read_parquet("data/resultsFreq.parquet")

## "True effect": the actual effect of that simulation is beneficial (trueBeta1 < 0)
## na.rm guards against the Wald shortcut returning NaN on zero-cell trials
trialSummary = resultsNormalApprox |>
    mutate(trueEffect = trueBeta1 < 0) |>
    group_by(pInt, cv, nTrialSize) |>
    summarise(
        nTrials = n(),
        nTrueEffect = sum(trueEffect),
        nNullOrHarm = sum(!trueEffect),

        # Bayesian rule (posterior P(beta1 < 0) > gamma)
        bayesTP = sum(trueEffect & success, na.rm = TRUE),
        bayesFP = sum(!trueEffect & success, na.rm = TRUE),
        bayesTN = sum(!trueEffect & !success, na.rm = TRUE),
        bayesFN = sum(trueEffect & !success, na.rm = TRUE),

        # Frequentist rule (one-sided Wald test)
        freqTP = sum(trueEffect & freqSuccess, na.rm = TRUE),
        freqFP = sum(!trueEffect & freqSuccess, na.rm = TRUE),
        freqTN = sum(!trueEffect & !freqSuccess, na.rm = TRUE),
        freqFN = sum(trueEffect & !freqSuccess, na.rm = TRUE),

        .groups = "drop"
    )

trialSummaryTrim = trialSummary |>
    filter(
        pInt == 0.45,
        cv %in% c(0.1, 0.5, 1.0, 1.19, 2.0),
        nTrialSize %in% c(1000, 5000, 20000, 50000, 100000)
    )

# Sample size for target power ----
# Total N achieving 80% and 90% power for each method, interpolated from the power curves
# NA means the target power is never reached within the simulated N grid
sampleSizeSummary = bind_rows(
    # Bayesian power curve: proportion of simulated trials declaring benefit
    resultsNormalApprox |>
        group_by(pInt, cv, nTrialSize) |>
        summarise(power = mean(success, na.rm = TRUE), .groups = "drop") |>
        mutate(method = "Bayesian"),

    # Frequentist assurance: closed-form power averaged over the design prior
    resultsAssurance |>
        select(pInt, cv, nTrialSize, power = assurancePower) |>
        mutate(method = "Frequentist (averaged over design prior)"),

    # Conventional frequentist power at the assumed (fixed) effect size: independent of cv
    resultsFreq |>
        select(pInt, nTrialSize, power = freqPower) |>
        mutate(method = "Frequentist (point alternative)", cv = NA)
) |>
    group_by(method, pInt, cv) |>
    summarise(
        n80 = interp_n_for_power(pick(nTrialSize, power), 0.80),
        n90 = interp_n_for_power(pick(nTrialSize, power), 0.90),
        .groups = "drop"
    ) |>
    # p-infinity = prior probability the effect is beneficial, i.e. the power ceiling
    mutate(pInfinity = pnorm(1 / cv), .after = cv) |>
    arrange(pInt, method, cv)

write_csv(sampleSizeSummary, "results/sampleSizeSummary.csv")
