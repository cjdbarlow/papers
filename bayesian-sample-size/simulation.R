# Setup ----
## Libraries
library(tidyverse)
library(arrow)
library(brms)
library(posterior)
library(parallel)

source("functions.R")

## Simulation Setup
seed = 123
nSims = 2000

nTrialSize = c(
    seq(100, 900, by = 100),
    seq(1000, 19000, by = 1000),
    seq(20000, 100000, by = 5000),
    seq(110000, 300000, by = 10000)
)

### Event rates
pCon = c(0.5)
pInt = c(0.4, 0.45, 0.48, 0.49)

## Don't simulate when the power is already high
nTrialSizeCap = tribble(
    ~pInt , ~nTrialSizeCap ,
    0.4   ,          20000 ,
    0.45  ,          50000 ,
    0.48  ,         300000 ,
    0.49  ,         300000
)

### Design Priors
designConSigma = 0.1 # beta0 sigma
cv = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0, 1.19, 1.5, 2.0)

### Analysis Priors
analysisIntMu = 0.0
analysisIntSigma = 0.5

### Study setup
altHypothesis = "one.sided"

sigThresh = 0.025
gamma = switch(
    # Bayesian decision threshold should match the frequentist one for a fair comparison, so set this automatically
    altHypothesis,
    "one.sided" = 1 - sigThresh,
    "two.sided" = 1 - sigThresh / 2,
    stop("wat")
)

# Calculate Simulation Settings ----
simSettings = expand_grid(pCon, pInt) |>
    # Calculate some basic properties
    mutate(
        riskDiff = pCon - pInt,
        conOdds = pCon / (1 - pCon),
        intOdds = pInt / (1 - pInt),
        OR = intOdds / conOdds,
        logOR = log(OR)
    ) |>
    # Don't simulate when the power is already high
    left_join(nTrialSizeCap, by = join_by(pInt)) |>
    expand_grid(cv) |>
    # Design prior parameters (mu duplicates columns above, kept explicit for clarity)
    mutate(
        # Design prior parameters
        designBeta0Mu = qlogis(pCon), # logit of the control rate
        designBeta0Sigma = designConSigma,
        designBeta1Mu = logOR, # design prior mean effect
        designBeta1Sigma = cv * abs(designBeta1Mu),
        # Analysis prior parameters
        analysisBeta1Mu = analysisIntMu,
        analysisBeta1Sigma = analysisIntSigma
    )

# Generate Simulated Data ----
simData = simSettings |>
    select(
        pInt,
        cv,
        nTrialSizeCap,
        designBeta0Mu,
        designBeta0Sigma,
        designBeta1Mu,
        designBeta1Sigma,
    ) |>
    # One row for each simulation
    expand_grid(nTrialSize, simNo = 1:nSims) |>
    # Filter out trial sizes above the cap
    filter(nTrialSize <= nTrialSizeCap) |>
    # fmt: skip
    mutate(
        nArm = nTrialSize / 2,

        # Assurance step: Determine the 'true' effect for each simulated trial, based on the design prior
        # n() works because we are passing an equal length vector to each argument...
        # ...returning a column of the same length
        trueBeta0 = rnorm(n(), mean = designBeta0Mu, sd = designBeta0Sigma),
        trueBeta1 = rnorm(n(), mean = designBeta1Mu, sd = designBeta1Sigma),

        # Realised event rates for each simulated trial
        pConReal = plogis(trueBeta0),
        pIntReal = plogis(trueBeta0 + trueBeta1),

        # Event counts for each simulated trial
        eventsCon = rbinom(n(), size = nArm, prob = pConReal),
        eventsInt = rbinom(n(), size = nArm, prob = pIntReal),
        nonEventsCon = nArm - eventsCon,
        nonEventsInt = nArm - eventsInt)

# Analyse Simulated Data ----
# Compute power by differene methods
# 1. Conventional Frequentist Power
# Closed-form one-sided power at the assumed (fixed) effect size
# Independent of the design prior, so one curve per effect size
resultsFreq = expand_grid(pCon, pInt, nTrialSize) |>
    mutate(
        nArm = nTrialSize / 2,
        freqPower = freq_power_onesided(
            n_arm = nArm,
            p_control = pCon,
            p_intervention = pInt,
            gamma = gamma
        )
    )

## 2. Compute Assurance
# One-sided frequentist power averaged over the design prior
# Only beneficial draws (beta1 < 0) contribute; harmful draws return 0
resultsAssurance = simSettings |>
    select(pCon, pInt, cv, designBeta1Mu, designBeta1Sigma) |>
    expand_grid(nTrialSize) |>
    rowwise() |>
    mutate(
        nArm = nTrialSize / 2,
        #(Custom function to replicate )
        assurancePower = compute_assurance(
            n_arm = nArm,
            p_control = pCon,
            dp_beta1_mu = designBeta1Mu,
            dp_beta1_sd = designBeta1Sigma,
            gamma = gamma
        )
    ) |>
    ungroup()

## 3a. Normal approximation by MLE using a GLM
resultsNormalApprox = simData |>
    rowwise() |>
    mutate(
        ## Fit a logistic regression model (keeping only the coefficients)...
        glmCoefs = list(
            glm(
                cbind(events, nonEvents) ~ group,
                data = tibble(
                    group = factor(c("control", "intervention"), levels = c("control", "intervention")),
                    events = c(eventsCon, eventsInt),
                    nonEvents = c(nonEventsCon, nonEventsInt)
                ),
                family = binomial()
            ) |>
                summary() |>
                coef()
        ),
        ## Apply a normal-normal conjugate update, and return TRUE if P(beta1 < 0 | data) > gamma
        success = normal_approx_decision(
            coefs = glmCoefs,
            ap_beta1_mu = analysisIntMu,
            ap_beta1_sigma = analysisIntSigma,
            gamma = gamma
        )
    ) |>
    ungroup() |>
    ## Closed-form Wald shortcut to prove my point to David
    ## (essentially matches the GLM coefficients to ~1e-6, runs bloody fast, but fails on zero cells)
    mutate(
        # MLE log odds ratio and its Wald SE
        beta1MLE = log(eventsInt) - log(nonEventsInt) - log(eventsCon) + log(nonEventsCon),
        beta1WaldSe = sqrt(
            1 / eventsCon + 1 / nonEventsCon + 1 / eventsInt + 1 / nonEventsInt
        ),

        # Conjugate normal-normal update
        # Combine the data (MLE, SE) with the analysis prior and produce a (precision-weighted) posterior
        precisionPrior = 1 / analysisIntSigma^2,
        precisionData = 1 / beta1WaldSe^2,
        precisionPost = precisionPrior + precisionData,
        muPost = (analysisIntMu * precisionPrior + beta1MLE * precisionData) / precisionPost,
        sigmaPost = sqrt(1 / precisionPost),

        # Bayesian decision: P(beta1 < 0 | data) > gamma == declare benefit
        postProb = pnorm(0, mean = muPost, sd = sigmaPost),
        successWald = postProb > gamma,

        # Frequentist decision: one-sided Wald test on the log-OR
        freqPValue = pnorm(beta1MLE / beta1WaldSe),
        freqSuccess = freqPValue < (1 - gamma)
    )

## 3b. MCMC validation of the normal approximation method, for the supplmenet
mcmcNSims = 1000
mcmcCvs = c(0.1, 0.5, 1.19, 1.5)

### brms settings
nCores = max(1, detectCores() - 4)
chains = 4
warmup = 1000
iter = 2000
delta = 0 # log-OR threshold for benefit: success = P(beta1 < delta) > gamma

### Analysis prior (skeptical), reusing the normal-approx prior parameters
mcmcAnalysisPriors = list(list(
    beta0 = c(mu = 0, sigma = analysisIntSigma),
    beta1 = c(mu = analysisIntMu, sigma = analysisIntSigma)
))

verify_priors(mcmcAnalysisPriors[[1]], "MCMC analysis prior")

### Reshape the chosen sims to the long, per-arm form brms expects, then nest one tibble of all sims per scenario (pInt x cv x trial size)
mcmcTrials = simData |>
    filter(
        cv %in% mcmcCvs,
        simNo <= mcmcNSims
    ) |>
    select(
        pInt,
        cv,
        nArm,
        sim_id = simNo,
        true_beta0 = trueBeta0,
        true_beta1 = trueBeta1,
        control = eventsCon,
        intervention = eventsInt
    ) |>
    # trials count for brms (= per-arm size); kept separate so nArm survives group_nest()
    mutate(n = nArm) |>
    pivot_longer(
        c(control, intervention),
        names_to = "group",
        values_to = "events"
    ) |>
    mutate(group = factor(group, levels = c("control", "intervention"))) |>
    group_nest(pInt, cv, nArm) |>
    mutate(nSims = mcmcNSims, gamma = gamma, delta = delta)

### Compile the Stan model once; fit_scenario reuses it via update()
mcmcTemplate = brm(
    events | trials(n) ~ group,
    family = binomial(),
    data = mcmcTrials$data[[1]] |>
        filter(sim_id == 1),
    prior = c(
        prior_string(
            sprintf(
                "normal(%g, %g)",
                mcmcAnalysisPriors[[1]]$beta0["mu"],
                mcmcAnalysisPriors[[1]]$beta0["sigma"]
            ),
            class = "Intercept"
        ),
        prior_string(
            sprintf(
                "normal(%g, %g)",
                mcmcAnalysisPriors[[1]]$beta1["mu"],
                mcmcAnalysisPriors[[1]]$beta1["sigma"]
            ),
            class = "b",
            coef = "groupintervention"
        )
    ),
    chains = chains,
    iter = iter,
    warmup = warmup,
    refresh = 0,
    silent = 2,
    cores = 1
)

### Fit each scenario, save per scenario, skip if already done
t_total = Sys.time()
for (s in seq_len(nrow(mcmcTrials))) {
    sum_file = file.path(
        "data/mcmc/summaries",
        sprintf("scenario_%03d.parquet", s)
    )
    post_file = file.path(
        "data/mcmc/posteriors",
        sprintf("scenario_%03d.parquet", s)
    )

    if (file.exists(sum_file) && file.exists(post_file)) {
        next
    }

    t0 = Sys.time()
    message(sprintf(
        "MCMC scenario %d/%d (nArm = %d)...",
        s,
        nrow(mcmcTrials),
        mcmcTrials$nArm[s]
    ))

    out = fit_scenario(mcmcTrials[s, ], mcmcTemplate, cores = nCores)

    # Tag the summary with scenario identity (pInt, cv) for later grouping
    out$summary = out$summary |>
        mutate(pInt = mcmcTrials$pInt[s], cv = mcmcTrials$cv[s])

    write_parquet(out$summary, sum_file)
    write_parquet(out$posterior, post_file)

    elapsed = round(difftime(Sys.time(), t0, units = "mins"), 2)
    message(sprintf(
        "  done in %.2f min   Bayesian power = %.3f",
        elapsed,
        mean(out$summary$success)
    ))
}
message(sprintf(
    "\nTotal MCMC time: %.2f min",
    round(difftime(Sys.time(), t_total, units = "mins"), 2)
))

# Results ----
## Bayesian power per scenario and per-arm sample size
summaries = open_dataset("data/mcmc/summaries") |>
    collect()

results = summaries |>
    group_by(pInt, cv, nArm) |>
    summarise(
        nTotal = first(nArm) * 2,
        bayesianPower = mean(success),
        .groups = "drop"
    )

print(results)

# Save outputs ----
write_parquet(simSettings, "data/simSettings.parquet")
write_parquet(simData, "data/simData.parquet")

saveRDS(resultsNormalApprox, "data/resultsNormalApprox.rds")
write_parquet(resultsAssurance, "data/resultsAssurance.parquet")
write_parquet(resultsFreq, "data/resultsFreq.parquet")
