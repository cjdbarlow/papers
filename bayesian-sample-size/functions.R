# Normal approximation decision function
# Takes the logistic GLM, applies a normal-normal conjugate update, and returns true if posterior probability above deicison threshold
normal_approx_decision = function(coefs, ap_beta1_mu, ap_beta1_sigma, gamma) {
    beta1MLE = coefs["groupintervention", "Estimate"]
    beta1WaldSe = coefs["groupintervention", "Std. Error"]

    prec_prior = 1 / ap_beta1_sigma^2
    prec_lhood = 1 / beta1WaldSe^2
    prec_post = prec_prior + prec_lhood

    mu_post = (ap_beta1_mu * prec_prior + beta1MLE * prec_lhood) / prec_post
    sigma_post = sqrt(1 / prec_post)

    post_prob = pnorm(0, mean = mu_post, sd = sigma_post)

    return(post_prob > gamma)
}

# Assurance: one-sided frequentist power averaged over the design prior
# Closed-form power on the risk-difference scale, with beta0 fixed at its prior mean (not drawn)
# Draws with beta1 >= 0 (harmful or null) cannot meet a beneficial decision rule, so contribute zero power
compute_assurance = function(n_arm, p_control, dp_beta1_mu, dp_beta1_sd, gamma, n_draws = 5000) {
    true_beta1 = rnorm(n_draws, dp_beta1_mu, dp_beta1_sd)
    p_i = plogis(qlogis(p_control) + true_beta1)

    # One-sided power toward detecting benefit (p_i < p_control)
    # z is positive when p_i < p_control (beneficial)
    se = sqrt(p_control * (1 - p_control) / n_arm + p_i * (1 - p_i) / n_arm)
    z = (p_control - p_i) / se

    powers = if_else(
        # Guard against harmful/null draws and degenerate event rates
        true_beta1 >= 0 | abs(p_i - p_control) < 1e-6 | p_i <= 0 | p_i >= 1,
        0,
        pnorm(z - qnorm(gamma))
    )

    mean(powers, na.rm = TRUE)
}

# Conventional frequentist power: closed-form one-sided power at the assumed (fixed) effect size
# Uses the same normal approximation on the risk-difference scale as compute_assurance
# Vectorised over n_arm (and p_intervention)
freq_power_onesided = function(n_arm, p_control, p_intervention, gamma) {
    se = sqrt(
        p_control * (1 - p_control) / n_arm + p_intervention * (1 - p_intervention) / n_arm
    )
    z = (p_control - p_intervention) / se
    pnorm(z - qnorm(gamma))
}

# Total sample size achieving a target power, interpolated linearly between the bracketing grid points
# Takes a power curve (one row per nTrialSize, with a power column)
# Returns NA if the target is never reached within the grid
interp_n_for_power = function(data, target) {
    below = data |>
        filter(power < target) |>
        slice_max(nTrialSize, n = 1)
    above = data |>
        filter(power >= target) |>
        slice_min(nTrialSize, n = 1)
    if (nrow(above) == 0) {
        return(NA_real_)
    }
    if (nrow(below) == 0) {
        return(above$nTrialSize)
    }
    below$nTrialSize +
        (above$nTrialSize - below$nTrialSize) *
            (target - below$power) /
            (above$power - below$power)
}

# Verify prior ----
verify_priors = function(priors, label) {
    b0 = priors$beta0
    b1 = priors$beta1

    # 95% CI on the log scale for each coefficient
    b0_lo = qnorm(0.025, b0["mu"], b0["sigma"])
    b0_hi = qnorm(0.975, b0["mu"], b0["sigma"])
    b1_lo = qnorm(0.025, b1["mu"], b1["sigma"])
    b1_hi = qnorm(0.975, b1["mu"], b1["sigma"])

    cat(sprintf("── %s ──────────────────────────────────\n", label))

    cat(sprintf("Beta0 ~ N(%.3f, %.3f)  [baseline]\n", b0["mu"], b0["sigma"]))
    cat(sprintf(
        "  log-odds:    median %.3f   95%% CI %.3f to %.3f\n\n",
        b0["mu"],
        b0_lo,
        b0_hi
    ))

    cat(sprintf(
        "Beta1 ~ N(%.3f, %.3f)  [treatment effect]\n",
        b1["mu"],
        b1["sigma"]
    ))
    cat(sprintf(
        "  log-OR: median %.3f   95%% CI %.3f to %.3f\n",
        b1["mu"],
        b1_lo,
        b1_hi
    ))
    cat(sprintf(
        "  OR:     median %.3f   95%% CI %.3f to %.3f\n\n",
        exp(b1["mu"]),
        exp(b1_lo),
        exp(b1_hi)
    ))
}


# Fit all sims for one scenario (one row of trialSim) ----
# Returns a list of two tibbles:
#   $summary    — one row per sim, with scenario metadata + p_success
#   $posterior  — one row per draw, joinable on (nArm, sim_id)
fit_scenario = function(scenario, fit_template, cores = 1L) {
    # Fit one simulated trial: re-uses the pre-compiled brms model via update().
    # Returns a list with two tibbles: posterior draws and a one-row summary.
    fit_one_sim = function(sim_data, delta, gamma) {
        fit = update(
            fit_template,
            newdata = sim_data,
            refresh = 0,
            silent = 2,
            recompile = FALSE
        )

        draws = as_draws_df(fit) |>
            as_tibble()
        p_success = mean(draws$b_groupintervention < delta)

        list(
            summary = tibble(
                p_success = p_success,
                success = p_success > gamma
            ),
            posterior = draws |>
                transmute(
                    .draw,
                    b_Intercept,
                    b_groupintervention
                )
        )
    }

    # Split scenario data into one tibble per simulation
    sim_data_list = scenario$data[[1]] |>
        group_split(sim_id)

    # Fit each sim, one forked worker per core.
    # delta/gamma are forwarded by name to fit_one_sim; mc.set.seed (the default)
    # gives each fork its own RNG stream.
    results = parallel::mclapply(
        sim_data_list,
        fit_one_sim,
        delta = scenario$delta,
        gamma = scenario$gamma,
        mc.cores = cores
    )

    # Pull truths from the first row of each sim's data
    truths = map_dfr(sim_data_list, \(d) {
        tibble(
            sim_id = d$sim_id[1],
            true_beta0 = d$true_beta0[1],
            true_beta1 = d$true_beta1[1]
        )
    })

    # Assemble summary table (one row per sim)
    summary_tbl = map_dfr(seq_along(results), \(i) {
        results[[i]]$summary |>
            mutate(sim_id = sim_data_list[[i]]$sim_id[1])
    }) |>
        left_join(truths, by = "sim_id") |>
        mutate(
            nArm = scenario$nArm,
            nSims = scenario$nSims,
            gamma = scenario$gamma,
            delta = scenario$delta
        ) |>
        relocate(nArm, sim_id, true_beta0, true_beta1, p_success, success)

    # Assemble posterior table (one row per draw)
    posterior_tbl = map_dfr(seq_along(results), \(i) {
        results[[i]]$posterior |>
            mutate(
                sim_id = sim_data_list[[i]]$sim_id[1],
                nArm = scenario$nArm
            )
    }) |>
        relocate(nArm, sim_id, .draw)

    list(summary = summary_tbl, posterior = posterior_tbl)
}
