# Libraries
library(BayesFactor) # For G&D
library(abtest) # For K&V


# Helper functions
## Log transformation
fn.log_trans = function(x) {
    log.trans = log(x / (1 - x))
    log.trans
}

## Inverse log transformation
fn.invlog_trans = function(x) {
    invlog.trans = exp(x) / (1 + exp(x))
    invlog.trans
}

## Better significant digits
# If x is between -1 and 1 then it will allow up to digits significant digits i.e. 0.000000xx rather than rounding to 0 
# Adapted from: https://stackoverflow.com/questions/43050903/round-to-significant-digits-only-with-decimal-portion-of-number-in-r
fn.signif_var <- Vectorize(function(x, digits = 2) {
    if(is.na(x)) {
        return(NA)
    } else if (x > 1 || x < -1) {
        signif(x, digits = digits)
    } else {
        as.integer(x) + signif(x - as.integer(x), digits)
    }
})


# Other functions
## Sample a value from each distribution and return the difference
fn.diff_sample = function(p1, p2){
    n1 = sample(p1, 1)
    n2 = sample(p2, 1)
    diff = n1 - n2
    diff
}


## Calculate p-value via Fishers exact test
fn.fisher = Vectorize(function(n_con, e_con, n_int, e_int){
    
    ## Calculate numbers for each of control/intervention and event/no event
    n_con_0 = n_con - e_con
    n_con_1 = e_con
    n_int_0 = n_int - e_int
    n_int_1 = e_int
    
    ## Put numbers into a matrix, as expected by fisher.test
    matrix = matrix(c(n_con_0, n_con_1,
                      n_int_0, n_int_1),
                    nrow = 2,
                    ncol = 2,
                    byrow  = TRUE)
    
    # Calculate, extract, and return p-value
    fisher = fisher.test(matrix)$p.value
    fisher
})


## Calculate FPR from BF and p
fn.fpr = Vectorize(function(bf10, sig){
    if(sig) {
        fpr = 1 / (bf10 + 1)
        return(fpr)
    } else {
        return(NA_real_)
    }
})


## Calculate FNR from BF and p
fn.fnr = Vectorize(function(bf10, sig) {
    if(sig) {
        return(NA_real_)
    } else {
        bf01 = 1/bf10
        fnr = 1/(bf01 + 1)
        return(fnr)
    }
})


## Calculate p(H0|d)
fn.pH0d = Vectorize(function(bf10, sig){
    # If significant then p(H0|d) == FPR, else the NPV
    if(sig){
        pH0d = fn.fpr(bf10, sig)
    } else {
        pH0d = 1 - fn.fnr(bf10, sig)
    }
    return(pH0d)
})


## Calculate p(H1|d)
fn.pH1d = Vectorize(function(bf10, sig){
    # If **not** significant then p(H1|d) == FNR, else the PPV
    if(!sig){
        pH1d = fn.fnr(bf10, sig)
    } else {
        pH1d = 1 - fn.fpr(bf10, sig)
    }
})


# Basic BF calculation (and associated) functions
## Calculate Gunel-Dickey BF10
fn.bf_gd = Vectorize(function(n_con, e_con, n_int, e_int){
    ## Calculate numbers for each of control/intervention and event/no event
    n_con_0 = n_con - e_con
    n_con_1 = e_con
    n_int_0 = n_int - e_int
    n_int_1 = e_int
    
    ## Put numbers into a matrix, as expected by contingencyTableBF
    matrix = matrix(c(n_con_0, n_con_1,
                      n_int_0, n_int_1),
                    nrow = 2,
                    ncol = 2,
                    byrow  = TRUE)
    
    # Calculate BF
    bf = BayesFactor::contingencyTableBF(matrix,
                                         sampleType = "indepMulti",
                                         fixedMargin = "rows",
                                         priorConcentration = 1)
    
    # Extract and return BF10
    bf10 = BayesFactor::extractBF(bf)$bf
    bf10
})

## Calculate Kass and Vaidyanathan BF10
fn.bf_kv = Vectorize(function(n_con, e_con, n_int, e_int){
    
    # Put numbers into list with nomenclature expected for ab_test
    data = list(y1 = e_con,
                n1 = n_con,
                y2 = e_int,
                n2 = n_int)
    
    # Calculate BF
    bf = abtest::ab_test(data = data,
                         prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1))
    
    # Extract and return BF10
    bf = bf[8]$bf
    bf10 = bf[[1]]
    bf10
})

## Calculate Selke MLR
fn.mlr_selke = Vectorize(function(p){
    # Cap p-value at 0.368
    p = ifelse(p > 0.368, 0.368, p)
    
    mlr = -1/(exp(1) * p * log(p))
    mlr
})

## Generate (uninformed by default) beta prior for conjugate methods
fn.conj_beta_prior = Vectorize(function(n, shp.1 = c(1, 1), shp.2 = c(1, 1)){
    
    # Generate beta priors for control and intervention groups
    prior.con = rbeta(n, shape1 = shp.1[1], shape2 = shp.1[2])
    prior.int = rbeta(n, shape1 = shp.2[1], shape2 = shp.2[2])
    
    # Calculate differences between control and intervention priors
    ## Pre-create vector
    prior.diff = vector(length = n)
    
    ## Fill vector
    for(i in 1:length(prior.diff)) {
        prior.diff[i] = fn.diff_sample(prior.con, prior.int)
    }
    
    ## Return as list
    list(prior.diff)
})

## Generate shape values for weakly informed beta prior for conjugate methods
fn.conj_beta_shp = Vectorize(function(cer, arr){
    # Calculates the shape values for a weakly informed beta prior...
    # ...where 90% of the density will be within the cer +/- arr
    
    beta.shape = LearnBayes::beta.select(list(x = cer - arr, p = 0.05),
                                         list(x = cer + arr, p = 0.95))
    
    list(beta.shape)
    
})

## Generates beta (uninformed by default) posteriors
fn.conj_beta_post = Vectorize(function(n, e_con, n_con, shp.1 = c(1, 1), e_int, n_int, shp.2 = c(1, 1)){
    # Generate beta posteriors based on same shape values as the prior, and the results of the study
    post.con = rbeta(n, shape1 = e_con + shp.1[1], shape2 = n_con - e_con + shp.1[2])
    post.int = rbeta(n, shape1 = e_int + shp.2[2], shape2 = n_int - e_int + shp.2[2])
    
    # Calculate posterior differences and return
    post.diff = vector(length = n)
    
    for(i in 1:length(post.diff)) {
        post.diff[i] = fn.diff_sample(post.con, post.int)
    }
    
    list(post.diff)
})

## Calculate Savage-Dickey BF at a given minimum clinically important difference
## NB: Currently the mcid values aren't exported from the function, so the mcid argument has currently no purpose
fn.bf_conj_sd = Vectorize(function(data.prior, data.posterior, mcid = 0.05, null = 0, one = 1) {
    
    # Internal functions that plot prior and posterior density curves
    ### NB: We can plot these functions with  plot(pri.density.fn, xlim = c(-1, 1))
    pri.density.fn = data.prior %>%
        density() %>%
        approxfun()
    
    post.density.fn = data.posterior %>%
        density() %>%
        approxfun()
    
    # Correct the prior and posterior functions if they return an NA result
    # (i.e. if the density function is unspecified at that level)
    # 0.01 chosen to reduce very high BFs
    post.null = post.density.fn(null)
    post.null = ifelse(is.na(post.null), 0.01, post.null)
    
    pri.null = pri.density.fn(null)
    pri.null = ifelse(is.na(pri.null), 0.01, pri.null)
    
    # Calculate and return BF10
    bf.10 = pri.null/post.null
    bf.10
    
})

# Flag to state whether the density function returns NA at that point
fn.bf_density_null = Vectorize(function(data, null){
    density.fn = data %>%
        density() %>%
        approxfun()
    
    dens.null = density.fn(null)
    
    is.null = ifelse(is.na(dens.null), TRUE, FALSE)
    is.null
})
