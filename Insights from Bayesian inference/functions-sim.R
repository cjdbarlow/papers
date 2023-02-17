# Trial size calculation
## Uses power.prop.test co calculate trial *group* size
fn.trial_size = Vectorize(function(con_event_rate, int_event_rate, alpha, power){
    n = power.prop.test(n = NULL,
                        p1 = con_event_rate,
                        p2 = int_event_rate,
                        sig.level = alpha,
                        power = power)$n
    
    # Remember, power.prop.test calculates size of *one* group not whole trial
    # Round up to the nearest integer
    ceiling(n)
})


# Run a single simulated RCT
##Simulates a 2-group RCT at the trial level via Bernoulli simulations, and returns a contingency table*
# *But as a vector, for convenience
fn.trial_run = function(n.group, con.er, int.er, balance = 0.5){
    # Group numbers (if you want uneven group sizes, pass balance)
    n.trial = 2 * n.group
    n.con = n.trial * balance
    n.int = n.trial * balance
    
    if(n.con%%1 != 0 | n.int%%1 != 0){
        stop("Trial size and balance settings are leading to non-whole numbers in trial groups")
    }
    
    # Outcomes
    con.dead = rbinom(1, n.con, con.er)
    con.alive = n.con - con.dead
    int.dead = rbinom(1, n.int, int.er)
    int.alive = n.int - int.dead
    
    # Return contingency "table"
    c(con.alive, con.dead, int.alive, int.dead)
    
}

# Run a set of simulated RCTs
## By calling trial_run:
#  - An n.sim number of times
#  - Simulations are split (based on the prior) into those with either a true effect or no (false) effect
#  - Returns a length 1 list containing a dataframe where:
#   - Each row is a simulation
#   - Variables are the trial type (true effect or no effect) and each cell of a contingency table

fn.trial_sim = Vectorize(function(n.sim, prior, n.group, con.er, int.er, alpha = 0.05, balance = 0.5){
    # Determine number of trials that either have a true effect, or don't
    n.true = n.sim * prior
    n.false = n.sim - n.true
    
    if(n.true%%1 != 0 | n.false%%1 != 0){
        stop("Sim number and prior settings are leading to non-whole numbers in simulation groups")
    }
    
    # Run all the 'true' trials
    ## Pre-populate df
    df.true = data.frame(effect = rep(TRUE, times = n.true),
                         con.alive = integer(length = n.true),
                         con.dead = integer(length = n.true),
                         int.alive = integer(length = n.true),
                         int.dead = integer(length = n.true))
    
    for(i in 1:n.true){
        # True effect trials - difference between control and intervention event rates
        contingency = fn.trial_run(n.group, con.er, int.er)
        
        df.true[i,]$con.alive = contingency[1]
        df.true[i,]$con.dead = contingency[2]
        df.true[i,]$int.alive = contingency[3]
        df.true[i,]$int.dead = contingency[4]
    }
    rm(contingency)
    
    # Again, but with the false trials
    ## Pre-populate df
    df.false = data.frame(effect = rep(FALSE, times = n.false),
                          con.alive = integer(length = n.false),
                          con.dead = integer(length = n.false),
                          int.alive = integer(length = n.false),
                          int.dead = integer(length = n.false))
    
    for(i in 1:n.false){
        # No true effect trials - the control and intervention group event rates are both the control event rate
        contingency = fn.trial_run(n.group, con.er, con.er)
        
        df.false[i,]$con.alive = contingency[1]
        df.false[i,]$con.dead = contingency[2]
        df.false[i,]$int.alive = contingency[3]
        df.false[i,]$int.dead = contingency[4]
    }
    
    df = rbind(df.true, df.false)
    list(df)
})

# Calculate p-value via Fishers exact test
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

# Calculate Kass and Vaidyanathan BF10
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

# Calculate FPR from BF and p
fn.fpr = Vectorize(function(bf10, sig){
    if(sig) {
        fpr = 1 / (bf10 + 1)
        return(fpr)
    } else {
        return(NA_real_)
    }
})


# Calculate FNR from BF and p
fn.fnr = Vectorize(function(bf10, sig) {
    if(sig) {
        return(NA_real_)
    } else {
        bf01 = 1/bf10
        fnr = 1/(bf01 + 1)
        return(fnr)
    }
})