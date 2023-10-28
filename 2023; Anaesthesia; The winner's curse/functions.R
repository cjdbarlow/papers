# Run basic Bernoulli Simulations
fn.trial_run = function(con_er, int_er, n_group, n_trials) {
  con_e = rbinom(n_trials, n_group, con_er)
  int_e = rbinom(n_trials, n_group, int_er)
  
  data = data.frame(con_e = con_e,
                    int_e = int_e)
  list(data)
}


# Calculate odds ratio
fn.or = Vectorize(function(int_e, con_e, n){
  oi = int_e/(n - int_e)
  oc = con_e/(n - con_e)
  or = oi/oc
  or
})