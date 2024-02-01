# Weighted calculations ----
## Calculated weighted mean, standard error, and 95% confidence interval for matched datasets for a given grouping variable of interest
fn.wtd_score = function(df, grouping_var) {
    df %>%
        group_by({{grouping_var}}, ecmo_episode) %>%
        summarise(n = n(),
                  sum.prop = sum(prop.score),
                  wtd_mean.prop = Hmisc::wtd.mean(prop.score, weights = match.weights),
                  wtd_stdev.prop = sqrt(Hmisc::wtd.var(prop.score, weights = match.weights))) %>%
        mutate(wtd_se.prop = wtd_stdev.prop / sqrt(n),
               ci_l.prop = wtd_mean.prop - qt(1 - (0.05 / 2), n - 1) * wtd_se.prop,
               ci_h.prop = wtd_mean.prop + qt(1 - (0.05 / 2), n - 1) * wtd_se.prop)
}


# Fragility index for regression results
fn.fragility = function(data.list, dv, ref.lvl, sig = 0.05, bigout = FALSE){
    # Set variables that are consistent throughout the model
    ## Get number of levels in variable of interest

    var.lvls = data.list[[1]][[dv]] %>%
        levels()
    
    var.lvls_dep = setdiff(var.lvls, ref.lvl)

    if(ref.lvl %in% var.lvls == FALSE) stop("Reference level not in dependent variable")

    # Relevel the outcome variable
    data.list = data.list %>%
        map(mutate, !!as.name(dv) := relevel(!!as.name(dv), ref.lvl))

    # Create dataframe for outputs
    output = data.frame(level = character(),
                        index = integer(),
                        p.value = numeric())
    
    if(bigout) {
        output = output %>%
        mutate(models = list(),
               data = list())
    }
    
    # Loop through each level of the DV to calculate a fragility index for each level
    for(i in 1:length(var.lvls_dep)){
        # Set variables for the current outcome level of interest
        ## Starting conditions
        index = 0

        ## Duplicate the data.frame to work on
        data = data.list
        
        ## Get the index of patients who did not receive ECMO AND have the given outcome level of interest
        i.ecmo = which(data.list[[1]]$ecmo_episode == FALSE &
                       data.list[[1]][[dv]] == var.lvls_dep[i])
        
        ## Calculate the p-value of the base model
        m1 = data %>%
            map(~ glm(ecmo_episode ~ .[[dv]],
                      data = .,
                      family = "binomial",
                      weights =.$match.weights,
                      model = TRUE,
                      trace = FALSE))
        
        p.val = m1  %>%
            mice::pool() %>%
            broom::tidy() %>%
            # Take the row number of the outcome level (intercept is first, hence the +1)
            filter(row_number() == i + 1) %>%
            pull(p.value)
        
        # Calculate the fragility index for a given level by:
        # - Randomly change one of the DV at the current outcome level (i) to the ref.lvl
        # - Repeat the logistic regression model
        # - Repeat this process until the p-value for the reference level for that model is no longer significant
        
        while(p.val < sig) {
            # Pluck a random patient from the ECMO list
            pt = sample(i.ecmo, 1)
            i.ecmo = setdiff(i.ecmo, pt)
            
            # Inefficiently change that patient to receive ECMO
            data = data %>%
                map(mutate, ecmo_episode = ifelse(row_number() == pt, TRUE, ecmo_episode))
            
            # Regression model
            model = data %>%
                map(~ glm(ecmo_episode ~ .[[dv]],
                          data = .,
                          family = "binomial",
                          weights =.$match.weights,
                          model = TRUE))

            # Extract p-value from pooled and tidy'd model
            p.val = model  %>%
                mice::pool() %>%
                broom::tidy() %>%
                filter(row_number() == i + 1) %>%
                pull(p.value)
            
            # Add to the index
            index = index + 1
            
            # Proof of life output
            cat(paste("\n\n",
                      "P-value:", p.val, "\n",
                      "Index:", index, "\n",
                      "Patient: ", pt, "\n",
                      "Patients remaining (at given IV level):", length(i.ecmo),
                      "\n\n"))
            
        }
        # Bind results
        output[i, "level"] = var.lvls_dep[i]
        output[i, "index"] = index
        output[i, "p.value"] = p.val
        
        if(bigout){
            output[i, "models"][[1]] = list(models = model)
            output[i, "data"][[1]] = list(raw = data)
        }
        
        
    }
    # Return the output dataframe
    return(output)
}