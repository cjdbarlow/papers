# Function to generate beta prior from mean and CI
fn.binomial_prior = function(p1.prob, p1.val, p2.prob, p2.val, n = 10^6){
    # Calculate shape function from the probabilities via LearnBayes
    shape = LearnBayes::beta.select(quantile1 = list(p = p1.prob, x = p1.val),
                                    quantile2 = list(p = p2.prob, x = p2.val))
    alpha = shape[1]
    beta = shape[2]
    
    # Print the shape values to console so we can check as we go
    print(paste("A:", alpha))
    print(paste("B:", beta))
    
    # Calculate and return random deviates from the beta function
    ## We could also return the density for a given value of theta with dbeta, but this allows us to easily calculate differences between groups
    prior = rbeta(n, alpha, beta)
    prior
}


# Function to generate likelihood function
fn.binomial_likelihood = function(events, group.n, n = 10^6){
    like = rbinom(n = n, size = group.n, prob = events/group.n)
    like/group.n
}


# Function to calculate posterior from prior and likelihood
fn.binomial_posterior = function(p1.prob, p1.val, p2.prob, p2.val, events, n.group, n){
    # Calculate shape values as per fn.binomial_prior
    prior.shape = LearnBayes::beta.select(quantile1 = list(p = p1.prob, x = p1.val),
                                    quantile2 = list(p = p2.prob, x = p2.val))
    prior.alpha = prior.shape[1]
    prior.beta = prior.shape[2]
    
    # Update the prior with events to get shape values for the posterior
    post.alpha = prior.alpha + events
    post.beta = prior.beta + n.group - events
    
    # Calculate and return beta posterior
    post = rbeta(n, post.alpha, post.beta)
    post
}
