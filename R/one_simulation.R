######################.
##### Estimation #####
######################.

if (cfg$sim_which=="estimation") {
  
  #' Run a single simulation
  #'
  #' @return A list with ...
  one_simulation <- function() {
    
    # Generate data
    x <- runif(L$n)
    y <- 10 + L$beta*x + rnorm(L$n)
    
    # Analyze data
    model <- lm(y~x)
    
    # Return results
    return(list(
      est = model$coefficients[[2]],
      se = vcov(model)[2,2]
    ))
    
  }
  
}
