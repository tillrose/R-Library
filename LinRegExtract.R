
### LinRegParameters ###
########################

# Returns a data frame with all factor levels and their parameters (slope, intercept) of an ANCOVA conducted with a linear model (lm).

## Input ##

# mod: A linear mode (lm)
# digits_parameter: How many digits should the parameter-values have?


LinRegParameters <- function(mod, digits_parameter = 2) {
  
  ## clump together all parameters
  m <- mod
  length.levels <- length(unlist(mod$xlevels))
  interaction <- (length(m$coefficients) - 2) >= length.levels
  a <- coef(m)[1]
  b <- abs(coef(m)[2])
  c <- coef(m)[3:(3 + (length.levels-2))]
  if(isTRUE(interaction)) {d <- coef(m)[(3 + (length.levels-1)):length(coef(m))]} else
  {d <- 0}

  l <- list(a = a, b = b, c = c, d = d)
  
  ## merge levels and their parameters
  parameters <- data.frame(level = rep(NA, times = length.levels), intercept = rep(NA, times = length.levels), slope = rep(NA, times = length.levels))
  
  for(i in 1:length.levels) {
    
    parameters[i, "level"] <- unname(unlist(mod$xlevels)[i])
    
    parameters[i, "intercept"] <- ifelse(i==1,
                                         l$a,
                                         l$a + unname(l$c[i-1]))
    
    parameters[i, "slope"] <- ifelse(i==1,
                                     l$b,
                                     l$b + unname(l$d[i-1]))
    
  }
  
  parameters$intercept <- signif(parameters$intercept, digits = digits_parameter)
  parameters$slope <- signif(parameters$slope, digits = digits_parameter)
  
  return(parameters)
  
}

