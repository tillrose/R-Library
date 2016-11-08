
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
  a <- round(coef(m)[1], digits = digits_parameter)
  b <- round(abs(coef(m)[2]), digits = digits_parameter)
  c <- round(coef(m)[3:(3 + (length.levels-2))], digits = digits_parameter)
  if(isTRUE(interaction)) {d <- round(coef(m)[(3 + (length.levels-1)):length(coef(m))], digits = digits_parameter)} else
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
  
  return(parameters)
  
}