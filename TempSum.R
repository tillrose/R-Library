
###* TempSum *###

## Input

# daily.temp:   Vector of daily mean temperature.
# date:         Associated vector of date as POSIX.
# base.temp:    Base temperature, all mean temperatures above this threshold value are included for the calculation of the temperature sum. Base temperature has to be equal or higher zero.
# start.date:   All mean temperatures from this date onwards (start.date is included) are included for the calculation of the temperature sum. Has to be POSIX.

## Additional Information

# NA-values are handled as base temperature and hence hold the preceding value


TempSum <- function(daily.temp, date, base.temp = 0, start.date) {
  
  ## Tests
  if(length(daily.temp) != length(date)) {stop("Vectors of daily temperature and date don't have the same length!")}
  if(!any(date == start.date)) {stop("Start date is not included in the date-vector!")}
  if(base.temp < 0) {stop("Base temperature is smaller than zero!")}
  
  ## Calculations
  dat <- data.frame(date, daily.temp)
  dat$daily.temp[dat$date < start.date] <- 0
  dat$daily.temp[is.na(dat$daily.temp)] <- 0
  dat <- cbind(dat$daily.temp, base.temp)
  temp.sum <- apply(X = dat, MARGIN = 1, FUN = max)
  temp.sum <- cumsum(na.exclude(temp.sum))
  return(temp.sum)
  
}
