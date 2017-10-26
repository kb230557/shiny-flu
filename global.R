library(MMWRweek)

getweek <- function (testdate) {
  if (weekdays(testdate) == "Friday") {
     MMWRweek(testdate)[2] - 1 
  }
  else { 
    MMWRweek(testdate)[2] - 2 
  }
}
