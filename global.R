library(MMWRweek)

getweek <- function (mydate) {
  if (weekdays(mydate) %in% c("Friday","Saturday")) {
     MMWRweek(mydate)[2] - 1 
  }
  else { 
    MMWRweek(mydate)[2] - 2 
  }
}


