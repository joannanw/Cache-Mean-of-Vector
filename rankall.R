## R Programming
## Programming Assignment 3: Hospital Quality
## Ranking hospitals in all states
## By Joanna Widjaja (Jan 25, 2015)


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  raw <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
  valid_states <- unique(raw$State)
  result <- matrix(0, nrow = length(valid_states), ncol = 2)
  result <- as.data.frame(result)
  names(result) <- c("hospital", "state")
  
  
  for (i in 1:length(valid_states))
  {
    result[i,1] <- rankhospital(valid_states[i],outcome, num)
    result[i,2] <- valid_states[i]
  }
    
  result[order(result[,2]),]
  
}