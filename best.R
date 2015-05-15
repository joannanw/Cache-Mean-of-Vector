## R Programming
## Programming Assignment 3: Hospital Quality
## Finding the Best Hospital in a State
## By Joanna Widjaja (Jan 25, 2015)

best <- function(state, outcome)
{
  ## state takes the 2-character abbreviated name of a state from 
  ## outcome is the kind of illness desired such as 'heart attack', 'pneumonia', etc
  
  ## Read outcome data
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  raw <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
  
  # Validate state and outcome
  valid_states <- unique(raw$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!is.element(state, valid_states)) stop("invalid state")
  if(!is.element(outcome, valid_outcomes)) stop("invalid outcome")
  
  # Set column index
  s <- strsplit(outcome, " ")[[1]]
  colOutcome <- paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=".")
  
  # Cut dataset to those of interest
  colOutcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",colOutcome, sep = "")
  idx <- grep(colOutcome, names(raw))[1]
  idx <- c(2,7,idx)
  data <- raw[,idx]
  names(data)[3] <- "Rate"
  data$Rate <- suppressWarnings(as.numeric(data$Rate))
  data2 <- data[data$State == state & !is.na(data$Rate),]
  sort(data2[data2$Rate == min(data2$Rate),1])
  
}