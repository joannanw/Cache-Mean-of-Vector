## R Programming
## Programming Assignment 3: Hospital Quality
## Ranking hospitals by outcome in State
## By Joanna Widjaja (Jan 25, 2015)



rankhospital <- function(state, outcome, num = "best") 
{
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if (num == "best") {return (best(state, outcome))}
  
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
  
  
  #return (sort(data2[data2$Rate == max(data2$Rate),1]))
  
  data2 <- data2[order(data2[,3], as.character(data2[,1])),]
  
  if(num == "worst") (return (data2[dim(data2)[1],1]))
  
  data2[num,1]
  
}