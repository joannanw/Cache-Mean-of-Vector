## R Programming
## Programming Assignment 3: Hospital Quality
## Plot 30-day Mortality Rates From Heart Attack
## By Joanna Widjaja (Jan 25, 2015)

# Read the csv file. Dim: 4706x46.
outcome <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
# Make histogram. There may be warning about adding NAs; that's okay.
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11], xlab="No. of Deaths")