## Coursera Data Scientist Specialization
## R Programming
## Programming Assignment 1: Air Pollution Part 1
## By Joanna Widjaja (jo.widjaja@gmail.com)

pollutantmean <- function(directory, pollutant, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## Fix 1:332 calculation issue
  
  ## Save original directory and navigate to new directory
  orgDirectory <- getwd()
  setwd(paste(getwd(),directory,sep="/"))
# file <- list.files(path = paste(".", "specdata", sep = "/"))
  
  ## Create empty data frame
#  data <- data.frame(Date=as.Date(character()),
#                     sulfate=double(),
#                     nitrate=double(),
#                     ID=integer())
  sum <- numeric(length(id))
  count <- numeric(length(id))
##  means <- numeric(length(id))
  j=1
  
  ## Read all files in directory
  file <- list.files()
  
  ## Import file content as data frames and combine based on id size
  for(i in id){
    ##    good <- complete.cases(raw)
    ##    data <- rbind(data,raw[good,])    
    ##    data <- rbind(data,raw)
#    print(j)
    raw <- read.csv(file[i])
    sum[j] = sum(raw[[pollutant]], na.rm = TRUE)
    good <- complete.cases(raw[[pollutant]])
    count[j] = nrow(raw[good,])
##    means[j] = mean(raw[[pollutant]], na.rm = TRUE)
##    print("sum[j]")
##    print(sum[j])
##    print("count")
##    print(count[j])
    if(j<length(id)) j <- j+1
       
  }
  
  ## Set directory back. Return mean after removing NA.
  setwd(orgDirectory)
##  mean(means, na.rm = TRUE)
  means = sum(sum,na.rm = TRUE)/sum(count, na.rm = TRUE)
  means

}