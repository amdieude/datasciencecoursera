## rankall.R

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",
                   colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = sort(unique(data[,7]))
  #if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                   "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## For each state, find the hospital of the given rank
  hospital<-character(0)
  
  for (i in seq_along(validState)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    data_state <- data[data$State==validState[i],]
    
    # order data by outcome
    sorted_data_state <- data_state[order(as.numeric(data_state[[colName]]),
                        data_state[["Hospital.Name"]],decreasing=FALSE,na.last=NA),]
    
    #handle num input
    this_num = num
    if (this_num=="best") this_num = 1
    if (this_num=='worst') this_num = nrow(sorted_data_state)
    
    hospital[i] <- sorted_data_state[this_num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validState,row.names=validState)
}