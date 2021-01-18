outcomes <- read.csv("C:/Users/kora/Downloads/Documents/outcome-of-care-measures.csv",
                    colClasses = "character" )
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])
head(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
head(outcome[,11])

hosp_sort <- function(state,outcome){
  #setting NA to be 0
  
  outcomes[outcomes == "Not Available"] <- 0
  index <- c(grep("^Hospital.*Death*", names(outcomes)))
  
  #this creates a dataframe for the mortality rates columns
  mortality_rates <- outcomes[,c(2,7,index)]
  
  #renaming the titles of the mortality rates in the new list
  names(mortality_rates)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  
  #Making rates numeric
  mortality_rates[,3:5] <- apply(mortality_rates[,3:5],2,as.numeric)
  
  #Getting the morrality rates for the specified state in the function call. 
  mortality_rates[mortality_rates == 0] <- NA
  selected_state <- mortality_rates[mortality_rates$State == state,]
  
  #This applies they dplyr package method arrange to order the outcomes of the selected states. 
  #first it sorts by outcomeS by the selected state code and then according to the hospital name.
  order_selected <- arrange(selected_state, selected_state[,outcome], Hospital.Name, na.last=TRUE)
  
  order_selected <- order_selected[complete.cases(order_selected[,outcome]),]
  return(order_selected)
  #na.omit(selected_state[order(c(selected_state[,outcome]), na.last = TRUE),])
}
best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("C:/Users/kora/Downloads/Documents/outcome-of-care-measures.csv",
                      colClasses = "character" )
  ## Check that state and outcome are valid
  if (!state %in% unique(outcomes$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  order_selected <- hosp_sort(state,outcome)
  
  ## rate
  return(order_selected[1,1])
  
}
worst <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  #This function returns the worst for the given outcome and state. 
  
  return(order_selected[nrow(order_selected),1])
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("C:/Users/kora/Downloads/Documents/outcome-of-care-measures.csv",
                       colClasses = "character" )
  ## Check that state and outcome are valid -> This check is achieved in the best and worst functions above. 
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}

