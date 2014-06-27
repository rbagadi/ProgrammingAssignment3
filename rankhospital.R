rankhospital <- function(state, outcome, num = "best") {
  
  valid_Outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome-of-care-measures input data file
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  
  if (!(state %in% outcome_data$State)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% valid_Outcomes)) {
    stop("invalid outcome")
  }
  
  state_df <- subset(outcome_data, State == state, select = c("hospital" = 2, "heart attack"= 11, 
                                                              "heart failure" = 17,  "pneumonia" = 23) )
  
  state_df[, 2] <- as.numeric(state_df[, 2])
  state_df[, 3] <- as.numeric(state_df[, 3])
  state_df[, 4] <- as.numeric(state_df[, 4])
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  if (outcome == "heart attack") {
    order_df <- state_df[order(state_df[, 2], state_df[, 1], na.last = NA), ]
    }
  else if (outcome == "heart failure") {
    order_df <- state_df[order(state_df[, 3], state_df[, 1], na.last = NA), ]
    }
  else {
    order_df <- state_df[order(state_df[4], state_df[, 1], na.last = NA), ]
    }
    
  if (num == "best") {
    Best <- as.character(order_df[1, 1])  
    return(Best)
  }
  else if (num == "worst") {
    Best <- as.character(order_df[nrow(order_df), 1])  
    return(Best)
  }
  else if (num >= nrow(order_df)) {
    return(NA)
  }
  else {
    Best <- as.character(order_df[num, 1])  
    return(Best)
  }
}
