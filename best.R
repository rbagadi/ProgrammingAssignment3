best <- function(state, outcome) {
  
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
    order_df <- state_df[order(state_df[, 2], state_df[, 1]), ]
    }
  else if (outcome == "heart failure") {
    order_df <- state_df[order(state_df[, 3], state_df[, 1]), ]
    }
  else {
    order_df <- state_df[order(state_df[4], state_df[, 1]), ]
  }
  
  Best <- as.character(order_df[1, 1])  
  return(Best)
}
