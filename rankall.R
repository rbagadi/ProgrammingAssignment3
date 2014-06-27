rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  
  ## Check that outcome is valid
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  else if (outcome == valid_outcomes[1]) {
    outcome_data[, 11] <- as.numeric(outcome_data[, 11])
    Mortality_col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } 
  else if (outcome == valid_outcomes[2]) {
    outcome_data[, 17] <- as.numeric(outcome_data[, 17])
    Mortality_col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } 
  else if (outcome == valid_outcomes[3]) {
    outcome_data[, 23] <- as.numeric(outcome_data[, 23])
    Mortality_col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }

  ## For each state, find the hospital of the given rank
  outcome_data <- outcome_data[, c("Hospital.Name", "State", Mortality_col_name)]
  
  split_by_state <- split(outcome_data, outcome_data$State)
  
  rank_hospital_by_state <- function(state_data_part, num) {
    state_rank_index <- order(state_data_part[3], state_data_part$Hospital.Name, na.last=NA)
    
    if (num == "best") {
      state_data_part$Hospital.Name[state_rank_index[1]]
    } else if (num == "worst") {
      state_data_part$Hospital.Name[state_rank_index[length(state_rank_index)]]
    } else if (is.numeric(num)) {
      state_data_part$Hospital.Name[state_rank_index[num]]
    } else if (is.numeric(num) >= length(state_rank_index)) {
      stop("invalid num")
    }
  }

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  State_Ranks <- lapply(split_by_state, rank_hospital_by_state, num)
  data.frame(hospital = unlist(State_Ranks), state = names(State_Ranks), row.names = names(State_Ranks))
}