rankhospital <- function(state, outcome,num="best"){
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)   ## Assumes the file is in the WD
  
  ##Check that state and outcome are valid
  dataState <- unique(data$State) ## subset with the possible states 
  if (is.element(state,dataState)== FALSE){
    stop("invalid state")
  }
  
  possOutcomes <- c("heart attack", "heart failure", "pneumonia") ##Vector with possible outcomes
  if (is.element(outcome,possOutcomes)== FALSE){
    stop("invalid outcome")
  }
  
  ##Table with the desired outcome column, excluding Not Available values, and Hospitals of the given State
  
  if(outcome == "heart attack"){
    data <- subset.data.frame(data,data[,11] != "Not Available")
    StateHosp<- data.frame("Hospital.Name"=data$Hospital.Name,"State"=data$State
                           ,"heart attack"=as.numeric(data[,11]))
    StateHosp<- subset.data.frame(StateHosp,  StateHosp$State == state)
  }else
    if(outcome == "heart failure"){
      
      data <- subset.data.frame(data,data[,17] != "Not Available")
      StateHosp<- data.frame("Hospital.Name"=data$Hospital.Name,"State"=data$State
                             ,"heart failure"=as.numeric(data[,17]))
      StateHosp<- subset.data.frame(StateHosp,  StateHosp$State == state)
    }else
      if(outcome == "pneumonia"){
        data <- subset.data.frame(data,data[,23] != "Not Available")
        StateHosp<- data.frame("Hospital.Name"=data$Hospital.Name,"State"=data$State
                               ,"pneumonia"= as.numeric(data[,23]))
        StateHosp<- subset.data.frame(StateHosp,  StateHosp$State == state)
      }
  Ranking <- StateHosp[order(StateHosp[,3],StateHosp[,1]),]
  
  if(num == "best"){
    return(Ranking[1,1])
  }else 
    if(num == "worst"){
      return(Ranking[nrow(Ranking),1])
    }else {
      return(Ranking[num,1])
    }
   
}