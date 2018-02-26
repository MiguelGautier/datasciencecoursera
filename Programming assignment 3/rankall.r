rankall <- function(outcome,num="best"){
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)   ## Assumes the file is in the WD
  
  ##Check outcome validity
  possOutcomes <- c("heart attack", "heart failure", "pneumonia") ##Vector with possible outcomes
  if (is.element(outcome,possOutcomes)== FALSE){
    stop("invalid outcome")
  }
  
  ##Table with the desired outcome column, excluding Not Available values, and Hospitals of the given State
  
  if(outcome == "heart attack"){
    data <- subset.data.frame(data,data[,11] != "Not Available")
    StateHosp<- data.frame("Hospital.Name"=data$Hospital.Name,"State"=data$State
                           ,"heart attack"=as.numeric(data[,11]))
  }else
    if(outcome == "heart failure"){
      
      data <- subset.data.frame(data,data[,17] != "Not Available")
      StateHosp<- data.frame("Hospital.Name"=data$Hospital.Name,"State"=data$State
                             ,"heart failure"=as.numeric(data[,17]))
    }else
      if(outcome == "pneumonia"){
        data <- subset.data.frame(data,data[,23] != "Not Available")
        StateHosp<- data.frame("Hospital.Name"=data$Hospital.Name,"State"=data$State
                               ,"pneumonia"= as.numeric(data[,23]))
      }
  splitStates <- split(StateHosp,StateHosp[,2])
  finalrank <- lapply(splitStates,FUN=function(list,num){
    
    list <- list[order(list[,3],list[,1]),]
    
    if(num == "best"){
      return(list[1,1])
        
    } else 
      if(num == "worst"){
        return(list[nrow(list),1])
      }else {
        return(list[num,1])
      }
  },num)

  return(data.frame(hospital = unlist(finalrank), state = names(finalrank)))
  
}