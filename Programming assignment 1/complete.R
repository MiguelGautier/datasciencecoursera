rightsubstr <- function(string,n = 1){
  ## This function returns the n character from the right of a string
  
  substr(string,nchar(string)-n+1,nchar(string))
  
}


complete <- function(directory = getwd(), id= 1:332){
  ## This function reads all the specified files of a directory
  ## and return a data frame with the number of complete cases in each file
  
  ## 'directory' character vector of lenght 1 indicating
  ## location of csv files to be read,default value working directory
  
  ## 'id'integer vector indicatig the monitors ID to be used
  
  completeobs <- data.frame()
       
  for(i in seq_along(id)){
    
    filename <- paste0(rightsubstr(paste0('00',id[i]),3),'.csv')
    
    pathfile <- paste0(directory,'/',filename)
    
    info <- read.csv(pathfile, header = TRUE)
   
    nobs<- sum(complete.cases(info))
    
    completeobs <- rbind(completeobs, c(id[i], nobs))
    
  }
  
  names(completeobs) <- c("id", "nobs")
  return(completeobs)
  
}

    