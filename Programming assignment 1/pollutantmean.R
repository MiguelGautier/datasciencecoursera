rightsubstr <- function(string,n = 1){
  ## This function returns the n character from the right of a string
  
  substr(string,nchar(string)-n+1,nchar(string))
  
}


pollutantmean <- function(directory = getwd(), pollutant = "sulfate", id = 1:332){
  ## This function returns the mean of the specified pollutant
  ## across the specified monitors. 
  
  ## 'directory' character vector of lenght 1 indicating
  ## location of csv files to be read, default value working directory
  
  ##'pollutant' character vector of lenght 1 indicatig the name of the 
  ## pollutant for which the mean is to be calculated. 
  ## Values "sulfate" or "nitrate"
  
  ## 'id'integer vector indicatig the monitors ID to be used
  
  readings <- 0 ## variable to sum the readings of the monitors
  div <- 0  ## count number of readings 
  
  for(i in seq_along(id)){
      
      filename <- paste0(rightsubstr(paste0('00',id[i]),3),'.csv')
      
      pathfile <- paste0(directory,'/',filename)
                        
      info <- read.csv(pathfile, header = TRUE)
      
      readings <- readings + sum(info[,pollutant],na.rm = TRUE)
      div <- div + sum(!is.na(info[,pollutant]))
      
    
  }
  readingsmean <- readings/div
  return(readingsmean)
  
}