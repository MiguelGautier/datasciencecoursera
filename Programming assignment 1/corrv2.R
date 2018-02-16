corr <- function(directory = getwd(), threshold = 0){
  
  ## 'directory' character vector of lenght 1 indicating
  ## location of csv files to be read, default value working directory
  
  ## 'threshold' is a numeric vector of lenght 1 indicating the
  ## number of completely observed observations required to compute
  ## the correlation between nitrate and sulfate, default is 0 
  
  completeobs <- complete(directory)
  
  correlations <- c()
  
  for(i in seq_along(completeobs$id)){
    
    filename <- paste0(rightsubstr(paste0('00',completeobs$id[i]),3),'.csv')
    
    pathfile <- paste0(directory,'/',filename)
    
    info <- read.csv(pathfile, header = TRUE)
    
    if(completeobs$nobs[i]>= threshold){
      correlations <- cbind(correlations, cor(info$nitrate, info$sulfate, "complete.obs"))
    }
  }
  return(correlations)
}

