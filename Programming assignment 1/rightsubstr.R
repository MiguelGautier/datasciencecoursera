rightsubstr <- function(string,n = 1){
  ## This function returns the n character from the right of a string
  
  substr(string,nchar(string)-n+1,nchar(string))
  
}

