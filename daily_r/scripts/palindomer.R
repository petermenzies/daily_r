# Function to determine if string input is a palindrome or not

library(tidyverse)


palindromer <- function(pal) {
  
  pal <- str_replace_all(pal, " ", "") %>% tolower()
  
  pal_split <- strsplit(pal, "") 
  
  pal_vec <- vector(mode = "character")
  
  for (character in pal_split) {
    pal_vec <- append(pal_vec, character)

  }
  
  vec_length <- as.numeric(length(pal_vec))
  
  for (i in seq_along(pal_vec)) {
    if (pal_vec[i] != pal_vec[(vec_length + 1) - i]) {
      
      return("not a palindrome ya dingdong")
      
    }
    
  } 
  
  return("you gotchurself a palindrome!!")
  
}


