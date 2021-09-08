library(tidyverse)

# Bonus function that will find the number of permutations of the elements in a given vector
# Even those with duplicate elements (the reason it was necessary for my method)
# Created to provide the proper stopping point for my while-loop below without cluttering it up ----

num_permuatation <- function(value) {
  
  char_repeats <- vector(mode = "numeric")
  
  unique_val <- unique(value)
  
# iterating through the unique values in our vector so that the instances of each 
# character in the original vector are only evaluated once
  
# gregexpr() looks for i in value and outputs a list representing indices in value 
# where i was not present with a -1 and where it was, with a 1. reagmatches() then 
# replaces the 1's with i and removes all the -1's. lengths() then outputs a vector
# with 1's in place of the i values and zeroes elsewhere. We take the sum of that 
# and add that number (num) to our char_repeats vector. 
# Surely this is the worst possible way to do this...but it works
  
  for (i in unique_val) {
    
    num <- sum(lengths(regmatches(value, gregexpr(i, value))))
    char_repeats <- append(char_repeats, num)
    
  }
  
  # set up the object that will be our factorial product
  factorial_val <- 1
  
  for (i in char_repeats) {
    
    # multiplying our factorialized duplicate counts gives us the denominator 
    # for our permutation equation below
    factorial_val <- factorial_val * factorial(i)
    
  }
  
  perms <- factorial(length(value)) / factorial_val
  return(perms)
  
}

# On to the actual challenge ----

# Function that takes a string and outputs a vector containing all possible anagrams

anagramr <- function(string) {
  
  string <- tolower(string)
  
  # vec to populate with anagrams
  anagram_vec <- vector(mode = "character")
  
  # strsplit outputs a list with our separated string characters, and indexing 
  # with the double brackets returns a vector
  string_vec <- strsplit(string, "")[[1]]
  
  # using function from above
  permutations <- num_permuatation(string_vec)
  
  # this while-loop will continue generating randomly sorted vectors of numbers 
  # (with a length that is the number of charters in the given string) and using 
  # those numbers as indices to create new string permutations until the length 
  # of the vector we're populating hits the number of possible permutations. The 
  # nested if statement prevents repeats from being added to the output vector.
  
  while (length(anagram_vec) < permutations) {
    
    rand_index <- sample(seq(1, nchar(string)), size = nchar(string))
    
    new_gram <- paste(string_vec[rand_index], collapse = "")
    
    if (!new_gram %in% anagram_vec){
      
      anagram_vec <- append(anagram_vec, new_gram)
      
    }
    
  }
  
  return(anagram_vec)
  
}

