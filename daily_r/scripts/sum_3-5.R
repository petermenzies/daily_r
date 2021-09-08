## find sum of all multiples of 3 or 5 below 1000
  
  
sum(which((seq(1, 999) %% 3 == 0) | (seq(1, 999) %% 5) == 0)) 

