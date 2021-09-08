## Given this prompt: imagine a circle and two squares - a smaller and a bigger one. For the smaller one, the circle is a circumcircle and for the bigger one, an incircle.

## Create a function, that takes an integer (radius of the circle) and returns the difference of the areas of the two squares.

sq_diff <- function(radius) {
  
  big_square <- (radius * 2) ^ 2
  
  small_square <- 2 * (radius ^ 2)
  
  return(big_square - small_square)
  
}
