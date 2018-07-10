
set.seed(13)

# Die längste Sequenz von 1s bestimmen
longestrun <- function(boolean_vec){
  output <- rle(boolean_vec)
  output
  seq_ones <- output$lengths[which(output$values == 1)]
  max(seq_ones)
}

# Test
boolean_vec <- sample(x = c(0,1), 
                      size=10, replace = TRUE)
boolean_vec
longestrun(boolean_vec)