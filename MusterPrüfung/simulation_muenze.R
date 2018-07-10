
set.seed(13)

# Die längste Sequenz von 1s bestimmen
longestrun <- function(boolean_vec){
  output <- rle(boolean_vec)
  output
  seq_ones <- output$lengths[which(output$values == 1)]
  max(seq_ones)
}
###########

# Aufgabe 1

boolean_vec <- sample(x = c(0,1), 
                      size=20, replace = TRUE)
boolean_vec
longestrun(boolean_vec)
###########

# Aufgabe 2

count <- 0
for (i in 1:1000) {
  boolean_vec <- sample(x = c(0,1), 
                        size=20, replace = TRUE)
  x <- longestrun(boolean_vec)
  if (x >= 7) count = count + 1
}

# relative Häufigkeit
count/1000

###########

# Ich schreibe lieber eine Funktion 
# mit Sample-size als Parameter. 

rel_Haeufigkeit <- function(sample_size){
  count <- 0
  for (i in 1:1000) {
    boolean_vec <- sample(x = c(0,1), replace = TRUE,
                          size = sample_size)
    x <- longestrun(boolean_vec)
    if (x >= 7) count = count + 1
  }
  return(count/1000)
}

##########

# Aufgabe 3

df <- data.frame("sample_size" = NA, 
                 "relative_Häufigkeit" = NA)

int <- 0
for (i in c(seq(10, 400, 10))) {
  int <- int + 1
  df[int,] <- c(i, rel_Haeufigkeit(i))
  
}

plot(df$sample_size, df$relative_Häufigkeit) 
abline(h = 0.5, col = "red")

df$sample_size[df$relative_Häufigkeit >= 0.5]

# Antwort:
# etwa 180 Male muss die Münze geworfen werden,  
# um eine Gewinnchance von mindestens 50% zu haben.

