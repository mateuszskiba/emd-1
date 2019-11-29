library(zoo)
library(dplyr)

herrings <- read.csv(file="sledzie.csv", header=TRUE, sep=",", na.strings="?")
herrings <- 
  herrings %>%
  do(na.locf(.))