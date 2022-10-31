library(dplyr)

df <- data.frame(name = c("Alex", "Ben", "Chris"), age = c(1,2,3))
df[(nrow(df)+1):(nrow(df)+2),] <- data.frame(name = c("Daniel", "Ed"), age = c(4,5))
