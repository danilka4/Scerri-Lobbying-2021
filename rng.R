set.seed(2017)
nums17 <- sample(1:1000, 1000)

set.seed(2018)
nums18 <- sample(1:1000, 1000)

set.seed(2019)
nums19 <- sample(1:1000, 1000)

set.seed(2020)
nums20 <- sample(1:1000, 1000)

set.seed(2021)
nums21 <- sample(1:1000, 1000)

output <- data.frame(nums17,
                     nums18,
                     nums19,
                     nums20,
                     nums21
)

write.csv(output, "random.csv")
output

