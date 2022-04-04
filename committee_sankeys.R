source("functions.R")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017)
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018)
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019)
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020)
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021)

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)

head(csv17)

#ef476f
#ffd166
#06d6a0
#118ab2
#073b4c
colors <- list("y2017" = "ef476f", "y2018" = "ffd166", "y2019" = "06d6a0", "y2020" = "118ab2", "y2021" = "073b4c",
               "dead" = "6a5d5d")
labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2",
          "Delivered to Governor", "Signed by Governor", "Law", "Dead")


head(committees <- com_sierra(csv18, FALSE), n = 20)
(labels <- c(levels(committees$x), "Passed"))
levels(committees$next_x)

test <- data.frame(x = c("a", "b","c","d"), y = c(1,2,3,4))
test[test$x == "a",2]

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labels,
    #x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    #y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(committees$x) - 1,
    target = as.numeric(committees$next_x),
    value = committees$n,
    color = ~as.factor(committees$color),
    line = list(color = "black", width = 0.5)
    ))%>%
  layout(title = "Sierra Sankey with Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
