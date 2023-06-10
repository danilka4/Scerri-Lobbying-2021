source("functions.R")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)

head(csv17)

#ef476f
#ffd166
#06d6a0
#118ab2
#073b4c
#8f2d56

colors <- list("y2017" = "ef476f", "y2018" = "ffd166", "y2019" = "06d6a0", "y2020" = "118ab2", "y2021" = "073b4c",
               "dead" = "6a5d5d")
labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2",
          "Delivered to Governor", "Signed by Governor", "Law", "Dead")

csv_com_total <- separate(csv_total, Com.1, into = "Com.1", sep = ";") %>%
    mutate(Com.1 = if_else(Com.1 == "H-CL" | Com.1 == "H-LC", "H-CL/LC", Com.1))
colnames(csv_com_total)
committees <- com_sierra(csv_com_total, TRUE)
labels <- c("Introduced", levels(committees$x)[2:5], "Passed Floor 1", "Passed Committee 2", "Passed Floor 2", "Delivered to Governor", "Signed by Governor", "Passed")

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labels,
    x = c(0, 0.2, 0.2, 0.2, 0.2),#, 0.35, 0.5, 0.65, 0.8, 1),
    y = c(0, -0.2, 0.20, 0.40, 0.8),#, 0.5, 0.5, 0.5, 0.5),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(committees$x) - 1,
    target = as.numeric(committees$next_x),
    value = committees$n,
    color = ~as.factor(committees$color),
    line = list(color = "black", width = 0.5)
    ))%>%
  layout(title = "Sierra by Committee in Total",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

csv177 <- separate(csv17, Com.1, into = c("Com.1", "Com.1.1", "Com.1.2", "Com.1.3"), sep = ";")
nrow(csv177)
csv177$Com.1
head(group_by(csv177, Com.1) %>% summarize(n = n()), n = 20)
filter(csv177, Com.1 == "H-CL")


rtable(csv17)
