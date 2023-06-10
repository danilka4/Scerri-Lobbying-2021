source("functions.R")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = ""))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = ""))
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021, Year_dis = paste(Year, Dis, sep = ""))

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)


joint_year <- rbind(
                    data_creator(csv17, colors$y2017),
                    data_creator(csv18, colors$y2018),
                    data_creator(csv19, colors$y2019),
                    data_creator(csv20, colors$y2020),
                    data_creator(csv21, colors$y2021)
                )

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
joint17 <- data_creator(csv17, colors$y2017)
joint18 <- data_creator(csv18, colors$y2018)
joint19 <- data_creator(csv19, colors$y2019)
joint20 <- data_creator(csv20, colors$y2020)
joint21 <- data_creator(csv21, colors$y2021)

short_labs <- c("Intro", "PC1", "PF1",
          "PC2", "PF2",
          "DGov", "Signed", "Law", "Dead")

joint2017 <- plot_ly(
  domain = list(x = c(0, 0.32), y = c(0.51, 1)),
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = short_labs,
    x = c(0, 0.13, 0.2, 0.4, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.20, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(joint17$x) - 1,
    target = as.numeric(joint17$next_x) - 1,
    value = joint17$n,
    color = ~as.factor(joint17$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Annual Sankey without Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
joint2018 <- plot_ly(
  domain = list(x = c(0.34, 0.66), y = c(0.51, 1)),
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = short_labs,
    x = c(0, 0.13, 0.2, 0.4, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.10, 0.10, 0.10, 0.15, 0.07, 0.07, 0),
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(joint18$x) - 1,
    target = as.numeric(joint18$next_x) - 1,
    value = joint18$n,
    color = ~as.factor(joint18$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Annual Sankey without Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
joint2019 <- plot_ly(
  domain = list(x = c(0.68, 1), y = c(0.51, 1)),
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = short_labs,
    x = c(0, 0.13, 0.2, 0.4, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.20, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(joint19$x) - 1,
    target = as.numeric(joint19$next_x) - 1,
    value = joint19$n,
    color = ~as.factor(joint19$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Annual Sankey without Joint Resolutions 2019-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
joint2020 <- plot_ly(
  domain = list(x = c(0.0, 0.32), y = c(0,0.49)),
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = short_labs,
    x = c(0, 0.13, 0.2, 0.4, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.30, 0.18, 0.18, 0),
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(joint20$x) - 1,
    target = as.numeric(joint20$next_x) - 1,
    value = joint20$n,
    color = ~as.factor(joint20$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Annual Sankey without Joint Resolutions 2020-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
joint2021 <- plot_ly(
  domain = list(x = c(0.34, 0.66), y = c(0,0.49)),
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = short_labs,
    x = c(0, 0.13, 0.23, 0.4, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.37, 0.18, 0.18, 0),
    color = "gray",
    pad = 10),
  link = list(
    source = as.numeric(joint21$x) - 1,
    target = as.numeric(joint21$next_x) - 1,
    value = joint21$n,
    color = ~as.factor(joint21$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Individual Joint Graphs for 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
jointall <- plot_ly(
  domain = list(x = c(0.68, 1), y = c(0,0.49)),
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = short_labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    pad = 10),
  link = list(
    source = as.numeric(joint_year$x) - 1,
    target = as.numeric(joint_year$next_x) - 1,
    value = joint_year$n,
    color = ~as.factor(joint_year$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Joint Graphs for Years 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

subplot(joint2017, joint2018, joint2019,
        joint2020, joint2021, jointall)
