source("functions.R")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = ""))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = ""))
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021, Year_dis = paste(Year, Dis, sep = ""))

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)


default_sierra_csv <- no_return_sierra_data(csv_total)

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
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(default_sierra_csv$x) - 1,
    target = as.numeric(default_sierra_csv$next_x) - 1,
    value = default_sierra_csv$n,
    color = ~as.factor(default_sierra_csv$color),
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey for 2017-2020 Data",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

shell_sierra_csv <- no_return_sierra_data(csv_total, shell = TRUE)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1),
    y = c(0.7, 0.5, 0.4, 0.43, 0.45, 0.50, 0.38, 0.4),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(shell_sierra_csv$x) - 1,
    target = as.numeric(shell_sierra_csv$next_x) - 1,
    value = shell_sierra_csv$n,
    color = ~as.factor(shell_sierra_csv$color),
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey for 2017-2020 Data with Shells",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_sierra_csv <- sierra_data_dead(csv_total)


plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.5, 0.85, 0.87, 0.89, 0.89, 0.89, 0.92, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_sierra_csv$x) - 1,
    target = as.numeric(dead_sierra_csv$next_x) - 1,
    value = dead_sierra_csv$n,
    color = dead_sierra_csv$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_sierra_csv_colored <- sierra_data_dead(csv_total, color_black = FALSE)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.5, 0.85, 0.87, 0.89, 0.89, 0.89, 0.92, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_sierra_csv_colored$x) - 1,
    target = as.numeric(dead_sierra_csv_colored$next_x) - 1,
    value = dead_sierra_csv_colored$n,
    color = dead_sierra_csv_colored$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_csv<- rbind(data_creator_dead(csv17, color_id = colors$y2017),
                  data_creator_dead(csv18, color_id = colors$y2018),
                  data_creator_dead(csv19, color_id = colors$y2019),
                  data_creator_dead(csv20, color_id = colors$y2020)
)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.5, 0.85, 0.87, 0.89, 0.89, 0.89, 0.92, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_csv$x) - 1,
    target = as.numeric(dead_csv$next_x) - 1,
    value = dead_csv$n,
    color = dead_csv$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_csv_colored <- rbind(data_creator_dead(csv17, color_id = colors$y2017, color_black = FALSE),
                  data_creator_dead(csv18, color_id = colors$y2018, color_black = FALSE),
                  data_creator_dead(csv19, color_id = colors$y2019, color_black = FALSE),
                  data_creator_dead(csv20, color_id = colors$y2020, color_black = FALSE)
)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.5, 0.85, 0.87, 0.89, 0.89, 0.89, 0.9, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_csv_colored$x) - 1,
    target = as.numeric(dead_csv_colored$next_x) - 1,
    value = dead_csv_colored$n,
    color = dead_csv_colored$color,
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

csv_colored <- rbind(data_creator(csv17, color_id = "ef476f"),
                  data_creator(csv18, color_id = "ffd166")
)

plot_ly(
  type = "sankey",
  #arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1),
    y = c(0.5, 0.4, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(csv_colored$x) - 1,
    target = as.numeric(csv_colored$next_x) - 1,
    value = csv_colored$n,
    color = csv_colored$color,
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph for 2017-2018",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
