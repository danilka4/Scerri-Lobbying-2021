source("functions.R")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = ""))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = ""))
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021, Year_dis = paste(Year, Dis, sep = ""))

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)



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

sierra_joint <- sierra_data(csv_total)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(sierra_joint$x) - 1,
    target = as.numeric(sierra_joint$next_x) - 1,
    value = sierra_joint$n,
    color = ~as.factor(sierra_joint$color),
    line = list(color = "black", width = 0.5)
    ))%>%
  layout(title = "Sierra Sankey with Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)


sierra_no_joint <- sierra_data(csv_total, include_joint = FALSE)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.20, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(sierra_no_joint$x) - 1,
    target = as.numeric(sierra_no_joint$next_x) - 1,
    value = sierra_no_joint$n,
    color = ~as.factor(sierra_no_joint$color),
    line = list(color = "black", width = 0.5)
    ))%>%
  layout(title = "Sierra Sankey without Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

joint_year <- rbind(
                    data_creator(csv17, colors$y2017),
                    data_creator(csv18, colors$y2018),
                    data_creator(csv19, colors$y2019),
                    data_creator(csv20, colors$y2020),
                    data_creator(csv21, colors$y2021)
                )

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
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
    ))%>%
  layout(title = "Annual Sankey with Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

no_joint_year <- rbind(
                    data_creator(csv17, colors$y2017,include_joint = FALSE),
                    data_creator(csv18, colors$y2018,include_joint = FALSE),
                    data_creator(csv19, colors$y2019,include_joint = FALSE),
                    data_creator(csv20, colors$y2020,include_joint = FALSE),
                    data_creator(csv21, colors$y2021,include_joint = FALSE)
                )

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.20, 0.17, 0.17, 0),
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(no_joint_year$x) - 1,
    target = as.numeric(no_joint_year$next_x) - 1,
    value = no_joint_year$n,
    color = ~as.factor(no_joint_year$color),
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Annual Sankey without Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
