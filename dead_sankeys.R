# Sources functions for dataframe creation
source("functions.R")

# Obtains the different years. Unsure how many of those pipes are necessary but they are used at some point in the various plotly's and ggplots
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = ""))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = ""))
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021, Year_dis = paste(Year, Dis, sep = ""))

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)



# Creates the list of all the colors that will be used for each of the years
colors <- list("y2017" = "ef476f", "y2018" = "ffd166", "y2019" = "06d6a0", "y2020" = "118ab2", "y2021" = "073b4c",
               "dead" = "6a5d5d")
# New labels, since the old ones have underscores and dots instead of spaces
labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2",
          "Delivered to Governor", "Signed by Governor", "Law", "Dead")

# JOINT STUFFFFFFFFF

dead_sierra_joint <- sierra_data_dead(csv_total)
plot_ly(
    # sets a type for plotly
  type = "sankey",
  arrangement = "snap",
  # sets up node behavior
  node = list(
    # makes labels not ugly
    label = labs,
    # sets up node coordinates. Note that (1,1) is bottom right
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    # Node color
    color = "gray",
    pad = 10), # 10 Pixel
    # Sets up flow behavior
  link = list(
    # Subtract 1 from node index because we want index 0
    source = as.numeric(dead_sierra_joint$x) - 1,
    target = as.numeric(dead_sierra_joint$next_x) - 1,
    value = dead_sierra_joint$n,
    color = dead_sierra_joint$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_sierra_joint_colored <- sierra_data_dead(csv_total, color_black = FALSE)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_sierra_joint_colored$x) - 1,
    target = as.numeric(dead_sierra_joint_colored$next_x) - 1,
    value = dead_sierra_joint_colored$n,
    color = dead_sierra_joint_colored$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)


dead_year_joint <- rbind(
                        data_creator_dead(csv17, colors$y2017),
                        data_creator_dead(csv18, colors$y2018),
                        data_creator_dead(csv19, colors$y2019),
                        data_creator_dead(csv20, colors$y2020),
                        data_creator_dead(csv21, colors$y2021)
                        )

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_year_joint$x) - 1,
    target = as.numeric(dead_year_joint$next_x) - 1,
    value = dead_year_joint$n,
    color = dead_year_joint$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_year_joint_colored <- rbind(
                        data_creator_dead(csv17, colors$y2017, color_black = FALSE),
                        data_creator_dead(csv18, colors$y2018, color_black = FALSE),
                        data_creator_dead(csv19, colors$y2019, color_black = FALSE),
                        data_creator_dead(csv20, colors$y2020, color_black = FALSE),
                        data_creator_dead(csv21, colors$y2021, color_black = FALSE)
                        )
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_year_joint_colored$x) - 1,
    target = as.numeric(dead_year_joint_colored$next_x) - 1,
    value = dead_year_joint_colored$n,
    color = dead_year_joint_colored$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

data_creator_dead <- function(csv, color_id, color_black = TRUE, include_joint = TRUE)




# NOT JOINT STUFFFFFFFFF

dead_sierra <- sierra_data_dead(csv_total, include_joint = FALSE)
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_sierra$x) - 1,
    target = as.numeric(dead_sierra$next_x) - 1,
    value = dead_sierra$n,
    color = dead_sierra$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_sierra_colored <- sierra_data_dead(csv_total, color_black = FALSE, include_joint = FALSE)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_sierra_colored$x) - 1,
    target = as.numeric(dead_sierra_colored$next_x) - 1,
    value = dead_sierra_colored$n,
    color = dead_sierra_colored$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)


dead_year <- rbind(
                        data_creator_dead(csv17, colors$y2017, include_joint = FALSE),
                        data_creator_dead(csv18, colors$y2018, include_joint = FALSE),
                        data_creator_dead(csv19, colors$y2019, include_joint = FALSE),
                        data_creator_dead(csv20, colors$y2020, include_joint = FALSE),
                        data_creator_dead(csv21, colors$y2021, include_joint = FALSE)
                        )

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_year$x) - 1,
    target = as.numeric(dead_year$next_x) - 1,
    value = dead_year$n,
    color = dead_year$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

dead_year_colored <- rbind(
                        data_creator_dead(csv17, colors$y2017, color_black = FALSE, include_joint = FALSE),
                        data_creator_dead(csv18, colors$y2018, color_black = FALSE, include_joint = FALSE),
                        data_creator_dead(csv19, colors$y2019, color_black = FALSE, include_joint = FALSE),
                        data_creator_dead(csv20, colors$y2020, color_black = FALSE, include_joint = FALSE),
                        data_creator_dead(csv21, colors$y2021, color_black = FALSE, include_joint = FALSE)
                        )
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.35, 0.45, 0.64, 0.75, 1, 1),
    y = c(0, 0.43, 0.76, 0.77, 0.83, 0.81, 0.83, 0.82, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(dead_year_colored$x) - 1,
    target = as.numeric(dead_year_colored$next_x) - 1,
    value = dead_year_colored$n,
    color = dead_year_colored$color,
    line = list(color = colors$dead, width = 0.5)
    ))%>% 
  layout(title = "Sankey Graph with Dead Included",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
