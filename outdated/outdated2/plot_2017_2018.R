source("functions_examples/plotly_function.R")
source("functions_examples/function_dead.R")

csv17 <- read.csv("data/csv_2017.csv")
csv18 <- read.csv("data/csv_2018.csv")

colors <- c("rgba(196,156,148,0.6)", "rgba(31,119,180,0.6)")
new_total_df <- rbind(data_creator_dead(csv17, colors[1], FALSE), data_creator_dead(csv18, colors[2], FALSE))
labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2", 
          "Delivered to Governor", "Signed by Governor", "Law", "Dead")
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.83, 0.85, 0.86, 0.87, 0.89, 0.9, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(new_total_df$x) - 1,
    target = as.numeric(new_total_df$next_x) - 1,
    value = new_total_df$n,
    color = ~as.factor(new_total_df$color),
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey for 2017b Data",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
