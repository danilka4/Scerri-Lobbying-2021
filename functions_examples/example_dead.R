source("function_dead.R")
# Example: 
csv<- read.csv("sample/2017b.csv")
##c49c94
new_total_df <- sierra_data_dead(csv, FALSE)

labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2", 
          "Delivered to Governor", "Signed by Governor", "Law", "Dead")
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.5, 0.8, 0.82, 0.85, 0.87, 0.87, 0.9, 0),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(new_total_df$x) - 1,
    target = as.numeric(new_total_df$next_x) - 1,
    value = new_total_df$n,
    color = new_total_df$color,
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey for 2017b Data",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
# Valid attributes include:
#'arrangement', 'customdata', 'customdatasrc', 'domain', 'hoverinfo', 
#'hoverlabel', 'ids', 'idssrc', 'legendgrouptitle', 'legendrank', 
#'link', 'meta', 'metasrc', 'name', 'node', 'orientation', 'selectedpoints', 
#'stream', 'textfont', 'type', 'uid', 'uirevision', 'valueformat',
#'valuesuffix', 'visible', 'key', 'set', 'frame', 'transforms', 
#'_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'