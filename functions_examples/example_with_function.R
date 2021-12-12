source("plotly_function.R")
# Example: 
csv<- read.csv("sample/2017b.csv")
##c49c94
new_total_df <- rbind(data_creator(csv, "rgba(196,156,148,0.6)"), data_creator(csv, "rgba(31,119,180,0.6)"))

labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2", 
          "Delivered to Governor", "Signed by Governor", "Law")
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0.1, 0.23, 0.4, 0.5, 0.6, 0.75, 0.9),
    y = c(0.5, 0.5, 0.5, 0.29, 0.4, 0.5, 0.3),
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