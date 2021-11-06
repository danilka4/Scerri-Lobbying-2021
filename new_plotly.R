library(plotly)
library(rjson)

json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
  type = "sankey",
  domain = c(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  
  node = list(
    label = json_data$data[[1]]$node$label,
    color = json_data$data[[1]]$node$color,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = json_data$data[[1]]$link$source,
    target = json_data$data[[1]]$link$target,
    value =  json_data$data[[1]]$link$value,
    color =  json_data$data[[1]]$link$color,
    label =  json_data$data[[1]]$link$label
  )
) %>% 
  layout(
    title = "Energy forecast for 2050, UK - Department of Energy & Climate Changes",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )
p
