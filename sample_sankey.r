library(ggplot2)
library(dplyr)
library(ggsankey)


df <- make_long(mtcars, cyl, vs, am, gear, carb)

ggplot(df, aes(x = x,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow_alpha = 0.5, node.color = 1) + 
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
