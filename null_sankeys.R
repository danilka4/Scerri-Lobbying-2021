source("functions.R")

null15 <- read.csv("data/null_2015.csv") %>% mutate(Year = 2015) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
null16 <- read.csv("data/null_2016.csv") %>% mutate(Year = 2016) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
null17 <- read.csv("data/null_2017.csv") %>% mutate(Year = 2017) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
null18 <- read.csv("data/null_2018.csv") %>% mutate(Year = 2018) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
null19 <- read.csv("data/null_2019.csv") %>% mutate(Year = 2019) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
null20 <- read.csv("data/null_2020.csv") %>% mutate(Year = 2020) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
null21 <- read.csv("data/null_2021.csv") %>% mutate(Year = 2021) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")

null_total <- rbind(null15, null16, null17, null18, null19, null20, null21)

colors <- list("y2015" = "4f2e39", "y2016" = "8f2d56", "y2017" = "ef476f", "y2018" = "ffd166", "y2019" = "06d6a0", "y2020" = "118ab2", "y2021" = "073b4c",
               "dead" = "6a5d5d")

labs <- c("Introduced", "Committee 1", "Floor 1",
          "Committee 2", "Floor 2",
          "Delivered to Governor", "Signed by Governor", "Law", "Dead")
link_hover <- paste("Flow between %{customdata[0]}<br>",
                   "and %{customdata[1]}",
                   "<extra>%{value:.0f}</extra>")
node_hover <- paste("%{customdata}",
                   "<extra>%{value:.0f}</extra>")

# Stuff for General Overview
joint_year <- rbind(
                    data_creator(null15, colors$y2015),
                    data_creator(null16, colors$y2016),
                    data_creator(null17, colors$y2017),
                    data_creator(null18, colors$y2018),
                    data_creator(null19, colors$y2019),
                    data_creator(null20, colors$y2020)#,
                    #data_creator(null21, colors$y2021)
                )
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(joint_year$x) - 1,
    target = as.numeric(joint_year$next_x) - 1,
    customdata = flow_names(joint_year),
    value = joint_year$n,
    color = ~as.factor(joint_year$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Diagram 2015-2020",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

vv_15 <- data_creator_vv(null15)
vv_16 <- data_creator_vv(null16)
vv_17 <- data_creator_vv(null17)
vv_18 <- data_creator_vv(null18)
vv_19 <- data_creator_vv(null19)
vv_20 <- data_creator_vv(null20)
#vv_21 <- data_creator_vv(null21)


plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(vv_15$x) - 1,
    target = as.numeric(vv_15$next_x) - 1,
    customdata = flow_names(vv_15),
    value = vv_15$n,
    color = ~as.factor(vv_15$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Sankey for 2015",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(vv_16$x) - 1,
    target = as.numeric(vv_16$next_x) - 1,
    customdata = flow_names(vv_16),
    value = vv_16$n,
    color = ~as.factor(vv_16$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Sankey for 2016",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(vv_17$x) - 1,
    target = as.numeric(vv_17$next_x) - 1,
    customdata = flow_names(vv_17),
    value = vv_17$n,
    color = ~as.factor(vv_17$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Sankey for 2017",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(vv_18$x) - 1,
    target = as.numeric(vv_18$next_x) - 1,
    customdata = flow_names(vv_18),
    value = vv_18$n,
    color = ~as.factor(vv_18$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Sankey for 2018",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(vv_19$x) - 1,
    target = as.numeric(vv_19$next_x) - 1,
    customdata = flow_names(vv_19),
    value = vv_19$n,
    color = ~as.factor(vv_19$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Sankey for 2019",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    customdata = node_names(labs),
    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
    color = "gray",
    hovertemplate = node_hover,
    thickness = 10,
    pad = 10),
  link = list(
    source = as.numeric(vv_20$x) - 1,
    target = as.numeric(vv_20$next_x) - 1,
    customdata = flow_names(vv_20),
    value = vv_20$n,
    color = ~as.factor(vv_20$color),
    hovertemplate = link_hover,
    line = list(color = "black", width = 0.5)
    )) %>%
  layout(title = "Null Sankey for 2020",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

#plot_ly(
#  type = "sankey",
#  arrangement = "snap",
#  node = list(
#    label = labs,
#    customdata = node_names(labs),
#    x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
#    y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
#    color = "gray",
#    hovertemplate = node_hover,
#    thickness = 10,
#    pad = 10),
#  link = list(
#    source = as.numeric(vv_21$x) - 1,
#    target = as.numeric(vv_21$next_x) - 1,
#    customdata = flow_names(vv_21),
#    value = vv_21$n,
#    color = ~as.factor(vv_21$color),
#    hovertemplate = link_hover,
#    line = list(color = "black", width = 0.5)
#    )) %>%
#  layout(title = "Null Sankey for 2021",
#         xaxis = list(showgrid = F, zeroline = F),
#         yaxis = list(showgrid = F, zeroline = F),
#         showlegend = T)

#yearly_sierra_joint <- plot_ly(
#  domain = list(x = c(0, 1), y = c(0, 0.49)),
#  type = "sankey",
#  arrangement = "snap",
#  node = list(
#    label = labels_total,
#    customdata = node_names(labels_total),
#    x = c(0, 0.15, 0.15, 0.15, 0.15, 0.25, 0.4, 0.4, 0.4, 0.4, 0.5, 0.65, 0.8, 1),
#    y = c(0, -0.2, 0.20, 0.40, 0.8, 0.5, -0.2, 0.2, 0.4, 0.8, 0.5, 0.54, 0.5, 0.5),
#    color = "gray",
#    hovertemplate = node_hover,
#    thickness = 10,
#    pad = 10), # 10 Pixel
#  link = list(
#    source = as.numeric(committees_total$x) - 1,
#    target = as.numeric(committees_total$next_x),
#    customdata = flow_names(committees_total),
#    value = committees_total$n,
#    color = ~as.factor(committees_total$color),
#    hovertemplate = link_hover,
#    line = list(color = "black", width = 0.5)
#    ))%>%
#  layout(title = "General Sankeys Separated by Committee",
#         xaxis = list(showgrid = F, zeroline = F),
#         yaxis = list(showgrid = F, zeroline = F),
#         showlegend = T)
