fonts <- c("Arial", "Balto", "Courier New", "Droid Sans", "Droid Serif", "Droid Sans Mono", "Gravitas One", "Old Standard TT", "Open Sans", "Overpass", "PT Sans Narrow", "Raleway", "Times New Roman")
font <- "arial"
    plot_ly(
     type = "sankey",
      arrangement = "snap",
      node = list(
        label = paste0("<b>", labs, "</b>"),
        customdata = node_names(labs),
        x = c(0, 0.13, 0.2, 0.33, 0.5, 0.63, 0.81, 1, 1),
        y = c(0.5, 0.49, 0.20, 0.20, 0.20, 0.24, 0.17, 0.17, 0),
        color = "gray",
        hovertemplate = node_hover,
        pad = 10,
        thickness = 10), # 10 Pixel
      link = list(
        source = as.numeric(sierra_joint$x) - 1,
        target = as.numeric(sierra_joint$next_x) - 1,
        customdata = flow_names(sierra_joint),
        value = sierra_joint$n,
        color = ~as.factor(sierra_joint$color),
        hovertemplate = link_hover,
        line = list(color = "black", width = 0.5)
        )) %>%
      layout(title = list(font = list(family = font, size = 19), text = "<b>VA Climate Bill Lifetimes 2015-2021</b>", y = 0.993),
             xaxis = list(showgrid = F, zeroline = F),
             yaxis = list(showgrid = F, zeroline = F),
             font  = list(family = font, size = 25),
             showlegend = T)
#length(font)
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
#    pad = 10,
#    thickness = 10), # 10 Pixel
#  link = list(
#    source = as.numeric(sierra_joint$x) - 1,
#    target = as.numeric(sierra_joint$next_x) - 1,
#    customdata = flow_names(sierra_joint),
#    value = sierra_joint$n,
#    color = ~as.factor(sierra_joint$color),
#    hovertemplate = link_hover,
#    line = list(color = "black", width = 0.5)
#    )) %>%
#  layout(title = list(font = list(family = font, size = 19), text = "VA Climate Bill Lifetimes 2015-2021", y = 0.993),
#         xaxis = list(showgrid = F, zeroline = F),
#         yaxis = list(showgrid = F, zeroline = F),
#         font  = list(family = font, size = 25),
#         showlegend = T)
#
##library(plotly)
##library(cowplot)
##source("functions.R")
##
##csv15 <- read.csv("data/csv_2015.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2015) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##csv16 <- read.csv("data/csv_2016.csv", nrows = 74) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2016) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##csv21 <- read.csv("data/csv_2021.csv", nrows = 84) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021) %>%
##    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
##
##csv_total <- rbind(csv15, csv16, csv17, csv18, csv19, csv20, csv21)
##
##
##
##colors <- list("y2015" = "4f2e39", "y2016" = "8f2d56", "y2017" = "ef476f", "y2018" = "ffd166", "y2019" = "06d6a0", "y2020" = "118ab2", "y2021" = "073b4c",
##               "dead" = "6a5d5d")
##labs <- c("Introduced\n628 bills", "Committee 1\n628 bills", "Floor 1\n287 bills",
##          "Committee 2\n280 bills", "Floor 2\n247 bills",
##          "Delivered to Governor\n233 bills*", "Signed by Governor\n232 bills", "Law\n225 bills", "Dead")
##
##link_hover <- paste("Flow between %{customdata[0]}<br>",
##                   "and %{customdata[1]}",
##                   "<extra>%{value:.0f}</extra>")
##node_hover <- paste("%{customdata}",
##                   "<extra>%{value:.0f}</extra>")
##
##csv_com_total <- separate(csv_total, Com.1, into = "Com.1", sep = ";") %>%
##    mutate(Com.1 = if_else(Com.1 == "H-CL" | Com.1 == "H-LC", "H-CL/LC", Com.1),
##    Com.2 = if_else(Com.2 == "H-CL" | Com.2 == "H-LC", "H-CL/LC", Com.2))
##committees_total <- com_sierra(csv_com_total)
##labels_total <- c("Introduced", levels(committees_total$x)[2:4], "Other Committee", "Passed Floor 1", gsub(".{2}$", "", levels(committees_total$x)[7:9]), "Other Committee", "Passed Floor 2", "Delivered to Governor", "Signed by Governor", "Passed")
##sierra_joint <- sierra_data(csv_total)
##
##
###gg17 <- ggplot(csv_total, aes(Dis, fill = Pos)) + geom_bar() +
###  scale_fill_manual(
###                     values = c("Supported" = "#9ACD32", "Neutral" = "#B0E0E6", "Opposed" = "#FF4500", "NA" = "#BEC4C6")) +
###  theme_minimal() +
###  scale_x_discrete(limits = c("Died in Committee", "Died Elsewhere", "Passed into Law")) + 
###  labs(title = "Fate of Different Bills in Relation to Sierra Club's Position in 2017 and 2018",
###       x = "Final Outcome of Bill", y = "Number of Bills",
###       fill = "Sierra Position") + 
###    theme(legend.text = element_text(size = 13),
###          legend.title = element_text(size = 18),
###          legend.key.size = unit(1.8, "cm"))
###plot(get_legend(gg17))
###ggsave("legend.png", bg = "white")
