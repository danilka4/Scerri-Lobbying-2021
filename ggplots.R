source("functions.r")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv_total <- rbind(csv17, csv18)

colors <- c("rgba(196,156,148,0.6)", "rgba(31,119,180,0.6)")

gg17 <- ggplot(csv17, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(limits = c("Died in Committee", "Died Elsewhere", "Passed into Law")) + 
  labs(title = "Fate of Different Bills in Relation to Sierra Club's Position in 2017 and 2018",
       x = "Final Outcome of Bill", y = "Number of Bills",
       fill = "Sierra Club Position") + guides(fill = "none")

gg18 <- ggplot(csv18, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(limits = c("Died in Committee", "Died Elsewhere", "Passed into Law")) + 
  labs(title = "",
       x = "", y = "",
       fill = "Sierra Club Position") + guides(fill = "none")

ggpubr::ggarrange(gg17, gg18, ncol = 2)

# Regular Stacked barplot
ggplot(csv_total, aes(Year_dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(labels = c( "2017-Died in Committee","Died Afterwards", "Passed in Law",  "2018-Died in Committee", "Died Afterwards","Passed in Law"),
                   limits = c("2017Died in Committee", "2017Died Elsewhere", "2017Passed into Law",
                              "2018Died in Committee", "2018Died Elsewhere", "2018Passed into Law")) + 
  labs(title = "Fate of Different Bills in Relation to Sierra Club's Position",
       x = "Final Outcome of Bill", y = "Number of Bills",
       fill = "Sierra Club Position")

changed_csv <- csv_total %>% group_by(Year_dis, Pos) %>% summarize(n = n())
# 
ggplot(changed_csv, aes(Year_dis, n, fill = Pos)) + geom_col(position = "fill") +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(labels = c( "2017-Died in Committee","Died Afterwards", "Passed in Law",  "2018-Died in Committee", "Died Afterwards","Passed in Law"),
                   limits = c("2017Died in Committee", "2017Died Elsewhere", "2017Passed into Law",
                              "2018Died in Committee", "2018Died Elsewhere", "2018Passed into Law")) + 
  labs(title = "Proportional Fate of Different Bills in Relation to Sierra Club's Position",
       x = "Final Outcome of Bill", y = "Proportion of Bills",
       fill = "Sierra Club Position")


library(ggsankey)

alluvial_data <- make_long(csv_total, Pos, Dis)
ggplot(alluvial_data, aes(x = x,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_alluvial(flow_alpha = 0.7, node.color = 1, node.fill = "gray") + 
  geom_alluvial_label(size = 3.5, color = 1, fill = "white") +
  theme_alluvial(base_size = 16) + 
  scale_x_discrete(labels = c("Position", "Fate")) +
  scale_fill_manual(values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  labs(title = "Alluvial Diagram for 2017-2018", x = "", fill = "Sierra Club Position")
