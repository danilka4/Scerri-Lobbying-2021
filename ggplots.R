source("functions.R")

csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = ""))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = ""))
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021, Year_dis = paste(Year, Dis, sep = ""))
csv_total <- rbind(csv17, csv18, csv19, csv20)

colors <- list("y2017" = "ef476f", "y2018" = "ffd166", "y2019" = "06d6a0", "y2020" = "118ab2", "y2021" = "073b4c",
               "dead" = "6a5d5d")

labels <- c("DiC", "DE", "PiL")
labels <- c("Died in Committee", "Died Elsewhere", "Passed into Law")
gg17 <- ggplot(csv17, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(breaks = c("Died in Committee","Died Elsewhere","Passed into Law"),labels = labels) + 
  labs(title = "Fate of Different Bills in Relation to Sierra Club's Position between 2017 and 2021",
       x = "Final Outcome of Bill", y = "Number of Bills",
       fill = "Sierra Club Position") + guides(fill = "none")

gg18 <- ggplot(csv18, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(breaks = c("Died in Committee","Died Elsewhere","Passed into Law"),labels = labels) + 
  labs(title = "",
       x = "", y = "",
       fill = "Sierra Club Position") + guides(fill = "none")

gg19 <- ggplot(csv19, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(breaks = c("Died in Committee","Died Elsewhere","Passed into Law"),labels = labels) + 
  labs(title = "",
       x = "", y = "",
       fill = "Sierra Club Position") + guides(fill = "none")
gg20 <- ggplot(csv20, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(breaks = c("Died in Committee","Died Elsewhere","Passed into Law"),labels = labels) + 
  labs(title = "",
       x = "", y = "",
       fill = "Sierra Club Position") + guides(fill = "none")
gg21 <- ggplot(csv21, aes(Dis, fill = Pos)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  scale_x_discrete(breaks = c("Died in Committee","Died Elsewhere","Passed into Law"),labels = labels) + 
  labs(title = "",
       x = "", y = "",
       fill = "Sierra Club Position") + guides(fill = "none")

ggpubr::ggarrange(gg17, gg18, gg19, gg20, gg21, ncol = 3, nrow = 2)

changed_csv <- csv_total %>% group_by(Year_dis, Pos) %>% summarize(n = n())
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
all <- ggplot(alluvial_data, aes(x = x,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_alluvial(flow_alpha = 0.5, node.color = 1, node.fill = "gray") + 
  geom_alluvial_label(size = 3.5, color = 1, fill = "white") +
  theme_alluvial(base_size = 16) + 
  scale_x_discrete(labels = c("Position", "Fate")) +
  scale_fill_manual(values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  labs(title = "Alluvial Diagram for 2017-2021", x = "", fill = "Sierra Club Position")
plot(all)

ggpubr::ggarrange(gg17, gg18, gg19, gg20, gg21,  ncol = 3, nrow = 2)

