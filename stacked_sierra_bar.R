csv<- read.csv("sample/2017b.csv")

library(ggplot2)
library(dplyr)

csv <- csv %>% mutate(Dis = as.factor(-1 * (Disposition == "DiC") + 2 * (Disposition == "PiL")), SC.Position = as.factor(SC.Position))
csv$Dis <- recode(csv$Dis, "-1" = "Died in Committee", "0" = "Died Elsewhere", "2" = "Passed into Law")
csv$SC.Position <- recode(csv$SC.Position, "1" = "Supported", "0" = "Neutral", "-1" = "Opposed")
csv$SC.Position <- factor(csv$SC.Position, levels = c("Supported", "Neutral", "Opposed"))


# Regular Stacked barplot
ggplot(csv, aes(Dis, fill = SC.Position)) + geom_bar() +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  labs(title = "Fate of Different Bills in Relation to Sierra Club's Position",
       x = "Final Outcome of Bill", y = "Number of Bills",
       fill = "Sierra Club Position")

changed_csv <- csv %>% group_by(Dis, SC.Position) %>% summarize(n = n())
# 
ggplot(changed_csv, aes(Dis, n, fill = SC.Position)) + geom_col(position = "fill") +
  scale_fill_manual(
                     values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
  theme_minimal() +
  labs(title = "Proportional Fate of Different Bills in Relation to Sierra Club's Position",
       x = "Final Outcome of Bill", y = "Proportion of Bills",
       fill = "Sierra Club Position")
