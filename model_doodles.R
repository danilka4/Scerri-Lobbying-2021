source("functions.R")
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% mutate(Amended = Amended.2) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = "")) %>% mutate(Becomes.Law = Disposition == "PiL", Leaves.Committee = Disposition != "DiC")
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = "")) %>% mutate(Becomes.Law = Disposition == "PiL", Leaves.Committee = Disposition != "DiC")
csv_total <- rbind(csv17, csv18)

# Position
mod_law_pos <- glm(Becomes.Law ~ I(SC.Position^2), csv_total, family = binomial)
summary(mod_law_pos)
ggplot(csv_total, aes(SC.Position, as.numeric(Becomes.Law))) + geom_point() + geom_smooth() + labs(title = "Becomes Law vs Position")
mod_com_pos <- glm(Leaves.Committee ~ SC.Position, csv_total, family = binomial)
summary(mod_com_pos)
ggplot(csv_total, aes(SC.Position, as.numeric(Leaves.Committee))) + geom_point() + geom_smooth() + labs(title = "Leaves Committee vs Position")
ggplot(csv_total) +
  geom_smooth(aes(SC.Position, as.numeric(Leaves.Committee), color = "1"), se = F) +
  geom_smooth(aes(SC.Position, as.numeric(Becomes.Law), color = "2"), se = F) + 
  theme_minimal() +
  labs(title = "Bills that Pass Key Milestones Based on SC Position in 2017-2018", x = "SC Position", y = "Portion") + 
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.5, 0.1)) + 
  scale_color_manual(name = "Portion of Bills that", labels = c("Leave Committee","Become Law"), values = c("orange", "green"))

ggplot(csv17) +
  geom_smooth(aes(SC.Position, as.numeric(Leaves.Committee), color = "1"), se = F) +
  geom_smooth(aes(SC.Position, as.numeric(Becomes.Law), color = "2"), se = F) + 
  theme_minimal() +
  labs(title = "Bills that Pass Key Milestones Based on SC Position in 2017", x = "SC Position", y = "Portion") + 
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.5, 0.1)) + 
  scale_color_manual(name = "Portion of Bills that", labels = c("Leave Committee","Become Law"), values = c("orange", "green"))

ggplot(csv18) +
  geom_point(aes(SC.Position, as.numeric(Leaves.Committee))) +
  geom_smooth(aes(SC.Position, as.numeric(Leaves.Committee), color = "1"), se = F) +
  geom_smooth(aes(SC.Position, as.numeric(Becomes.Law), color = "2"), se = F) + 
  theme_minimal() +
  labs(title = "Bills that Pass Key Milestones Based on SC Position in 2018", x = "SC Position", y = "Portion") + 
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 0.5, 0.1)) + 
  scale_color_manual(name = "Portion of Bills that", labels = c("Leave Committee","Become Law"), values = c("orange", "green"))

# Year
mod_law_year <- glm(Becomes.Law ~ Year, csv_total, family = binomial)
summary(mod_law_year)
mod_com_year <-  glm(Leaves.Committee ~ Year, csv_total, family = binomial)
summary(mod_com_year)
ggplot(csv_total, aes(Year, as.numeric(Becomes.Law))) + geom_point() + geom_smooth()


# Law and Year
summary(glm(Becomes.Law ~ Year + SC.Position, csv_total, family = binomial))
summary(glm(Leaves.Committee ~ Year + SC.Position, csv_total, family = binomial))
