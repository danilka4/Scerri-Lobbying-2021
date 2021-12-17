source("functions.R")
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% mutate(Amended = Amended.2) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = "")) %>% mutate(Becomes.Law = Disposition == "PiL", Leaves.Committee = !(Disposition %in% c("DiC", "Inc")))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = "")) %>% mutate(Becomes.Law = Disposition == "PiL", Leaves.Committee = !(Disposition %in% c("DiC", "Inc")))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = "")) %>% mutate(Becomes.Law = Disposition == "PiL", Leaves.Committee = !(Disposition %in% c("DiC", "Inc")))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = "")) %>% mutate(Becomes.Law = Disposition == "PiL", Leaves.Committee = !(Disposition %in% c("DiC", "Inc")))
csv_total <- rbind(csv17, csv18, csv19, csv20)

# Position
mod_law_pos <- glm(Becomes.Law ~ I(SC.Position^2), csv_total, family = binomial)
summary(mod_law_pos)
ggplot(csv_total, aes(SC.Position, as.numeric(Becomes.Law))) + geom_point() + geom_smooth() + labs(title = "Becomes Law vs Position")
mod_com_pos <- glm(Leaves.Committee ~ SC.Position, csv_total, family = binomial)
summary(mod_com_pos)
ggplot(csv_total, aes(SC.Position, as.numeric(Leaves.Committee))) + geom_point() + geom_smooth() + labs(title = "Leaves Committee vs Position")

ggtotal <- plot_avg(csv_total, "Total")
gg17 <- plot_avg(csv17, 2017, show.legend = FALSE)
gg18 <- plot_avg(csv18, 2018, show.legend = FALSE)
gg19 <- plot_avg(csv19, 2019, show.legend = FALSE)
gg20 <- plot_avg(csv20, 2020)

ggpubr::ggarrange(gg17, gg18, gg19, gg20, ncol = 2, nrow = 2)
# Year
mod_law_year <- glm(Becomes.Law ~ Year, csv_total, family = binomial)
summary(mod_law_year)
mod_com_year <-  glm(Leaves.Committee ~ Year, csv_total, family = binomial)
summary(mod_com_year)
ggplot(csv_total, aes(Year, as.numeric(Becomes.Law))) + geom_point() + geom_smooth()


# Law and Year
summary(glm(Becomes.Law ~ Year + SC.Position, csv_total, family = binomial))
summary(glm(Leaves.Committee ~ Year + SC.Position, csv_total, family = binomial))
