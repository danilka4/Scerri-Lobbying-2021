source("functions.R")
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% add_identifiers() %>% mutate(Year = 2017, Year_dis = paste(Year, Dis, sep = ""))
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% add_identifiers() %>% mutate(Year = 2018, Year_dis = paste(Year, Dis, sep = ""))
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% add_identifiers() %>% mutate(Year = 2019, Year_dis = paste(Year, Dis, sep = ""))
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% add_identifiers() %>% mutate(Year = 2020, Year_dis = paste(Year, Dis, sep = ""))
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% add_identifiers() %>% mutate(Year = 2021, Year_dis = paste(Year, Dis, sep = ""))

csv_total <- rbind(csv17, csv18, csv19, csv20, csv21)

length(unique(csv_total$Bill)) # Duplicate bill names across years


# Position
mod_law_pos <- glm(Passed ~ I(SC.Position^2), csv_total, family = binomial)
summary(mod_law_pos)
ggplot(csv_total, aes(SC.Position, as.numeric(Passed))) + geom_point() + geom_smooth() + labs(title = "Becomes Law vs Position")
mod_com_pos <- glm(Leaves.Committee ~ SC.Position, csv_total, family = binomial)
summary(mod_com_pos)
ggplot(csv_total, aes(SC.Position, as.numeric(Leaves.Committee))) + geom_point() + geom_smooth() + labs(title = "Leaves Committee vs Position")

ggtotal <- plot_avg(csv_total, "Total", show.legend = FALSE)
gg17 <- plot_avg(csv17, 2017, show.legend = FALSE)
gg18 <- plot_avg(csv18, 2018, show.legend = FALSE)
gg19 <- plot_avg(csv19, 2019, show.legend = FALSE)
gg20 <- plot_avg(csv20, 2020, show.legend = FALSE)
gg21 <- plot_avg(csv21, 2021, show.legend = FALSE)

plot(ggtotal)

ggpubr::ggarrange(gg17, gg18, gg19, gg20, gg21, ggtotal, ncol = 3, nrow = 2)
# Year
mod_law_year <- glm(Passed ~ Year, csv_total, family = binomial)
summary(mod_law_year)
mod_com_year <-  glm(Leaves.Committee ~ Year, csv_total, family = binomial)
summary(mod_com_year)
ggplot(csv_total, aes(Year, as.numeric(Passed))) + geom_point() + geom_smooth()


# Law and Year
summary(glm(Passed ~ Year + SC.Position, csv_total, family = binomial))
summary(glm(Leaves.Committee ~ Year + SC.Position, csv_total, family = binomial))
