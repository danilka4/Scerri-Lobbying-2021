library(plotly)
library(cowplot)
source("functions.R")

csv15 <- read.csv("data/csv_2015.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2015) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv16 <- read.csv("data/csv_2016.csv", nrows = 74) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2016) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv21 <- read.csv("data/csv_2021.csv", nrows = 84) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")

csv_total <- rbind(csv15, csv16, csv17, csv18, csv19, csv20, csv21)

politicians <- read.csv("data/politicians.csv")
com_pol <- read.csv("data/committee_members.csv")
swing <- read.csv("data/swing.csv") %>%
    mutate(Swing = 1)

comb <- left_join(com_pol, politicians, by = "Name") %>%
    select(Year = Session, Committee, Name, District) %>%
    left_join(swing, by = c("District", "Year")) %>%
    mutate(Swing = coalesce(Swing, 0))
rats <- filter(comb, Year %in% c(2017)) %>%
    group_by(Year, Committee) %>%
    summarize(SwingMembers = sum(Swing),
    TotalMembers = n(),
    SwingProp = mean(Swing))
filter(rats, SwingProp == 1)
bills <- filter(csv_total, Year %in% c(2017)) %>%
    select(Bill, Year, Com.1, Pass.Com.1, Com.2, Pass.Com.2) %>%
    group_by(Year, Com.1) %>%
    summarize(TotalPassed = sum(Pass.Com.1), PropPassed = mean(Pass.Com.1))
head(bills)
completely_combined <- right_join(rats, bills, by = c("Year", "Committee" = "Com.1"))
summary(completely_combined)
completely_combined
mod <- lm(PropPassed ~ SwingProp, completely_combined, weights = TotalPassed)
summary(mod)
ggplot(completely_combined, aes(SwingProp, PropPassed)) +
    geom_point(aes(size = log(TotalPassed)), alpha = 0.5) +
    geom_smooth(method=lm) +
    theme_minimal() +
    labs(size = "Committee Importance",
    x = "Portion Committee in Swing District",
    y = "Portion Bills Passed",
    title = "Do Swing Districts Affect Bills Passing?")
ggsave("swing_bill.png", bg = "white")
completely_combined_h <- filter(completely_combined, substr(Committee, 1, 1) == "H")
summary(completely_combined_h)
completely_combined_h
modh <- lm(PropPassed ~ SwingProp, completely_combined_h, weights = TotalPassed)
summary(modh)
completely_combined_s <- filter(completely_combined, substr(Committee, 1, 1) == "S")
summary(completely_combined_s)
completely_combined_s
mods <- lm(PropPassed ~ SwingProp, completely_combined_s, weights = TotalPassed)
summary(mods)
