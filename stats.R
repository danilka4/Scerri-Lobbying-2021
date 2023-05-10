# Performs Chi-Squared tests on the different outcomes
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
csv22 <- read.csv("data/csv_2022.csv", nrows = 132) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2022) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")

csv_total <- rbind(csv15, csv16, csv17, csv18, csv19, csv20, csv21, csv22) %>%
    mutate(Disposition = if_else(Disposition == "", "Inc", Disposition))

ed15 <- read.csv("data/null_ed_2015.csv") %>% mutate(Year = 2015) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed16 <- read.csv("data/null_ed_2016.csv") %>% mutate(Year = 2016) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed17 <- read.csv("data/null_ed_2017.csv") %>% mutate(Year = 2017) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed18 <- read.csv("data/null_ed_2018.csv") %>% mutate(Year = 2018) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed19 <- read.csv("data/null_ed_2019.csv") %>% mutate(Year = 2019) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed20 <- read.csv("data/null_ed_2020.csv") %>% mutate(Year = 2020) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed21 <- read.csv("data/null_ed_2021.csv") %>% mutate(Year = 2021) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed22 <- read.csv("data/null_ed_2022.csv") %>% mutate(Year = 2022) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed_total <- rbind(ed15, ed16, ed17, ed18, ed19, ed20, ed21, ed22) %>%
    mutate(Disposition = if_else(Disposition == "", "Inc", Disposition))


climate <- csv_total$Disposition[csv_total$Disposition != "JRP"]
education <- ed_total$Disposition
tab <- rbind(climate = table(climate), education = table(education))

chisq.test(tab)
