library(plyr)

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
output <- filter(csv_total, Disposition != "JRP") %>%
    select(Bill, Year, SC.Position,
                 Com.1, Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, Pass.Governor = Passed,
                ) %>%
          mutate(across(c(Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, Pass.Governor), ~ifelse(is.na(.), 0, .))) %>%
          mutate(SC.Position = SC.Position + 2,
                 Com.1 = as.numeric(factor(if_else(Com.1 == "H-CL" | Com.1 == "H-LC", "H-CL", if_else(Com.1 == "S-F" | Com.1 == "S-FA", "S-F", Com.1))
          )),
                 Dem.Control = if_else(Year < 2020, 0, 1)
          ) %>%
          select(Bill, Year, SC.Position, Dem.Control,
                 Com.1, Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, Pass.Governor
             )
write.csv(output, "combined_2015_2021.csv", row.names = FALSE)
head(output)
