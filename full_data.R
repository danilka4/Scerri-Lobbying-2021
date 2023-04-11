library(plyr)
library(dplyr)

source("functions.R")

csv15 <- read.csv("data/csv_2015.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2015) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv16 <- read.csv("data/csv_2016.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2016) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv17 <- read.csv("data/csv_2017.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv18 <- read.csv("data/csv_2018.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv19 <- read.csv("data/csv_2019.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv20 <- read.csv("data/csv_2020.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv21 <- read.csv("data/csv_2021.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
csv22 <- read.csv("data/csv_2022.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2022) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")

csv_total <- rbind(csv15, csv16, csv17, csv18, csv19, csv20, csv21, csv22)
output <- filter(csv_total, Disposition != "JRP") %>%
    select(Bill, Year, SC.Position, Com.2,
                 Com.1, Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, Pass.Governor = Passed,
                ) %>%
          mutate(across(c(Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, Pass.Governor), ~ifelse(is.na(.), 0, .))) %>%
          mutate(SC.Position = SC.Position + 2,
                 Com.1 = as.numeric(factor(if_else(Com.1 == "H-CL" | Com.1 == "H-LC" | Com.1 == "H-CE", "H-CL", if_else(Com.1 == "S-F" | Com.1 == "S-FA", "S-F", Com.1)))),
                 Com.2 = as.numeric(factor(if_else(Com.2 == "H-CL" | Com.2 == "H-LC" | Com.2 == "H-CE", "H-CL", if_else(Com.2 == "S-F" | Com.2 == "S-FA", "S-F", Com.2))))
          ,
                 Dem.House = as.numeric(Year %in% 2020:2021),
                 Dem.Senate = as.numeric(Year %in% 2020:2022)
          ) %>%
          select(Bill, Year, SC.Position, Dem.House, Dem.Senate,
                 Com.1, Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, Pass.Governor
             )
write.csv(output, "combined_2015_2022.csv", row.names = FALSE)
head(output)
