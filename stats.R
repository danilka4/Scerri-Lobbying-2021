# Performs Chi-Squared tests on the different outcomes
source("functions.R")
library(stringr)

annual_fish <- function(climate, education, transport, year = 2015) {
    cl <- filter(climate, Year == year)
    cl <- filter(cl, Disposition != "JRP")
    cl <- data.frame(dis = cl$Disposition, type = "Climate")
    e <- filter(education, Year == year)
    e <- data.frame(dis = e$Disposition, type = "Education")
    t <- filter(transport, Year == year)
    t <- data.frame(dis = t$Disposition, type = "Transportation")
    comb <- table(rbind(cl, e, t))
    print(comb)
    print(chisq.test(comb, simulate.p.value = TRUE, B = 99999))
    return(comb)
}

annual_fish(csv_total, ed_total, tr_total)

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
    mutate(Disposition = if_else(Disposition == "", "INC", str_to_upper(Disposition))) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIH", "DIS"), "DIF", Disposition))

ed15 <- read.csv("data/null_ed_2015.csv") %>% mutate(Year = 2015) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed16 <- read.csv("data/null_ed_2016.csv") %>% mutate(Year = 2016) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed17 <- read.csv("data/null_ed_2017.csv") %>% mutate(Year = 2017) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed18 <- read.csv("data/null_ed_2018.csv") %>% mutate(Year = 2018) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed19 <- read.csv("data/null_ed_2019.csv") %>% mutate(Year = 2019) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed20 <- read.csv("data/null_ed_2020.csv") %>% mutate(Year = 2020) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed21 <- read.csv("data/null_ed_2021.csv") %>% mutate(Year = 2021) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed22 <- read.csv("data/null_ed_2022.csv") %>% mutate(Year = 2022) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
ed_total <- rbind(ed15, ed16, ed17, ed18, ed19, ed20, ed21, ed22) %>%
    mutate(Disposition = if_else(Disposition == "", "INC", str_to_upper(Disposition))) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIH", "DIS"), "DIF", Disposition))

tr15 <- read.csv("data/null_tr_2015.csv") %>% mutate(Year = 2015) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr16 <- read.csv("data/null_tr_2016.csv") %>% mutate(Year = 2016) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr17 <- read.csv("data/null_tr_2017.csv") %>% mutate(Year = 2017) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr18 <- read.csv("data/null_tr_2018.csv") %>% mutate(Year = 2018) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr19 <- read.csv("data/null_tr_2019.csv") %>% mutate(Year = 2019) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr20 <- read.csv("data/null_tr_2020.csv") %>% mutate(Year = 2020) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr21 <- read.csv("data/null_tr_2021.csv") %>% mutate(Year = 2021) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr22 <- read.csv("data/null_tr_2022.csv") %>% mutate(Year = 2022) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
tr_total <- rbind(tr15, tr16, tr17, tr18, tr19, tr20, tr21, tr22) %>%
    mutate(Disposition = if_else(Disposition %in% c("", "I"), "INC", str_to_upper(Disposition))) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIH", "DIS"), "DIF", Disposition))

table(tr_total$Disposition)

climate <- data.frame(fate = csv_total$Disposition[csv_total$Disposition != "JRP"], bill = "Climate")
education <- data.frame(fate = ed_total$Disposition, bill = "Education")
transport <- data.frame(fate = tr_total$Disposition, bill = "Transport")
tab_cl_ed <- table(rbind(climate, education))
tab_cl_tr <- table(rbind(climate, transport))
tab_ed_tr <- table(rbind(education, transport))
comb <- table(rbind(climate, education, transport))

chisq.test(tab_cl_ed)
chisq.test(tab_cl_tr)
chisq.test(tab_ed_tr)
chisq.test(comb)
comb

for (i in 2015:2022) {
    print(i)
    annual_fish(csv_total, ed_total, tr_total, i)
}
chisq.test(comb)
(chisq.test(comb, simulate.p.value = TRUE, B = 99999))
