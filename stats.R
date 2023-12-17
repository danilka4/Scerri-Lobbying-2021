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
    print(chisq.test(comb, simulate.p.value = TRUE, B=99999))
    return(comb)
}

csv15 <- read.csv("data/csv_2015.csv") %>% col_care() %>% add_identifiers() %>% mutate(Year = 2015) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv16 <- read.csv("data/csv_2016.csv", nrows = 74) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2016) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2017) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2018) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2019) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2020) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv21 <- read.csv("data/csv_2021.csv", nrows = 84) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2021) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)
csv22 <- read.csv("data/csv_2022.csv", nrows = 132) %>% col_care() %>% add_identifiers() %>% mutate(Year = 2022) %>%
    separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = SC.Position)

csv_total <- rbind(csv15, csv16, csv17, csv18, csv19, csv20, csv21, csv22) %>%
    mutate(Disposition = if_else(Disposition == "", "INC", str_to_upper(Disposition))) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIH", "DIS"), "DIF", Disposition))

ed15 <- read.csv("data/null_ed_2015.csv") %>% mutate(Year = 2015) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed16 <- read.csv("data/null_ed_2016.csv") %>% mutate(Year = 2016) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed17 <- read.csv("data/null_ed_2017.csv") %>% mutate(Year = 2017) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed18 <- read.csv("data/null_ed_2018.csv") %>% mutate(Year = 2018) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed19 <- read.csv("data/null_ed_2019.csv") %>% mutate(Year = 2019) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed20 <- read.csv("data/null_ed_2020.csv") %>% mutate(Year = 2020) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed21 <- read.csv("data/null_ed_2021.csv") %>% mutate(Year = 2021) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
ed22 <- read.csv("data/null_ed_2022.csv") %>% mutate(Year = 2022) %>% separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>% rename(prog = VEA.Support)
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



annual_fish(csv_total, ed_total, tr_total)

head(climate)

climate <- data.frame(fate = csv_total$Disposition[csv_total$Disposition != "JRP"], bill = "Climate", year = csv_total$Year[csv_total$Disposition != "JRP"])  %>% mutate(fate = if_else(fate %in% c("DIC", "PIL"), fate, "DIO"))  %>%
    mutate(dem = year %in% c(2020, 2021)) %>%
    filter(fate %in% c("DIC", "PIL"))
education <- data.frame(fate = ed_total$Disposition, bill = "Education", year = ed_total$Year)  %>% mutate(fate = if_else(fate %in% c("DIC", "PIL"), fate, "DIO"))  %>%
    mutate(dem = year %in% c(2020, 2021)) %>%
    filter(fate %in% c("DIC", "PIL"))
chisq.test(table(education$dem, education$fate))
transport <- data.frame(fate = tr_total$Disposition, bill = "Transport") %>% mutate(fate = if_else(fate %in% c("DIC", "PIL"), fate, "DIO")) 
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
    annual_fish(csv_total %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")), ed_total %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")), tr_total  %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")), i)
}
for (i in 2015:2022) {
    print(i)
    annual_fish(csv_total, ed_total, tr_total, i)
}
head(csv_total)
chisq.test(comb)
(chisq.test(comb, simulate.p.value = TRUE, B = 99999))


# Climate committees
com_comparison <- function(list_of_committees) {
    com <- mutate(csv_total, hcl = Com.1 %in% list_of_committees) %>% filter(Disposition != "JRP") %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")) %>% select(hcl, Disposition)
    tab <- table(com)
    print(tab)
    chisq.test(tab, B=99999)
}
com_comparison(c("H-CL", "H-LC", "H-CE"))
com_comparison(c("H-CL", "H-LC", "H-CE", "S-CL"))
com_comparison(c("H-ACNR", "S-ACNR"))

for (c in unique(csv_total$Com.1)) {
    com <- mutate(csv_total, hcl = Com.1 == c) %>% filter(Disposition != "JRP") %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")) %>% select(hcl, Disposition) %>% mutate(Disposition = Disposition == "DIC")
    tab <- table(com)
    print(c)
    print(tab)
    print(chisq.test(tab, B=99999))
}

# Comparison of progressiveness
ed <- filter(ed_total, Disposition != "JRP") %>%
    #filter(prog != 0) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO"))

cl <- filter(csv_total, Disposition != "JRP") %>%
    filter(prog != 0) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO"))
(cl_dis_prog <- table(cl$prog, cl$Disposition))
chisq.test(cl_dis_prog)
(ed_dis_prog <- table(ed$prog, ed$Disposition))
chisq.test(ed_dis_prog)

(clim_ed_prog <- table(rbind(data.frame(type = "Climate", prog = cl$prog), data.frame(type = "Education", prog = ed$prog))))
chisq.test(clim_ed_prog)

ed_com_comparison <- function(list_of_committees) {
    com <- mutate(ed_total, hcl = Com.1 %in% list_of_committees) %>% filter(Disposition != "JRP") %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")) %>% select(hcl, Disposition)
    tab <- table(com)
    print(tab)
    chisq.test(tab, B=99999)
}
ed_com_comparison("H-E")
for (c in unique(ed_total$Com.1)) {
    com <- mutate(ed_total, hcl = Com.1 == c) %>% filter(Disposition != "JRP") %>% mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")) %>% select(hcl, Disposition) %>% mutate(Disposition = Disposition == "DIC")
    tab <- table(com)
    print(c)
    print(tab)
    print(chisq.test(tab, B=99999))
}
