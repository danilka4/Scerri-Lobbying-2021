# Performs Chi-Squared tests on the different outcomes
source("functions.R")
library(stringr)

annual_fish <- function(climate, education, year = 2015) {
    cl <- filter(climate, Year == year)
    cl <- filter(cl, Disposition != "JRP") %>%
        mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO"))
    cl <- data.frame(dis = cl$Disposition, type = "Climate")
    e <- filter(education, Year == year) %>%
        mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO"))
    e <- data.frame(dis = e$Disposition, type = "Education")
    comb <- table(rbind(cl, e))
    print(comb)
    print(chisq.test(comb))
    return(comb)
}

manipulate <- function(df, Year){
    return(df %>%
        mutate(Year = Year)  %>%
        separate(Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>%
        separate(Com.2, into = c("Com.2", "Com.2.2", "Com.2.3", "Com.2.4"), sep = ";") %>%
        filter(Disposition != "JRP") %>%
        mutate(
            Com.1 = if_else(Com.1 == "H-CL" | Com.1 == "H-LC" | Com.1 == "H-CE", "H-CL", Com.1),
            Com.1.2 = if_else(Com.1.2 == "H-CL" | Com.1.2 == "H-LC" | Com.1.2 == "H-CE", "H-CL", Com.1.2),
            Com.1.3 = if_else(Com.1.3 == "H-CL" | Com.1.3 == "H-LC" | Com.1.3 == "H-CE", "H-CL", Com.1.3),
            Com.1.4 = if_else(Com.1.4 == "H-CL" | Com.1.4 == "H-LC" | Com.1.4 == "H-CE", "H-CL", Com.1.4),
            Com.2 = if_else(Com.2 == "H-CL" | Com.2 == "H-LC" | Com.2 == "H-CE", "H-CL", Com.2),
            Com.2.2 = if_else(Com.2.2 == "H-CL" | Com.2.2 == "H-LC" | Com.2.2 == "H-CE", "H-CL", Com.2.2),
            Com.2.3 = if_else(Com.2.3 == "H-CL" | Com.2.3 == "H-LC" | Com.2.3 == "H-CE", "H-CL", Com.2.3),
            Com.2.4 = if_else(Com.2.4 == "H-CL" | Com.2.4 == "H-LC" | Com.2.4 == "H-CE", "H-CL", Com.2.4)
        )

    )
}

csv15 <- read.csv("data/csv_2015.csv") %>% col_care() %>% add_identifiers() %>% manipulate(Year=2015) %>% rename(prog = SC.Position)
csv16 <- read.csv("data/csv_2016.csv", nrows = 74) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2016) %>% rename(prog = SC.Position)
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2017) %>% rename(prog = SC.Position)
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2018) %>% rename(prog = SC.Position)
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2019) %>% rename(prog = SC.Position)
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2020) %>% rename(prog = SC.Position)
csv21 <- read.csv("data/csv_2021.csv", nrows = 84) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2021) %>% rename(prog = SC.Position)
csv22 <- read.csv("data/csv_2022.csv", nrows = 132) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2022) %>% rename(prog = SC.Position)
csv23 <- read.csv("data/csv_2023.csv", nrows = 132) %>% col_care() %>% add_identifiers() %>% manipulate(Year = 2023) %>% rename(prog = SC.Position)

csv_total <- rbind(csv15, csv16, csv17, csv18, csv19, csv20, csv21, csv22, csv23) %>%
    mutate(Disposition = if_else(Disposition == "", "INC", str_to_upper(Disposition))) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIH", "DIS"), "DIF", Disposition))

ed15 <- read.csv("data/null_ed_2015.csv") %>% manipulate(Year = 2015) %>% rename(prog = VEA.Support)
ed16 <- read.csv("data/null_ed_2016.csv") %>% manipulate(Year = 2016) %>% rename(prog = VEA.Support)
ed17 <- read.csv("data/null_ed_2017.csv") %>% manipulate(Year = 2017) %>% rename(prog = VEA.Support)
ed18 <- read.csv("data/null_ed_2018.csv") %>% manipulate(Year = 2018) %>% rename(prog = VEA.Support)
ed19 <- read.csv("data/null_ed_2019.csv") %>% manipulate(Year = 2019) %>% rename(prog = VEA.Support)
ed20 <- read.csv("data/null_ed_2020.csv") %>% manipulate(Year = 2020) %>% rename(prog = VEA.Support)
ed21 <- read.csv("data/null_ed_2021.csv") %>% manipulate(Year = 2021) %>% rename(prog = VEA.Support)
ed22 <- read.csv("data/null_ed_2022.csv") %>% manipulate(Year = 2022) %>% rename(prog = VEA.Support)
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
    mutate(dem = year %in% c(2020, 2021))
education <- data.frame(fate = ed_total$Disposition, bill = "Education", year = ed_total$Year)  %>% mutate(fate = if_else(fate %in% c("DIC", "PIL"), fate, "DIO"))  %>%
    mutate(dem = year %in% c(2020, 2021))
chisq.test(table(climate$dem, climate$fate))
chisq.test(table(education$dem, education$fate))
transport <- data.frame(fate = tr_total$Disposition, bill = "Transport") %>% mutate(fate = if_else(fate %in% c("DIC", "PIL"), fate, "DIO")) 
tab_cl_ed <- table(select(rbind(climate, education), bill, fate))
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
    annual_fish(csv_total, ed_total, i)
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

# TODO
for (i in 2015:2022) {
    print(i)
    cl <- filter(ed_total, Disposition != "JRP") %>%
        filter(prog != 0) %>%
        # mutate(prog = prog == 1) %>%
        mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO")) %>%
        filter(Year == i)
    cl_dis_prog <- table(cl$prog, cl$Disposition)
    print(cl_dis_prog)
    print(chisq.test(cl_dis_prog, simulate.p.value = TRUE))
}

# Comparison of progressiveness
ed <- filter(ed_total, Disposition != "JRP") %>%
    filter(prog != 0) %>%
    mutate(prog = prog == 1) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO"))
(ed_dis_prog <- table(ed$prog, ed$Disposition))
chisq.test(ed_dis_prog)

cl <- filter(csv_total, Disposition != "JRP") %>%
    filter(prog != 0) %>%
    # mutate(prog = prog == 1) %>%
    mutate(Disposition = if_else(Disposition %in% c("DIC", "PIL"), Disposition, "DIO"))
(cl_dis_prog <- table(cl$prog, cl$Disposition))
chisq.test(cl_dis_prog)

(clim_ed_prog <- table(rbind(data.frame(type = "Climate", prog = cl$prog), data.frame(type = "Education", prog = ed$prog))))
chisq.test(clim_ed_prog)

(prog_dis <- table(rbind(data.frame(dis = cl$Disposition, prog = cl$prog), data.frame(dis = ed$Disposition, prog = ed$prog))))
chisq.test(prog_dis)

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

# TODO: Make this easier to read
com_comparison <- function(df, list_of_committees) {
    in_pass <- 0
    out_pass <- 0
    in_fail <- 0
    out_fail <- 0
    for (i in seq_len(nrow(df))) {
        row <- df[i, ]

        if (is.na(row["Pass.Com.1"])) {
            if (row["Com.1"] %in% list_of_committees) {
                in_fail <- in_fail + 1
            } else {
                out_fail <- out_fail + 1
            }
            next
        }

        if (row["Pass.Com.1"] == 1) {
            if (row["Com.1"] %in% list_of_committees) {
                in_pass <- in_pass + 1
            } else {
                out_pass <- out_pass + 1
            }
            if (!is.na(row["Com.1.2"])) {
                if (row["Com.1.2"] %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
            if (!is.na(row["Com.1.3"])) {
                if (row["Com.1.3"] %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
            if (!is.na(row["Com.1.4"])) {
                if (row["Com.1.4"] %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
        } else {
            # Doesn't pass committee 1
            if (is.na(row["Com.1.2"])) {
                if (row["Com.1"] %in% list_of_committees) {
                    in_fail  <- in_fail + 1
                    next
                } else {
                    out_fail <- out_fail + 1
                    next
                }
            } else {
                if (row["Com.1"] %in% list_of_committees) {
                    in_pass  <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
                if (is.na(row["Com.1.3"])) {
                    if (row["Com.1.2"] %in% list_of_committees) {
                        in_fail <- in_fail + 1
                        next
                    } else {
                        out_fail <- out_fail + 1
                        next
                    }
                } else {
                    if (row["Com.1.2"] %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                    if (is.na(row["Com.1.4"])) {
                        if (row["Com.1.3"] %in% list_of_committees) {
                            in_fail <- in_fail + 1
                            next
                        } else {
                            out_fail <- out_fail + 1
                            next
                        }
                    } else {
                        if (row["Com.1.3"] %in% list_of_committees) {
                            in_pass <- in_pass + 1
                        } else {
                            out_pass <- out_pass + 1
                        }
                        if (row["Com.1.4"] %in% list_of_committees) {
                            in_fail <- in_fail + 1
                            next
                        } else {
                            out_fail <- out_fail + 1
                            next
                        }
                    }
                }
            }
        }

        if (!is.na(row["Pass.Floor.1"])) {
            if (row["Pass.Floor.1"] == 0) {
                next
            }
        if (row["Pass.Com.2"] == 1) {
            if (row["Com.2"] %in% list_of_committees) {
                in_pass <- in_pass + 1
            } else {
                out_pass <- out_pass + 1
            }
            if (!is.na(row["Com.2.2"])) {
                if (row["Com.2.2"] %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
            if (!is.na(row["Com.2.3"])) {
                if (row["Com.2.3"] %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
            if (!is.na(row["Com.2.4"])) {
                if (row["Com.2.4"] %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
        } else {
            # Doesn't pass committee 1
            if (is.na(row["Com.2.2"])) {
                if (row["Com.2"] %in% list_of_committees) {
                    in_fail  <- in_fail + 1
                        next
                } else {
                    out_fail <- out_fail + 1
                        next
                }
            } else {
                if (row["Com.2"] %in% list_of_committees) {
                    in_pass  <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
                if (is.na(row["Com.2.3"])) {
                    if (row["Com.2.2"] %in% list_of_committees) {
                        in_fail <- in_fail + 1
                            next
                    } else {
                        out_fail <- out_fail + 1
                            next
                    }
                } else {
                    if (row["Com.2.2"] %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                    if (is.na(row["Com.2.4"])) {
                        if (row["Com.2.3"] %in% list_of_committees) {
                            in_fail <- in_fail + 1
                        } else {
                            out_fail <- out_fail + 1
                        }
                    } else {
                        if (row["Com.2.3"] %in% list_of_committees) {
                            in_pass <- in_pass + 1
                        } else {
                            out_pass <- out_pass + 1
                        }
                        if (row["Com.2.4"] %in% list_of_committees) {
                            in_fail <- in_fail + 1
                                next
                        } else {
                            out_fail <- out_fail + 1
                                next
                        }
                    }
                }
            }
        }
        }
    }

    tab <- as.table(rbind(c(in_pass, in_fail), c(out_pass, out_fail)))
    dimnames(tab) <- list(committee = c("in", "out"), pass_fail = c("pass", "fail"))
    # print(tab)
    # print(chisq.test(tab))
    return(c(in_pass, in_fail))
}


com_comparison_readable <- function(df, list_of_committees, print_table=FALSE) {
    in_pass <- 0
    out_pass <- 0
    in_fail <- 0
    out_fail <- 0
    df[df==""]<-NA
    for (i in seq_len(nrow(df))) {
        row <- df[i, ]
        if (row$Disposition %in% c("PIL", "DIF", "V")) {
            if (!is.na(row$Com.1)) {
                if (row$Com.1 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }

            if (!is.na(row$Com.1.2)) {
                if (row$Com.1.2 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }

            if (!is.na(row$Com.1.3)) {
                if (row$Com.1.3 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }

            if (!is.na(row$Com.1.4)) {
                if (row$Com.1.4 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }


            if (!is.na(row$Com.2)) {
                if (row$Com.2 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }

            if (!is.na(row$Com.2.2)) {
                if (row$Com.2.2 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }

            if (!is.na(row$Com.2.3)) {
                if (row$Com.2.3 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }

            if (!is.na(row$Com.2.4)) {
                if (row$Com.2.4 %in% list_of_committees) {
                    in_pass <- in_pass + 1
                } else {
                    out_pass <- out_pass + 1
                }
            }
        } else {
            # Failed somewhere in the committees
            if (!is.na(row$Com.1)) {
                if (is.na(row$Com.1.2) && is.na(row$Com.2)) {
                    if (row$Com.1 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.1 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }

            if (!is.na(row$Com.1.2)) {
                if (is.na(row$Com.1.3) && is.na(row$Com.2)) {
                    if (row$Com.1.2 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.1.2 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }

            if (!is.na(row$Com.1.3)) {
                if (is.na(row$Com.1.4) && is.na(row$Com.2)) {
                    if (row$Com.1.3 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.1.3 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }

            if (!is.na(row$Com.1.4)) {
                if (is.na(row$Com.2)) {
                    if (row$Com.1.4 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.1.4 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }

            if (!is.na(row$Com.2)) {
                if (is.na(row$Com.2.2)) {
                    if (row$Com.2 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.2 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }

            if (!is.na(row$Com.2.2)) {
                if (is.na(row$Com.2.3)) {
                    if (row$Com.2.2 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.2.2 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }
            if (!is.na(row$Com.2.3)) {
                if (is.na(row$Com.2.4)) {
                    if (row$Com.2.3 %in% list_of_committees) {
                        in_fail <- in_fail + 1
                    } else {
                        out_fail <- out_fail + 1
                    }
                } else {
                    if (row$Com.2.3 %in% list_of_committees) {
                        in_pass <- in_pass + 1
                    } else {
                        out_pass <- out_pass + 1
                    }
                }
            }

            if (!is.na(row$Com.2.4)) {
                if (row$Com.2.4 %in% list_of_committees) {
                    in_fail <- in_fail + 1
                } else {
                    out_fail <- out_fail + 1
                }
            }
        }
    }

    tab <- as.table(rbind(c(in_pass, in_fail), c(out_pass, out_fail)))
    dimnames(tab) <- list(committee = c("in", "out"), pass_fail = c("pass", "fail"))
    if (print_table) {
        print(tab)
        print(chisq.test(tab))
    }
    return(c(in_pass, in_fail))
}


com_comparison(ed_total, "H-R")

set_committees <- na.omit(unique(c(csv_total$Com.1, csv_total$Com.1.2, csv_total$Com.1.3, csv_total$Com.1.4, csv_total$Com.2, csv_total$Com.2.2, csv_total$Com.2.3, csv_total$Com.2.4))) %>% .[. != ""]
for (com in set_committees) {
    print(com)
    out <- com_comparison_readable(csv_total, com, TRUE)
    if (out[1] + out[2] > 100) {
        print(c(com, as.character(out[1] / (out[1] + out[2])), '%'))
    }
}


set_committees_ed <- na.omit(unique(c(ed_total$Com.1, ed_total$Com.1.2, ed_total$Com.1.3, ed_total$Com.1.4, ed_total$Com.2, ed_total$Com.2.2, ed_total$Com.2.3, ed_total$Com.2.4))) %>% .[. != ""]
for (com in unique(ed_total$Com.1)) {
        print(com)
        com_comparison_readable(ed_total, com)
}
filter(ed_total, is.na(Pass.Com.2))

# TODO: Create comparison for congresspeople
df <- data.frame(matrix(ncol = 4, nrow = 127))
colnames(df) <- c("year", "committee", "pass", "fail")
i <- 1
for (year in 2015:2022) {
    subset <- filter(csv_total, Year == year)

    set_committees <- na.omit(unique(c(csv_total$Com.1, csv_total$Com.1.2, csv_total$Com.1.3, csv_total$Com.1.4, csv_total$Com.2, csv_total$Com.2.2, csv_total$Com.2.3, csv_total$Com.2.4))) %>% .[. != ""]
    for (com in set_committees) {
        result <- com_comparison_readable(subset, com)
        df[i,] <- c(year, com, result[1], result[2])
        i = i + 1
    }
}
df$year <- as.numeric(as.character(df$year))
df$pass <- as.numeric(as.character(df$pass))
df$fail <- as.numeric(as.character(df$fail))

com_comparison_readable(csv_total, "H-CL", TRUE)

write.csv(df, "com_pass_fail_year.csv", row.names = FALSE)
