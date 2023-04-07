library(plotly)
# OP data manipulation
library(dplyr)
# Used for make_long function that goes a long way in combining everything into a useful format
library(ggsankey)
# Functions to create a tidy df, mostly used to separate committees
library(tidyr)
# Table Library
library(reactable)
source("functions.R")

top_n_table_year <- function(csv) {
    consol_csv <- consolidate_com(csv) %>%
        filter(Com.1 != "Other.Committee") %>%
        group_by(Com.1) %>%
        summarize(
        n = n(),
        Pass.Com.1 = sum(Pass.Com.1, na.rm = TRUE),
        Pass.Floor.1 = sum(Pass.Floor.1, na.rm = TRUE),
        Pass.Com.2 = sum(Pass.Com.2, na.rm = TRUE),
        Pass.Floor.2 = sum(Pass.Floor.2, na.rm = TRUE),
        To.Gov = sum(To.Gov, na.rm = TRUE),
        Law = sum(Passed, na.rm = TRUE)
    ) %>% as.data.frame()
    rownames(consol_csv) <- c(consol_csv$Com.1)
    consol_csv <- select(consol_csv, -Com.1)
    colnames(consol_csv) <- c("Bill Introduced", "Passed Committee 1", "Passed Floor 1", "Passed Committee 2", "Passed Floor 2", "Delivered to Governor", "Signed into Law")
    return(as.data.frame(t(consol_csv)))
}
