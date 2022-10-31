source("functions.R")
library(lavaan)
library(semPlot)

csv <- read.csv("combined_2015_2021.csv") %>% mutate(Com.1 = factor(Com.1))
head(csv)

cor(csv)
mod <- '
Pass.Com.1 ~  Dem.Control + SC.Position
Pass.Floor.1 ~  Dem.Control + SC.Position + Pass.Com.1 
Pass.Com.2 ~  Dem.Control + SC.Position + Pass.Floor.1
Pass.Floor.2 ~  Dem.Control + SC.Position + Pass.Com.2
Pass.Governor ~  Dem.Control + SC.Position + Pass.Floor.2
'

fit <- cfa(mod, data = csv)
summary(fit)

semPaths(fit, 'std', layout = "tree")
