source("functions.R")
csv17 <- read.csv("data/csv_2017.csv", nrows = 89) %>% add_identifiers() %>% mutate(Year = 2017)
csv18 <- read.csv("data/csv_2018.csv", nrows = 111) %>% add_identifiers() %>% mutate(Year = 2018)
csv19 <- read.csv("data/csv_2019.csv", nrows = 92) %>% add_identifiers() %>% mutate(Year = 2019)
csv20 <- read.csv("data/csv_2020.csv", nrows = 92) %>% add_identifiers() %>% mutate(Year = 2020)
csv21 <- read.csv("data/csv_2021.csv", nrows = 92) %>% add_identifiers() %>% mutate(Year = 2021)

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

unique(csv_total$Com.2)

head(csv_total)
com <- remove_extra_com(csv17)

table(com$Com.1, com$Pos)

dim(com)
filter(csv_total, Com.1 == "NA")
View(unique(com$Committee))

mutate(com, Combined = factor(Com.1)) %>%
    ggplot(aes(Combined, fill = Pos)) +
    geom_histogram(stat = "count") + 
    scale_fill_manual(
                       values = c("Supported" = "#00ba38", "Neutral" = "#619cff", "Opposed" = "#f8766d")) +
    theme_minimal() +
    labs(title = "Bills Passed Per Committee",
         x = "Congressional Committee", y = "Number of Bills",
         fill = "Sierra Club Position") + guides(fill = "none")
# H-CL 19, H-ACNR 16, S-CL 15, S-ACNR 8, S-F 6, H-F 5
# H-CL 24, S-CL 19, H-ACNR 7, H-PE 5, H-R 5
# H-CL 24, H-ACNR 16, S-CL 10, H-R 6, H-CCT 5
# H-LC 13, S-CL 11, H-ACNR 11, H-F 5, S-GLT 5
# H-LC 16, H-PE 12, H-ACNR 7, S-ACNR 7, S-PE 6, H-R 5
# Going to try to have all the committees with 10+ bills as separate

memes <- remove_extra_com(csv_total) %>% consolidate_com()

test <- com_creator(csv17, "black")
head(test, n = 20)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    color = "gray",
    pad = 10), 
  link = list(
    source = as.numeric(meme$x) - 1,
    target = as.numeric(meme$next_x) - 1,
    value = meme$n,
    color = ~as.factor("black"),
    line = list(color = "black", width = 0.5)
    ))%>%
  layout(title = "Annual Sankey with Joint Resolutions 2017-2021",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

meme <- csv17 %>%
    mutate(Intro.Com = 1, Law = Passed) %>%
    remove_extra_com() %>% consolidate_com() %>%
    make_long(
              Intro.Com,
              Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2,
              To.Gov,
              Passed,
              Law
    ) %>%
na_if(0) %>%
filter(!is.na(node))

ggplot(meme, aes(x = x, next_x = next_x,
                 node = node, next_node = next_node,
                 label = node)) + 
geom_sankey() + 
  geom_sankey_label(size = 3, color = "white", fill = "gray40")
