a <- read.csv("sample/2017a.csv")
(a[,4:(ncol(a) - 1)])

(a_interest <- a[, -c(1,2,3,8,9,16)])

library(dplyr)
library(ggplot2)
library(ggsankey)

df <- a_interest %>% mutate(Intro.Com = 1) %>%
  make_long(Intro.Com,
            Pass.Com,
#            Amd.Sub,
            Pass.Floor,
            Pass.Com.1,
            Passed.Floor.2,
#            Returned,
            To.Gov,
#            Returned.1,
            Signed.by.Gov) %>% 
  na_if(0) %>% 
  filter(!is.na(node)) 
ggplot(df, aes(x = x,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(x),
               label = x)) +
  geom_sankey(flow_alpha = 0.5, node.color = 1, width = 0.3) + 
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none") + 
  xlab(NULL)

ggplot(a, aes(x = Disposition)) + geom_bar(fill = "red", color = "green", size = 0.3) + theme_minimal()+
  labs(title = "Barplot of Each of Piece of Legislature's Fate")

sum(a$Returned, na.rm = T)
sum(a$Returned.1, na.rm = T)

