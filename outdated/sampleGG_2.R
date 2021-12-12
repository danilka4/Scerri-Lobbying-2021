# Going to try to add behavior for bills that get amended/returned
a <- read.csv("sample/2017a.csv")

library(dplyr)
library(ggsankey)
library(ggplot2)

# First lets separate all the different scenarios
## Scenario 1: it's a normal bill
a <- mutate(a, Intro.Com = 1)
normal_a <- a %>% filter(is.na(Returned) | Returned == 0) %>% 
  filter(is.na(Returned.1) | Returned.1 == 0) 
normal_a[,c("Returned", "Returned.1")]
normal_a[,"Amd.Sub"]

# Separate all the data with the initial return
one_a <- a %>% filter(Returned == 1)
one_a[,c("Returned", "Returned.1")]

# Separate all the data with the second return
two_a <- a %>% filter(Returned.1 == 1)
two_a[,c("Returned", "Returned.1")]

# Now to turn the data into something that can be read easily
normal_df <- normal_a %>% 
  make_long(Intro.Com,
            Pass.Com,
            Pass.Floor,
            Pass.Com.1,
            Passed.Floor.2,
            To.Gov,
            Signed.by.Gov) %>% 
  na_if(0) %>% 
  filter(!is.na(node)) 

one_df <- rbind(
  make_long(one_a,
            Intro.Com,
            Pass.Com,
            Pass.Floor,
            Pass.Com.1,
            Passed.Floor.2) %>% filter(!is.na(next_node)),
  make_long(one_a,
            Passed.Floor.2,
            Pass.Floor,
            To.Gov,
            Signed.by.Gov)) %>% 
  na_if(0) %>% 
  filter(!is.na(node)) 

two_df <- rbind(
  make_long(two_a,
            Intro.Com,
            Pass.Com,
            Pass.Floor,
            Pass.Com.1,
            Passed.Floor.2, 
            To.Gov) %>% filter(!is.na(next_node)),
  make_long(two_a,
            To.Gov,
            Pass.Floor,
            Passed.Floor.2,
            Signed.by.Gov)) %>% 
  na_if(0) %>% 
  filter(!is.na(node)) 


total_df <- rbind(normal_df, one_df, two_df)
ggplot(total_df, aes(x = x,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(x),
               label = x)) +
  geom_sankey(flow_alpha = 0.5, node.color = 1, width = 0.3, smooth = 10, node.color = 1) + 
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none") + 
  xlab(NULL)
