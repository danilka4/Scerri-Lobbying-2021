library(plotly)
library(dplyr)
library(ggsankey)
# First lets separate all the different scenarios
## Scenario 1: it's a normal bill
a <- read.csv("sample/2017a.csv")
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

# Now to turn the data into something that can be read by the ggsankey
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
label <- levels(total_df$x)
new_total_df <- total_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n())
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = label,
    x = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8),
    y = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.5, 0.3),
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(new_total_df$x) - 1,
    target = as.numeric(new_total_df$next_x) - 1,
    value = new_total_df$n/10))%>% layout(title = "Sankey Sample Diagram with Plotly")
