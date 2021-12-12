library(plotly)
library(dplyr)
library(ggsankey)


data_creator_dead <- function(csv, color_id, color_black = TRUE) {
  a <- mutate(csv, Intro.Com = 1, Law = Signed.by.Gov)
  normal_df <- a %>% 
    make_long(Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2,
              To.Gov,
              Signed.by.Gov,
              Law) %>% 
    na_if(0) %>% 
    filter(!is.na(node))
  
  label <- levels(normal_df$x)
  
  new_total_df <- normal_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n()) %>% 
    mutate(color = color_id) %>% filter(!is.na(next_x))
  dead <- (new_total_df$n - lead(new_total_df$n))[1:6]
  new_total_df <- rbind(
    new_total_df, 
    data.frame(
      x = as.factor(c("Pass.Com.1", "Pass.Floor.1", "Pass.Com.2", "Pass.Floor.2", "To.Gov", "Signed.by.Gov")), 
      next_x = as.factor(rep("Dead", 6)), 
      n = dead, 
      color = color_id))
  if (color_black) {
    new_total_df <- color_dead(new_total_df)
  }
  return(new_total_df)
}

sierra_data_dead <- function(csv, color_black = TRUE) {
  supported <- filter(csv, SC.Position == 1)
  neutral <- filter(csv, SC.Position == 0)
  opposed <- filter(csv, SC.Position == -1)
  supported_df <- data_creator_dead(supported, "rgba(154,205,50,1.0)", color_black)
  neutral_df <- data_creator_dead(neutral, "rgba(176,224,230,1.0)", color_black)
  opposed_df <- data_creator_dead(opposed, "rgba(255,69,0,1.0)", color_black)
  output <- rbind(supported_df, neutral_df, opposed_df)
  if (color_black) {
    output <- color_dead(output)
  }
  return(output)
}

color_dead <- function(df) {
  non_dead <- filter(df, next_x != "Dead")
  dead <- filter(df, next_x == "Dead")
  dead_comb <- group_by(dead, x, next_x) %>% 
    summarise(n = sum(n), color = "black")
  return(rbind(non_dead, dead_comb))
}
