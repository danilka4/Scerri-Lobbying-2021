no_return_sierra_data <- function(csv, shell = FALSE) {
  supported <- filter(csv, SC.Position == 1)
  neutral <- filter(csv, SC.Position == 0)
  opposed <- filter(csv, SC.Position == -1)
  supported_df <- no_return_data_creator(supported, "rgba(154,205,50,1.0)")
  neutral_df <- no_return_data_creator(neutral, "rgba(176,224,230,1.0)")
  opposed_df <- no_return_data_creator(opposed, "rgba(255,69,0,1.0)")
  if (shell) {
    opposed_df <- rbind(opposed_df, data.frame(x = as.factor(c("Pass.Floor.2", "To.Gov", "To.Gov", "Pass.Floor.1", "Pass.Floor.2")),
                                                   next_x = as.factor(c("Pass.Floor.1", "Pass.Floor.1", "Pass.Floor.2","Signed.by.Gov", "Signed.by.Gov")),
                                                   n = 1, color = "gray"))
  }
  return(rbind(supported_df, neutral_df, opposed_df))
}

no_return_data_creator <- function(csv, color_id, shell = FALSE) {
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
    filter(!is.na(next_x)) %>% 
    mutate(color = color_id)
  if (shell) {
    new_total_df <- rbind(new_total_df, data.frame(x = as.factor(c("Pass.Floor.2", "To.Gov", "To.Gov", "Pass.Floor.1", "Pass.Floor.2")),
                                                   next_x = as.factor(c("Pass.Floor.1", "Pass.Floor.1", "Pass.Floor.2","Signed.by.Gov", "Signed.by.Gov")),
                                                   n = 1, color = "gray"))
  }
  return(new_total_df)
  
}
