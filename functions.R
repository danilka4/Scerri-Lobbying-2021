library(plotly)
library(dplyr)
library(ggsankey)

data_creator <- function (csv, color_id, include_joint = TRUE) {
    csv <- mutate(csv, Intro.Com = 1, Law = Passed)
    normal <- filter(csv, Disposition != "JRP")
    jrps <- filter(csv, Disposition == "JRP")
    normal_df <- normal %>% 
        make_long(Intro.Com,
            Pass.Com.1,
            Pass.Floor.1,
            Pass.Com.2,
            Pass.Floor.2,
            To.Gov,
            Passed,
            Law
            ) %>%
        na_if(0) %>%
        filter(!is.na(node))
    jrps_df <- jrps %>%
        make_long(Intro.Com,
            Pass.Com.1,
            Pass.Floor.1,
            Pass.Com.2,
            Pass.Floor.2,
            Passed,
            Law
            ) %>%
        na_if(0) %>%
        filter(!is.na(node))
    if (include_joint) {
        normal_df <- rbind(normal_df, jrps_df)
    }
    label <- levels(normal_df$x)
    new_normal <- normal_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n()) %>%
        filter(!is.na(next_x)) %>% 
        mutate(color = color_id)
    return(new_normal)
}

sierra_data <- function(csv, include_joint = TRUE) {
  supported <- filter(csv, SC.Position == 1)
  neutral <- filter(csv, SC.Position == 0)
  opposed <- filter(csv, SC.Position == -1)
  supported_df <- data_creator(supported, "rgba(154,205,50,1.0)", include_joint)
  neutral_df <- data_creator(neutral, "rgba(176,224,230,1.0)", include_joint)
  opposed_df <- data_creator(opposed, "rgba(255,69,0,1.0)", include_joint)
  return(rbind(supported_df, neutral_df, opposed_df))
}







data_creator_dead <- function(csv, color_id, color_black = TRUE, include_joint = TRUE) {
    csv <- mutate(csv, Intro.Com = 1, Law = Passed)
    normal <- filter(csv, Disposition != "JRP")
    jrps <- filter(csv, Disposition == "JRP")
    normal_df <- normal %>% 
        make_long(Intro.Com,
            Pass.Com.1,
            Pass.Floor.1,
            Pass.Com.2,
            Pass.Floor.2,
            To.Gov,
            Passed,
            Law
            ) %>%
        na_if(0) %>%
        filter(!is.na(node))
    jrps_df <- jrps %>%
        make_long(Intro.Com,
            Pass.Com.1,
            Pass.Floor.1,
            Pass.Com.2,
            Pass.Floor.2,
            Passed,
            Law
            ) %>%
        na_if(0) %>%
        filter(!is.na(node)) %>%
        select(x, next_x) %>%
        group_by(x, next_x) %>%
        summarize(n = n()) %>%
        mutate(color = color_id) %>% filter(!is.na(next_x))
  label <- levels(normal_df$x)
  new_total_df <- normal_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n()) %>%
    mutate(color = color_id) %>% filter(!is.na(next_x))
  dead <- (new_total_df$n - lead(new_total_df$n))[1:6]
  new_total_df <- rbind(
    new_total_df, 
    data.frame(
      x = as.factor(c("Pass.Com.1", "Pass.Floor.1", "Pass.Com.2", "Pass.Floor.2", "To.Gov", "Passed")),
      next_x = as.factor(rep("Dead", 6)), 
      n = dead, 
      color = color_id))
  if (include_joint) {
    new_total_df <- rbind(new_total_df, jrps_df) %>%
        group_by(x, next_x, color) %>% summarize(n = sum(n))
  }
  if (color_black) {
    new_total_df <- color_dead(new_total_df)
  }
  return(new_total_df)
}

sierra_data_dead <- function(csv, color_black = TRUE, include_joint = TRUE) {
  supported <- filter(csv, SC.Position == 1, include_joint)
  neutral <- filter(csv, SC.Position == 0, include_joint = TRUE)
  opposed <- filter(csv, SC.Position == -1, include_joint = TRUE)
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
    summarise(n = sum(n), color = "6a5d5d")
  #2c1e1e
  #6a5d5d
  #c06666
  return(rbind(non_dead, dead_comb))
}



no_return_sierra_data <- function(csv, shell = FALSE) {
  supported <- filter(csv, SC.Position == 1)
  neutral <- filter(csv, SC.Position == 0)
  opposed <- filter(csv, SC.Position == -1)
  supported_df <- no_return_data_creator(supported, "rgba(154,205,50,1.0)")
  neutral_df <- no_return_data_creator(neutral, "rgba(176,224,230,1.0)")
  opposed_df <- no_return_data_creator(opposed, "rgba(255,69,0,1.0)")
  if (shell) {
    opposed_df <- rbind(opposed_df, data.frame(x = as.factor(c("Pass.Floor.2", "To.Gov", "To.Gov", "Pass.Floor.1", "Pass.Floor.2")),
                                                   next_x = as.factor(c("Pass.Floor.1", "Pass.Floor.1", "Pass.Floor.2","Passed", "Passed")),
                                                   n = as.numeric(floor(nrow(csv) / 50)), color = "gray"))
  }
  return(rbind(supported_df, neutral_df, opposed_df))
}

no_return_data_creator <- function(csv, color_id, shell = FALSE) {
  a <- mutate(csv, Intro.Com = 1, Law = Passed)
  normal_df <- a %>% 
    make_long(Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2,
              To.Gov,
              Passed,
              Law) %>% 
    na_if(0) %>% 
    filter(!is.na(node)) 
  
  label <- levels(normal_df$x)
  
  new_total_df <- normal_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n()) %>% 
    filter(!is.na(next_x)) %>% 
    mutate(color = color_id)
  if (shell) {
    new_total_df <- rbind(new_total_df, data.frame(x = as.factor(c("Pass.Floor.2", "To.Gov", "To.Gov", "Pass.Floor.1", "Pass.Floor.2")),
                                                   next_x = as.factor(c("Pass.Floor.1", "Pass.Floor.1", "Pass.Floor.2","Passed", "Passed")),
                                                   n = new_total_df[1,3] / 15, color = "gray"))
  }
  return(new_total_df)
  
}


# ggplot identifiers
add_identifiers <- function(x) {
x %>% mutate(Dis = factor(if_else(Disposition == "DiC", "Died in Committee", if_else(Disposition == "PiL", "Passed into Law", "Died Elsewhere")),
                             levels = c("Died in Committee", "Died Elsewhere", "Passed into Law")), 
                        Pos = if_else(SC.Position == 1, "Supported", if_else(SC.Position == 0, "Neutral", "Opposed")))
}

# obtain columns we care about
col_care <- function(x) {
  x %>% select(SC.Position, Pass.Com.1, Pass.Floor.1, Pass.Com.2, Pass.Floor.2, To.Gov, Passed, Disposition, Amended, Returned)
}

average <- function(x) {
  group_by(x, SC.Position) %>% summarize(Passed = mean(Passed), Pass.Com.1 = mean(Pass.Com.1))
}

plot_avg <- function(csv, name, show.legend = TRUE) {
gg <- ggplot(average(csv)) +
  geom_smooth(aes(SC.Position, Pass.Com.1, color = "1"), method = "loess") + 
  geom_smooth(aes(SC.Position, Passed, color = "2"), method = "loess") +
  geom_point(aes(SC.Position, Pass.Com.1, color = "1")) + 
  geom_point(aes(SC.Position, Passed, color = "2")) + 
  theme_minimal() +
  labs(title = paste("Bills that Pass Key Milestones Based on SC Position in", name), x = "SC Position", y = "Portion") + 
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 1, 0.1)) + 
  scale_color_manual(name = "Portion of Bills that", labels = c("Leave Committee","Become Law"), values = c("orange", "green"))
ggsave(paste("images/", name, "_gg_curve.png", sep = ""), plot = gg, bg = "white")
if (!show.legend) {
  gg <- gg + theme(legend.position = "none")
}
return(gg)
}

data_creator_shell <- function(csv, color_id, split = FALSE) {
  a <- mutate(csv, Intro.Com = 1, Law = Passed)
  normal_a <- a %>% filter(is.na(Amended) | Amended == 0) %>%
    filter(is.na(Returned) | Returned == 0) 
  # Separate all the data with the initial return
  one_a <- a %>% filter(Amended == 1, Returned == 0)
  # Separate all the data with the second return
  two_a <- a %>% filter(Amended == 0, Returned == 1)
  # Separate all the data with both returns
  both_a <- a %>% filter(Amended == 1, Returned == 1)
  # Now to turn the data into something that can be read by the ggsankey
  normal_df <- normal_a %>% 
    make_long(Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2,
              To.Gov,
              Passed,
              Law) %>% 
    na_if(0) %>% 
    filter(!is.na(node)) 
  
  one_df <- rbind(
    make_long(one_a,
              Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2) %>% filter(!is.na(next_node)),
    make_long(one_a,
              Pass.Floor.2,
              Pass.Floor.1,
              To.Gov,
              Passed,
              Law)) %>% 
    na_if(0) %>% 
    filter(!is.na(node)) 
  two_df <- NULL
  both_df <- NULL
 if (split) {
  two_df <- rbind(
    make_long(two_a,
              Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2, 
              To.Gov) %>% filter(!is.na(next_node)),
    make_long(two_a,
              To.Gov,
              Pass.Floor.1,
              Passed),
    make_long(two_a,
              To.Gov,
              Pass.Floor.2,
              Passed,
              Law)) %>% 
    na_if(0) %>% 
    filter(!is.na(node)) 
  
  both_df <- rbind(
    make_long(both_a,
              Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2) %>% filter(!is.na(next_node)),
    make_long(both_a,
              Pass.Floor.2,
              Pass.Floor.1,
              To.Gov) %>% filter(!is.na(next_node)),
    make_long(both_a,
              To.Gov,
              Pass.Floor.1,
              Passed) %>% filter(!is.na(next_node)),
    make_long(both_a,
              To.Gov,
              Pass.Floor.2,
              Passed,
              Law) %>% filter(!is.na(next_node))
  )
 } else {
  two_df <- rbind(
    make_long(two_a,
              Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2, 
              To.Gov) %>% filter(!is.na(next_node)),
    make_long(two_a,
              To.Gov,
              Pass.Floor.1,
              Pass.Floor.2,
              Passed,
              Law)
    ) %>% 
    na_if(0) %>% 
    filter(!is.na(node)) 
  
  both_df <- rbind(
    make_long(both_a,
              Intro.Com,
              Pass.Com.1,
              Pass.Floor.1,
              Pass.Com.2,
              Pass.Floor.2) %>% filter(!is.na(next_node)),
    make_long(both_a,
              Pass.Floor.2,
              Pass.Floor.1,
              To.Gov) %>% filter(!is.na(next_node)),
    make_long(both_a,
              To.Gov,
              Pass.Floor.1,
              Pass.Floor.2,
              Passed,
              Law) %>% filter(!is.na(next_node))
  )
 
 }
  
  total_df <- rbind(normal_df, one_df, two_df)
  label <- levels(total_df$x)
  new_total_df <- total_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n()) %>% 
    filter(!is.na(next_x)) %>% 
    mutate(color = color_id)
  return(new_total_df)
}
