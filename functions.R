# Imports all the necessary libraries
# Plotting library
library(plotly)
# OP data manipulation
library(dplyr)
# Used for make_long function that goes a long way in combining everything into a useful format
library(ggsankey)
# Functions to create a tidy df, mostly used to separate committees
library(tidyr)
# Table Library
library(reactable)

# Creates a data frame with the associated color id. There is an option to include joint resolutions
data_creator <- function (csv, color_id, include_joint = TRUE) {
    # Adds "committee introduction," otherwise we cannot see all the bills that didn't make it out of committee
    # Added law for similar reasons as well
    csv <- mutate(csv, Intro.Com = 1, Law = Passed)

    # Creation of data frames for JR's and non-JR's
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

    # Adds on joint resolutions if that is true
    if (include_joint) {
        normal_df <- rbind(normal_df, jrps_df)
    }
    label <- levels(normal_df$x)

    # Coerces the above dataframe into one that is usable by plotly
    #   This is because the make_long function was made with a different library in mind
    new_normal <- normal_df %>% select(x, next_x) %>% group_by(x, next_x) %>% summarize(n = n()) %>%
        filter(!is.na(next_x)) %>% 
        mutate(color = color_id)
    return(new_normal)
}

# Calls the previous function and colors in based on whether Sierra Club approved of the bill
sierra_data <- function(csv, include_joint = TRUE) {
  supported <- filter(csv, SC.Position == 1)
  neutral <- filter(csv, SC.Position == 0)
  opposed <- filter(csv, SC.Position == -1)
  not_available <- filter(csv, is.na(SC.Position))
  supported_df <- data_creator(supported, "rgba(154,205,50,1.0)", include_joint)
  neutral_df <- data_creator(neutral, "rgba(176,224,230,1.0)", include_joint)
  opposed_df <- data_creator(opposed, "rgba(255,69,0,1.0)", include_joint)
  not_available_df <- data_creator(not_available, "rgba(190,195,198, 1.0)", include_joint)
  return(rbind(supported_df, neutral_df, opposed_df, not_available_df))
}







# Creates a dataframe that includes all the dead bills
#   In the original function the dead bills just disappear
data_creator_dead <- function(csv, color_id, color_black = TRUE, include_joint = TRUE) {
    if (nrow(csv) == 0) {
        return()
    }
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

    # Creates dead bills based off how many bills fell off per step in legislative process
  dead <- (new_total_df$n - lead(new_total_df$n))[1:6]

  # Adds on dead bills to the dataframe
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
  # Colors in the dead bills black if the option is selected
  if (color_black) {
    new_total_df <- color_dead(new_total_df)
  }
  return(new_total_df)
}

# Creates data colored based on Sierra opinion + can make dead bills black
sierra_data_dead <- function(csv, color_black = TRUE, include_joint = TRUE) {
  supported <- filter(csv, SC.Position == 1, include_joint)
  neutral <- filter(csv, SC.Position == 0, include_joint = TRUE)
  opposed <- filter(csv, SC.Position == -1, include_joint = TRUE)
  not_available <- filter(csv, is.na(SC.Position))
  supported_df <- data_creator_dead(supported, "rgba(154,205,50,1.0)", color_black)
  neutral_df <- data_creator_dead(neutral, "rgba(176,224,230,1.0)", color_black)
  opposed_df <- data_creator_dead(opposed, "rgba(255,69,0,1.0)", color_black)
  not_available_df <- data_creator_dead(not_available, "rgba(190,195,198, 1.0)", color_black)
  output <- rbind(supported_df, neutral_df, opposed_df, not_available_df)
  # Color-black has a secondary purpose here of combining all the separate dead bills coming out of a node into one
  #     ie what usually happens is you would have a separate "dead flow" for each of the sierra opinions (supported/opposed)
  #     and since you are coloring in the dead bills in the same color it is useful to just combine these flows, otherwise
  #     they are needlessly separated
  if (color_black) {
    output <- color_dead(output)
  }
  return(output)
}

# Colors all dead bills black and combines them if multiple dead connections have identical nodes
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



# Probably needs a name change, but these functions are similar to data creator without jr's that add a return path so we can see where returned bills would be sent off to
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



# OG function that actually shows all the bills being returned instead of just a small path showing where returned bills go
#   Has a few options for how complicated you want to make the splits (more complicated is more realistic but the issue is
#   obviously that it will be more complicated, so have to deal with a trade off there)
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

sierra_data_shell <- function(csv) {
  supported <- filter(csv, SC.Position == 1)
  neutral <- filter(csv, SC.Position == 0)
  opposed <- filter(csv, SC.Position == -1)
  not_available <- filter(csv, is.na(SC.Position))
  supported_df <- data_creator_shell(supported, "rgba(154,205,50,1.0)")
  neutral_df <- data_creator_shell(neutral, "rgba(176,224,230,1.0)")
  opposed_df <- data_creator_shell(opposed, "rgba(255,69,0,1.0)")
  not_available_df <- data_creator_shell(not_available, "rgba(190,195,198, 1.0)")
  return(rbind(supported_df, neutral_df, opposed_df, not_available_df))
}

# Helper function that will isolate all committees with 10+ Primary bills
consolidate_com <- function(csv) {
    isolated_first <- separate(csv, Com.1, into = c("Committee", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>%
        group_by(Committee) %>% summarize(n = n()) %>% arrange(desc(n))
    isolated_second <- separate(csv, Com.2, into = c("Committee", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>%
        filter(!is.na(Committee), Committee != "") %>%
        group_by(Committee) %>% summarize(n = n()) %>% arrange(desc(n))
    csv <- separate(csv, Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")  %>%
        separate(Com.2, into = c("Com.2", "Com.2.2", "Com.2.3", "Com.2.4"), sep = ";")
    new_csv <- mutate(csv, Com.1 = if_else(Com.1 %in% isolated_first$Committee[1:3], Com.1, "Other.Committee"),
                      Com.2 = case_when(
                                        Com.2 %in% isolated_second$Committee[1:3] ~ paste(Com.2, "2", sep = "."),
                                        is.na(Com.2) ~ NA_character_,
                                        Com.2 == "" ~ NA_character_,
                                        TRUE ~ "Other.Committee.2"
                                        )) %>%
        mutate(Com.1 = factor(Com.1), Com.2 = factor(Com.2, exclude = ".2"))
    return(new_csv)
}

# ggplot identifiers, also turns SC Position into a string form
add_identifiers <- function(x) {
x %>% mutate(Dis = factor(if_else(Disposition == "DiC", "Died in Committee", if_else(Disposition == "PiL", "Passed into Law", "Died Elsewhere")),
                             levels = c("Died in Committee", "Died Elsewhere", "Passed into Law")), 
                        Pos = if_else(SC.Position == 1, "Supported", if_else(SC.Position == 0, "Neutral", "Opposed")))
}

# obtain columns we care about
col_care <- function(x) {
    x$Com.1 <- gsub('\\s+', '', x$Com.1)
    x$Com.2 <- gsub('\\s+', '', x$Com.2)
  x %>% select(SC.Position, Com.1, Pass.Com.1, Pass.Floor.1, Com.2, Pass.Com.2, Pass.Floor.2, To.Gov, Passed, Disposition, Amended, Returned)
}

# ggplot + line graph helper function
average <- function(x) {
  group_by(x, SC.Position) %>% summarize(Passed = mean(Passed), Pass.Com.1 = mean(Pass.Com.1))
}

# More ggplot stuff
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


# Takes csv and turns it into a reactable
rtable <- function(csv) {
    formatted_csv <- group_by(csv, Com.1) %>%
        summarize(
                  n = n(),
                  amount.passed = sum(Pass.Com.1),
                  positive      = sum(SC.Position == 1, na.rm = TRUE),
                  neutral       = sum(SC.Position == 0, na.rm = TRUE),
                  negative      = sum(SC.Position == -1, na.rm = TRUE),
                  become.law    = sum(Passed, na.rm = TRUE)
                  )
    return(
           reactable(formatted_csv,
                     defaultSorted = list(n = "desc"),
                     defaultPageSize = 6,
                     columns = list(
                                    Com.1 = colDef(name = "First Committee", footer = "Total"),
                                    n = colDef(name = "Total Bills Received", footer = sprintf("%d", sum(formatted_csv$n))),
                                    amount.passed = colDef(name = "Total Bills Passed", footer = sprintf("%d", sum(formatted_csv$amount.passed))),
                                    positive = colDef(name = "Positive", footer = sprintf("%d", sum(formatted_csv$positive))),
                                    neutral = colDef(name = "Neutral", footer = sprintf("%d", sum(formatted_csv$neutral))),
                                    negative = colDef(name = "Negative", footer = sprintf("%d", sum(formatted_csv$negative))),
                                    become.law = colDef(name = "Bills Passed into Law", footer = sprintf("%d", sum(formatted_csv$become.law)))
                                    ), bordered = TRUE, striped = TRUE, highlight = TRUE,
                     defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
           )
           )
}

com_creator <- function (csv, color_id = "black", include_joint = TRUE, consol = TRUE) {
    if (nrow(csv) == 0) {
        return()
    }
    # Adds "committee introduction," otherwise we cannot see all the bills that didn't make it out of committee
    # Added law for similar reasons as well
    csv <- mutate(csv, Intro.Com = 1, Law = Passed)

    csv <- separate(csv, Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";")
    csv <- separate(csv, Com.2, into = c("Com.2", "Com.2.2", "Com.2.3", "Com.2.4"), sep = ";")
    if (consol) {
        csv <- consolidate_com(csv)
    }
    # Creation of data frames for JR's and non-JR's
    normal <- filter(csv, Disposition != "JRP")
    jrps <- filter(csv, Disposition == "JRP")
    normal_df <- normal %>% 
        make_long(Intro.Com,
            Com.1,
            Pass.Floor.1,
            Com.2,
            Pass.Floor.2,
            To.Gov,
            Passed,
            Law,
            Passed
            ) %>%
        na_if(0) %>%
        filter(!is.na(node))
    jrps_df <- jrps %>%
        make_long(Intro.Com,
            Com.1,
            Pass.Floor.1,
            Com.2,
            Pass.Floor.2,
            Passed,
            Law,
            Passed
            ) %>%
        na_if(0) %>%
        filter(!is.na(node))

    # Adds on joint resolutions if that is true
    if (include_joint) {
        normal_df <- rbind(normal_df, jrps_df)
    }



    # A bunch of code to reorder factors to correct order
    #   First line orders non-other committees in increasing order
    levs <- filter(normal_df, next_x == "Com.1", next_node != "Other.Committee") %>%
        group_by(next_node) %>% summarize(n = n()) %>% filter(next_node != "Other.Committee") %>%
        arrange(desc(n))
    levs_2 <- filter(normal_df, next_x == "Com.2", next_node != "Other.Committee.2") %>%
        group_by(next_node) %>% summarize(n = n()) %>% filter(next_node != "Other.Committee.2") %>%
        arrange(desc(n))
    # This one attaches other committees + everything else to levels
    levs <- c("Intro.Com", levs$next_node, "Other.Committee", "Pass.Floor.1",
        levs_2$next_node, "Other.Committee.2", "Pass.Floor.2", "To.Gov", "Passed", "Law")


    # Coerces the above dataframe into one that is usable by plotly
    #   This is because the make_long function was made with a different library in mind
    new_df <- normal_df %>%
        mutate(x = as.character(x), node = as.character(node), next_x = as.character(next_x), next_node = as.character(next_node)) %>%
        mutate(x = if_else(x == "Com.1", node, x), next_x = if_else(next_x == "Com.1", next_node, next_x)) %>%
        mutate(x = if_else(x == "Com.2", node, x), next_x = if_else(next_x == "Com.2", next_node, next_x)) %>%
        group_by(x, next_x) %>% summarize(n = n()) %>%
        mutate(x = factor(x, levels = levs[1:(length(levs) - 1)]), next_x = factor(next_x, levels = levs[2:length(levs)])) %>%
        filter(!is.na(next_x))

    # Fix amount of bills going from committee to floor
    subt <- filter(csv, Pass.Com.1 == 1) %>%
        group_by(Com.1) %>% summarize(n = n())
    subt_2 <- filter(csv, Pass.Com.2 == 1) %>%
        group_by(Com.2) %>% summarize(n = n())
    subt <- data.frame(x = subt$Com.1, next_x = "Pass.Floor.1", n = subt$n)
    subt_2 <- data.frame(x = subt_2$Com.2, next_x = "Pass.Floor.2", n = subt_2$n)


    new_df <- filter(new_df, next_x != "Pass.Floor.1", next_x != "Pass.Floor.2") %>%
        rbind(subt, subt_2) %>%
        mutate(color = color_id)
    # Don't necessarily want to level together different factors if doing it for Sierra club
    if (consol) {
            new_df <- mutate(new_df, x = factor(x, levels = levs[1:(length(levs) - 1)]), next_x = factor(next_x, levels = levs[2:length(levs)]))
    }

    new_df <- filter(new_df, !is.na(x))
    return(new_df)
}

com_sierra <- function(csv, include_joint = TRUE) {
    csv <- consolidate_com(csv)

    # Makes levels such that Other.Committee is last
    levs <- group_by(csv, Com.1) %>%
        summarize(n = n()) %>%
        arrange(desc(n))
    levs_2 <- group_by(csv, Com.2) %>%
        summarize(n = n()) %>%
        arrange(desc(n)) %>% filter(!is.na(Com.2))
    levs$Com.1 <- as.character(levs$Com.1)
    levs_2$Com.2 <- as.character(levs_2$Com.2)
    levs <- c(levs$Com.1[levs$Com.1 != "Other.Committee"], "Other.Committee")
    levs_2 <- c(levs_2$Com.2[levs_2$Com.2 != "Other.Committee.2"], "Other.Committee.2")
    levs_2 <- levs_2[!is.na(levs_2)]
    csv$Com.1  <- factor(csv$Com.1, levels = levs)
    csv$Com.2 <- factor(csv$Com.2, levels = levs_2)
    levs <- c("Intro.Com", levs, "Pass.Floor.1",
              levs_2, "Pass.Floor.2", "To.Gov", "Passed", "Law")

    supported <- filter(csv, SC.Position == 1)
    neutral <- filter(csv, SC.Position == 0)
    opposed <- filter(csv, SC.Position == -1)
    not_available <- filter(csv, is.na(SC.Position))
    supported_df <- com_creator(supported, "rgba(154,205,50,1.0)", include_joint, FALSE)
    neutral_df <- com_creator(neutral, "rgba(176,224,230,1.0)", include_joint, FALSE)
    opposed_df <- com_creator(opposed, "rgba(255,69,0,1.0)", include_joint, FALSE)
    not_available_df <- com_creator(not_available, "rgba(190,195,198, 1.0)", include_joint, FALSE)

    # Makes levels check out
    new_df <- rbind(supported_df, neutral_df, opposed_df, not_available_df) %>% mutate(x = factor(x, levels = levs[1:(length(levs) - 1)]), next_x = factor(next_x, levels = levs[2:length(levs)]))
    return(new_df)
}


# A function to generate a plotly line graph based on the inputted data.frame
line_graph <- function(csv, year = 2017, prop = TRUE) {
    title <- ""
    if(prop) {
        title <- paste("Proportion of Bills in", year)
    } else {
        title <- paste("Number of Bills in", year)
    }
    csv_pos <- filter(csv, SC.Position == 1)
    csv_neu <- filter(csv, SC.Position == 0)
    csv_neg <- filter(csv, SC.Position == -1)

    total <- c(nrow(csv), sum(csv$Pass.Com.1, na.rm = TRUE), sum(csv$Pass.Floor.1, na.rm = TRUE), sum(csv$Pass.Com.2, na.rm = TRUE), sum(csv$Pass.Floor.2, na.rm = TRUE), sum(csv$To.Gov, na.rm = TRUE), sum(csv$Passed, na.rm = TRUE))
    positive <- c(nrow(csv_pos), sum(csv_pos$Pass.Com.1, na.rm = TRUE), sum(csv_pos$Pass.Floor.1, na.rm = TRUE), sum(csv_pos$Pass.Com.2, na.rm = TRUE), sum(csv_pos$Pass.Floor.2, na.rm = TRUE), sum(csv_pos$To.Gov, na.rm = TRUE), sum(csv_pos$Passed, na.rm = TRUE))
    neutral <- c(nrow(csv_neu), sum(csv_neu$Pass.Com.1, na.rm = TRUE), sum(csv_neu$Pass.Floor.1, na.rm = TRUE), sum(csv_neu$Pass.Com.2, na.rm = TRUE), sum(csv_neu$Pass.Floor.2, na.rm = TRUE), sum(csv_neu$To.Gov, na.rm = TRUE), sum(csv_neu$Passed, na.rm = TRUE))
    negative <- c(nrow(csv_neg), sum(csv_neg$Pass.Com.1, na.rm = TRUE), sum(csv_neg$Pass.Floor.1, na.rm = TRUE), sum(csv_neg$Pass.Com.2, na.rm = TRUE), sum(csv_neg$Pass.Floor.2, na.rm = TRUE), sum(csv_neg$To.Gov, na.rm = TRUE), sum(csv_neg$Passed, na.rm = TRUE))
    if(prop) {
        total <- total / max(total)
        positive <- positive / max(positive)
        neutral <- neutral / max(neutral)
        negative <- negative / max(negative)
    }
    daf <- data.frame(x = seq_len(length(total)),
                      total, positive, neutral, negative
    )
    plot_ly(daf, x = ~x, y = ~total, name = "Overall Bills", type = "scatter", mode = "lines", line = list(color = "black")) %>%
        add_trace(y = ~positive, name = "Positive", type = "scatter", mode = "lines", line = list(color = "rgba(154,205,50,1.0)")) %>%
        add_trace(y = ~neutral, name = "Neutral", type = "scatter", mode = "lines", line = list(color = "rgba(176,224,230,1.0)")) %>%
        add_trace(y = ~negative, name = "Negative", type = "scatter", mode = "lines", line = list(color = "rgba(255,69,0,1.0)")) %>%
        layout(
               title = title,
               yaxis = list(
                            range = c(0, max(daf$total))
               )
        )
}
