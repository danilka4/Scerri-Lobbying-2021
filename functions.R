# Imports all the necessary libraries
library(plotly)
library(dplyr)
library(ggsankey)
library(tidyr)

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
  supported_df <- data_creator(supported, "rgba(154,205,50,1.0)", include_joint)
  neutral_df <- data_creator(neutral, "rgba(176,224,230,1.0)", include_joint)
  opposed_df <- data_creator(opposed, "rgba(255,69,0,1.0)", include_joint)
  return(rbind(supported_df, neutral_df, opposed_df))
}







# Creates a dataframe that includes all the dead bills
#   In the original function the dead bills just disappear
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
  supported_df <- data_creator_dead(supported, "rgba(154,205,50,1.0)", color_black)
  neutral_df <- data_creator_dead(neutral, "rgba(176,224,230,1.0)", color_black)
  opposed_df <- data_creator_dead(opposed, "rgba(255,69,0,1.0)", color_black)
  output <- rbind(supported_df, neutral_df, opposed_df)
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

# Creates a data frame with the associated color id. There is an option to include joint resolutions
com_creator <- function (csv, color_id, include_joint = TRUE, consol = TRUE) {
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
            Com.1,
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



    # A bunch of code to reorder factors to correct order
    #   First line orders non-other committees in increasing order
    levs <- filter(normal_df, next_x == "Com.1", next_node != "Other.Committee") %>%
        group_by(next_node) %>% summarize(n = n()) %>%
        arrange(desc(n))
    # This one attaches other committees + everything else to levels
    levs <- c("Intro.Com", levs$next_node, "Other.Committee", "Pass.Floor.1",
              "Pass.Com.2", "Pass.Floor.2", "To.Gov", "Passed")


    # Coerces the above dataframe into one that is usable by plotly
    #   This is because the make_long function was made with a different library in mind
    new_df <- normal_df %>%
        mutate(x = as.character(x), node = as.character(node), next_x = as.character(next_x), next_node = as.character(next_node)) %>%
        mutate(x = if_else(x == "Com.1", node, x), next_x = if_else(next_x == "Com.1", next_node, next_x)) %>%
        group_by(x, next_x) %>% summarize(n = n()) %>%
        mutate(x = factor(x, levels = levs[1:(length(levs) - 1)]), next_x = factor(next_x, levels = levs[2:length(levs)])) %>%
        filter(!is.na(next_x))

    # Fix amount of bills going from committee to floor
    subt <- filter(csv, Pass.Com.1 == 1) %>%
        group_by(Com.1) %>% summarize(n = n())
    subt <- data.frame(x = subt$Com.1, next_x = "Pass.Floor.1", n = subt$n)


    new_df <- filter(new_df, next_x != "Pass.Floor.1") %>%
        rbind(subt) %>%
        mutate(color = color_id)
    # Don't necessarily want to level together different factors if doing it for Sierra club
    if (consol) {
            new_df <- mutate(new_df, x = factor(x, levels = levs[1:(length(levs) - 1)]), next_x = factor(next_x, levels = levs[2:length(levs)]))
    }

    return(new_df)
}

com_sierra <- function(csv, include_joint = TRUE) {
    csv <- consolidate_com(csv)

    # Makes levels such that Other Committee is last
    levs <- group_by(csv, Com.1) %>%
        summarize(n = n()) %>%
        arrange(desc(n))
    levs$Com.1 <- as.character(levs$Com.1)
    levs <- c(levs$Com.1[levs$Com.1 != "Other Committee"], "Other Committee")
    csv$Com.1  <- factor(csv$Com.1, levels = levs)
    levs <- c("Intro.Com", levs, "Pass.Floor.1",
              "Pass.Com.2", "Pass.Floor.2", "To.Gov", "Passed")



    supported <- filter(csv, SC.Position == 1)
    neutral <- filter(csv, SC.Position == 0)
    opposed <- filter(csv, SC.Position == -1)
    supported_df <- com_creator(supported, "rgba(154,205,50,1.0)", include_joint, FALSE)
    neutral_df <- com_creator(neutral, "rgba(176,224,230,1.0)", include_joint, FALSE)
    opposed_df <- com_creator(opposed, "rgba(255,69,0,1.0)", include_joint, FALSE)

    # Makes levels check out
    new_df <- rbind(supported_df, neutral_df, opposed_df) %>% mutate(x = factor(x, levels = levs[1:(length(levs) - 1)]), next_x = factor(next_x, levels = levs[2:length(levs)]))
    return(new_df)
}

# Helper function that will isolate all committees with 10+ Primary bills
consolidate_com <- function(csv) {
    isolated_first <- separate(csv, Com.1, into = c("Committee", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") %>%
        group_by(Committee) %>% summarize(n = n()) %>% arrange(desc(n))
    csv <- separate(csv, Com.1, into = c("Com.1", "Com.1.2", "Com.1.3", "Com.1.4"), sep = ";") 
    new_csv <- mutate(csv, Com.1 = if_else(Com.1 %in% isolated_first$Committee[1:3], Com.1, "Other Committee")) %>%
        mutate(Com.1 = factor(Com.1))
    return(new_csv)
}

## Helper function that gets all the first committee, second committee, or both in addition to sierra club position
#isolate_committees <- function(csv, first = TRUE, second = TRUE) {
#
#    # Obtains the sierra club position
#    csv <- add_identifiers(csv)
#    base_committees <- NULL
#
#    # Gets first committee
#    if (first) {
#        base_committees <- select(csv, Committee = Com.1, Pos)
#    }
#
#    # Gets second committee
#    if (second) {
#        sec <- select(csv, Committee = Com.2, Pos) %>%
#            filter(Committee != "")
#        base_committees <- rbind(base_committees, sec)
#    }
#    # Regex to strip whitespace from committees left in there by author
#    base_committees$Committee <- gsub('\\s+', '', base_committees$Committee)
#    # Some bills go through multiple committees, this line separates those committees into multiple rows
#    #   ex. a bill goes through the following committees: H-CL;H-A. Originally these are on one row
#    #   and the function separates that bill into 2 observations, one with H-CL and the other with H-A so
#    #   we can easily look at committees individually
#    base_committees <- separate_rows(base_committees, Committee, sep = ";") %>%
#        separate_rows(Committee, sep = ",") %>%
#    #    separate(Committee, into = c("Chamber", "Committee"), sep = "-")
#    return(base_committees)
#}
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
