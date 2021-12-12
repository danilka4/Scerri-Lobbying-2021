# Makes the created functions usable
source("function_shell.R")


# Reads in the csv file
csv<- read.csv("sample/2017b.csv")
#csv<- read.csv("<csv file here>")


# Creates the dataframe in the desired format with a given rgb + opacity value
new_total_df <- no_return_sierra_data(csv, shell = TRUE)

# Makes labels for plotly pretty
labs <- c("Introduced", "Passed Committee 1", "Passed Floor 1",
          "Passed Committee 2", "Passed Floor 2", 
          "Delivered to Governor", "Signed by Governor", "Law")

# Plotting machinery
plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0.1, 0.23, 0.4, 0.5, 0.6, 0.75, 0.9),
    y = c(0.5, 0.5, 0.5, 0.29, 0.4, 0.5, 0.3),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(new_total_df$x) - 1,
    target = as.numeric(new_total_df$next_x) - 1,
    value = new_total_df$n,
    color = new_total_df$color,
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sierra Sankey for 2017b Data with Shells",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)

#### -----------------------------------------------------------

# Regular data


df_2017 <- no_return_data_creator(csv, "rgba(31,119,180,0.6)", shell = TRUE)
df_2017_1 <- no_return_data_creator(csv, "rgba(63,54,25,0.6)", shell = TRUE)
df_total <- rbind(df_2017, df_2017_1)

plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = labs,
    x = c(0.1, 0.23, 0.4, 0.5, 0.6, 0.75, 0.9),
    y = c(0.5, 0.5, 0.5, 0.29, 0.4, 0.5, 0.3),
    color = "gray",
    pad = 10), # 10 Pixel
  link = list(
    source = as.numeric(df_total$x) - 1,
    target = as.numeric(df_total$next_x) - 1,
    value = df_total$n,
    color = df_total$color,
    line = list(color = "black", width = 0.5)
    ))%>% 
  layout(title = "Sankey for 2017b Data with Shells",
         xaxis = list(showgrid = F, zeroline = F),
         yaxis = list(showgrid = F, zeroline = F),
         showlegend = T)
