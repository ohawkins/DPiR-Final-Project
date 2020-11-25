library(readr)
library(dplyr)
library(ggplot2)


df <- read_csv("electoral_college_vs_popvote_oct28.csv")
df$cycle <- NULL
df$branch <- NULL
df$model <- NULL
df$candidate_3rd <- NULL
df$ecwin_3rd <- NULL
df$timestamp <- NULL
df$modeldate <- as.Date(df$modeldate, format = "%m/%d/%Y")
df <- rename(df, "Date" = "modeldate",
             "Incumbent" = "candidate_inc",
             "Challenger" = "candidate_chal", 
             "LB popular vote" = "lower_bin_text",
             "UB popular vote" = "upper_bin_text", 
             "Trump wins EC" = "ecwin_inc",
             "Biden wins EC" = "ecwin_chal",
             "Nobody wins EC" = "ecwin_nomajority", 
             "Trump EC votes" = "total_ev_inc",
             "LB Trump EC votes" = "ev_inc_lo", 
             "UB Trump EC votes" = "ev_inc_hi",
             "Biden EC votes" = "total_ev_chal",
             "LB Biden EC votes" = "ev_chal_lo",
             "UB Biden EC votes" = "ev_chal_hi",
             "Num sims" = "count",
             "Total sims" = "simulations")

df$winner <- df$`Trump wins EC` > 0.5
df$winner <- as.numeric(df$winner)
df$winner <- as.character(df$winner)
df$winner[df$winner == 1] <- "Trump"
df$winner[df$winner == 0] <- "Biden"
df$winner <- factor(df$winner)

p <- qplot(`Num sims`,
      data = df, 
      geom = "histogram",
      binwidth = 400,
      fill = winner)

p <- p + scale_fill_manual(
  values = alpha(c("blue", "red"), .7),
  name = "Winner")

p <- p + xlab("Simulations per outcome")
p <- p + ylab("Frequency")
p
