
library(readr)
library(ggplot2)

# National Toplines
df <- read_csv('presidential_national_toplines_2020.csv')
df$modeldate <- as.Date(df$modeldate, format = "%m/%d/%Y")

df <- subset(df, select = -c(cycle, 
                             branch, 
                             model, 
                             candidate_inc, 
                             candidate_chal, 
                             candidate_3rd,
                             popwin_3rd,
                             ev_3rd,
                             ev_3rd_hi,
                             ev_3rd_lo,
                             national_voteshare_3rd_hi,
                             national_voteshare_3rd_lo,
                             ecwin_3rd,
                             national_voteshare_3rd,
                             simulations))

p_date_ev_inc <- qplot(modeldate, ev_inc, data = df, geom = 'point')
p_date_popwin_inc <- qplot(modeldate, popwin_inc, data = df, geom = 'point')
p <- qplot(modeldate, ec_nomajority, data = df, geom = 'point')


# Polls
df <- read_csv('presidential_polls_2020.csv')
df$startdate <- as.Date(df$startdate, format = "%m/%d/%Y")
df$enddate <- as.Date(df$enddate, format = "%m/%d/%Y")
df$candidate_name <- factor(df$candidate_name)
df$pollster <- factor(df$pollster)
df$population <- factor(df$population)
df$tracking <- factor(df$tracking)
df$modeldate <- as.Date(df$modeldate, format = "%m/%d/%Y")
df$state <- factor(df$state)
df <- subset(df, select = -c(cycle, tracking))

df <- read_csv('presidential_poll_averages_2020.csv')
