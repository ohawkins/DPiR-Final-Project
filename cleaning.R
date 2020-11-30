
library(readr)
library(ggplot2)

# National Toplines
df <- read_csv('presidential_national_toplines_2020_oct28.csv')
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
df <- read_csv('presidential_polls_2020_oct28.csv')
df$startdate <- as.Date(df$startdate, format = "%m/%d/%Y")
df$enddate <- as.Date(df$enddate, format = "%m/%d/%Y")
df$candidate_name <- factor(df$candidate_name)
df$pollster <- factor(df$pollster)
df$population <- factor(df$population)
df$tracking <- factor(df$tracking)
df$modeldate <- as.Date(df$modeldate, format = "%m/%d/%Y")
df$state <- factor(df$state)
df <- subset(df, select = -c(cycle, tracking))


# Presidential poll averages
df <- read_csv('presidential_poll_averages_2020_nov2.csv')
df$state <- factor(df$state)
df$modeldate <- as.Date(df$modeldate, format = '%m/%d/%Y')
df$candidate_name <- factor(df$candidate_name)
df <- subset(df, select = -cycle)

#Scenario Analysis and Visualization 
df_scenario<-read_csv("Presidential_scenario_analysis_2020_nov2.csv")
mycols<-c("cycle","branch","modeldate","candidate_inc","candidate_chal","scenario_id","probability","scenario_description","simulations")
df_scenario<-df_scenario[,mycols]
df_scenario$modeldate<-(as.Date(df_scenario$modeldate,"%m/%d/%y"))
df_scenario$scenario_id<-factor(df_scenario$scenario_id)


qplot(probability,scenario_description,data= df_scenario,geom="point")
qplot(probability,scenario_description,data= df_scenario,geom="bin2d")
qplot(probability,scenario_description,data= df_scenario,geom="boxplot")
ggsave("boxplot.png",plot=last_plot() , dpi=600)

### electoral college vs popular vote cleaning

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
