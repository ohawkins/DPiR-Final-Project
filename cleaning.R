
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

### Presidential electoral vote probabilities
library(reshape2)

df <- read_csv("presidential_ev_probabilities_2020_oct28.csv")
df$cycle <- NULL
df$branch <- NULL
df$model <- NULL
df$modeldate <- as.Date(df$modeldate, format = "%m/%d/%Y")
df$candidate_3rd <- NULL
df$evprob_3rd <- NULL
df$simulations <- NULL
df$timestamp <- NULL
df <- rename(df, "Incumbent" = "candidate_inc",
             "Challenger" = "candidate_chal",
             "Date" = "modeldate",
             "Num Electoral Votes" = "total_ev", 
             "Inc Chance of Winning EV" = "evprob_inc", 
             "Chal Chance of Winning EV" = "evprob_chal")

df2 <- df[, c(4,5,6)]
df2 <- melt(df2, id.vars = c("Num Electoral Votes"))

p <- ggplot(df2, aes(`Num Electoral Votes`, value, col = variable)) +
  geom_point()
p <- p + scale_color_brewer(palette = "Set1", name = "Candidate", labels = c("Trump", "Biden")) + xlab("Total Electoral Votes") + ylab("Probability") + geom_vline(xintercept = 270, linetype = "dashed") + ggtitle("Forecasted probability of each EC outcome by candidate") + labs(subtitle = "(as of October 28th)") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
p <- p + geom_point(data = highlight_df2, 
                    aes(x = highlight_df2$`Num Electoral Votes`, y = highlight_df2$value),
                    color = "black",
                    size = 3)
p <- p + geom_point(data = highlight2_df2,
                    aes(x = highlight2_df2$`Num Electoral Votes`, y = highlight2_df2$value),
                    color = "black",
                    size = 3)
p
ggsave("EVProbPlot.png", plot = p, width = 6, height = 4, units = "in", dpi = 600)


### EC vs Popular Vote 
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

df$winner <- as.numeric(df$`Trump wins EC` > 0.5)
df$winner <- as.character(df$winner)
df$winner[df$winner == 1] <- "Trump"
df$winner[df$winner == 0] <- "Biden"
df$winner <- factor(df$winner)

p <- qplot(df$winner, df$`Num sims`, data = df, geom = "boxplot", color = winner) + geom_jitter(alpha = I(0.1))
p <- p + xlab("Winner") + ylab("Number of Sims")
p <- p + scale_color_manual(values = c("Biden" = "blue",
                                       "Trump" = "red"))

ggsave("ECvsPop.png", plot = p, width = 6, height = 4, dpi = 600)


win <- df$`Trump EC votes`
onwin <- df$`Biden EC votes`
boxplot(win, onwin, 
        main = "EC votes per candidate",
        ylab = "Number of EC Votes", 
        names = c("Trump", "Biden"),
        ylim = c(0,538),
        col = c("red", "blue"),
        boxwex = 0.4)

median(df$`Biden EC votes`)
median(df$`Trump EC votes`)
mean(df$`Biden EC votes`)
mean(df$`Trump EC votes`)



