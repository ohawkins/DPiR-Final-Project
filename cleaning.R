
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)

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

scenario_q<-qplot(probability,scenario_description,data= df_scenario,geom="boxplot")
#ggsave("boxplot.png",plot=last_plot() , dpi=600)

########
### Presidential electoral vote probabilities
df_evprob <- read_csv("presidential_ev_probabilities_2020_oct28.csv")
df_evprob$cycle <- NULL
df_evprob$branch <- NULL
df_evprob$model <- NULL
df_evprob$modeldate <- as.Date(df$modeldate, format = "%m/%d/%Y")
df_evprob$candidate_3rd <- NULL
df_evprob$evprob_3rd <- NULL
df_evprob$simulations <- NULL
df_evprob$timestamp <- NULL
df_evprob <- rename(df_evprob, "Incumbent" = "candidate_inc",
             "Challenger" = "candidate_chal",
             "Date" = "modeldate",
             "Num Electoral Votes" = "total_ev", 
             "Inc Chance of Winning EV" = "evprob_inc", 
             "Chal Chance of Winning EV" = "evprob_chal")
             

df2_evprob <- df_evprob[, c(4,5,6)]
df2_evprob <- melt(df2_evprob, id.vars = c("Num Electoral Votes"))

highlight_df2 <- df2_evprob %>%
  filter(variable == "Chal Chance of Winning EV" & `Num Electoral Votes` == 306)
highlight2_df2 <- df2_evprob %>%
  filter(variable == "Inc Chance of Winning EV" & `Num Electoral Votes` == 232)

p_evprob <- ggplot(df2_evprob, aes(`Num Electoral Votes`, value, col = variable)) +
  geom_point()
p_evprob <- p_evprob + scale_color_brewer(palette = "Set1", name = "Candidate", labels = c("Trump", "Biden")) + xlab("Total Electoral Votes") + ylab("Probability") + geom_vline(xintercept = 270, linetype = "dashed") + ggtitle("Forecasted probability of each EC outcome by candidate") + labs(subtitle = "(as of October 28th)") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
p_evprob <- p_evprob + geom_point(data = highlight_df2, 
                    aes(x = highlight_df2$`Num Electoral Votes`, y = highlight_df2$value),
                    color = "black",
                    size = 3)
p_evprob <- p_evprob + geom_point(data = highlight2_df2,
                    aes(x = highlight2_df2$`Num Electoral Votes`, y = highlight2_df2$value),
                    color = "black",
                    size = 3)
p_evprob   
#ggsave("EVProbPlot.png", plot = p_evprob, width = 6, height = 4, units = "in", dpi = 600)

###########

### EC vs Popular Vote 
df_ecvspop <- read_csv("electoral_college_vs_popvote_oct28.csv")
df_ecvspop$cycle <- NULL
df_ecvspop$branch <- NULL
df_ecvspop$model <- NULL
df_ecvspop$candidate_3rd <- NULL
df_ecvspop$ecwin_3rd <- NULL
df_ecvspop$timestamp <- NULL
df_ecvspop$modeldate <- as.Date(df_ecvspop$modeldate, format = "%m/%d/%Y")
df_ecvspop <- rename(df_ecvspop, "Date" = "modeldate",
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


df_ecvspop$winner <- as.numeric(df_ecvspop$`Trump wins EC` > 0.5)
df_ecvspop$winner <- as.character(df_ecvspop$winner)
df_ecvspop$winner[df_ecvspop$winner == 1] <- "Trump"
df_ecvspop$winner[df_ecvspop$winner == 0] <- "Biden"
df_ecvspop$winner <- factor(df_ecvspop$winner)

p_ecvspop <- qplot(df_ecvspop$winner, df_ecvspop$`Num sims`, data = df_ecvspop, geom = "boxplot", color = winner) + geom_jitter(alpha = I(0.1))
p_ecvspop <- p_ecvspop + xlab("Winner") + ylab("Number of Sims")
p_ecvspop <- p_ecvspop + scale_color_manual(values = c("Biden" = "blue",
                                       "Trump" = "red"))
p_ecvspop <- p_ecvspop + ggtitle("Winners per simulation", subtitle = "(as of October 28th)") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
p_ecvspop

#ggsave("ECvsPop.png", plot = p_ecvspop, width = 6, height = 4, dpi = 600)


win <- df_ecvspop$`Trump EC votes`
onwin <- df_ecvspop$`Biden EC votes`
boxplot(win, onwin, 
        main = "EC votes per candidate",
        ylab = "Number of EC Votes", 
        names = c("Trump", "Biden"),
        ylim = c(0,538),
        col = c("red", "blue"),
        boxwex = 0.4)

median(df_ecvspop$`Biden EC votes`)
median(df_ecvspop$`Trump EC votes`)
mean(df_ecvspop$`Biden EC votes`)
mean(df_ecvspop$`Trump EC votes`)


