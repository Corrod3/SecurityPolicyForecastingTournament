###############################################################################
## File for computing scores, testing and aggregating
## by: Alexander Sacharow
###############################################################################

###############################################################################
# CONTENT
# 0. Preparations
# 1. Get and prepare data
# 2. Forecast summary
# 3. Scoreboard
# 4. control variables
# 5. demographics
# 6. testing
# 7. Aggregation of forecasts
# 8. presentation forecasts
###############################################################################

###############################################################################
# 0. Preparations
###############################################################################

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# load packages / clean environment
source("prep.R")

###############################################################################
# 1. Import and prepare data
###############################################################################

# load clean data (created with clean.R)
# source("clean.R") # requires non-anonymized raw data
SPFT <- read.csv("raw/SPFT-clean.csv")

# Dates + times
SPFT$EndDate <- as.Date(as.character(SPFT$EndDate))
SPFT$Duration..in.seconds. <- as.numeric(SPFT$Duration..in.seconds.)
SPFT$time.sec2_Page.Submit <- as.numeric(SPFT$time.sec2_Page.Submit)

# demographic variables to factor
SPFT$sex <- as.factor(SPFT$sex)
SPFT$selfassessment <- factor(SPFT$selfassessment, 
                              levels =  c("Extremely bad", "Moderately bad",
                                          "Slightly bad", "Neither good nor bad", 
                                          "Slightly good", "Moderately good",
                                          "Extremely good"))

# source to character
SPFT$source <- as.character(SPFT$source)

# Import realized outcomes
FQ <- read_excel("raw/SPFT-questions-results.xlsx")

# clean FQ file
FQ <- FQ %>% filter(!is.na(fq.id))

# create arrays
# array with column names of forecasts
fq <- colnames(select(SPFT, contains("fq")))

# mct responses to numeric
mct.col <- colnames(select(SPFT, contains("mct.")))

# column names only pro + con arguments
mct.col <- mct.col[-c(1,14)]

###############################################################################
# 2. Forecasting summaries
###############################################################################

# Create data frame for aggregated forecasts
FO <- FQ[,(1:2)]

# compute means for each question
fore.means <- t(aggregate(SPFT[,fq], list(part.group = SPFT$part.group), mean))

# rename columns to include mean and participants group
colnames(fore.means) =  paste(fore.means[1,], "mean", sep = ".")
fore.means = fore.means[-1, ]

# compute average of all
fore.means <- cbind(fore.means, all.mean = colMeans(SPFT[,fq]))
fore.means <- data.frame(fq.id = gsub("_1","",row.names(fore.means)),
                         fore.means, row.names = NULL)

# combine forecasting means with questions
FO <- merge(FO,fore.means, by = "fq.id")
rm (fore.means)

# compute standard deviations and add to data frame
fore.sd <- t(aggregate(SPFT[,fq], list(Part = SPFT$part.group), sd))
colnames(fore.sd) =  paste(fore.sd[1,], "sd", sep = ".")
fore.sd = fore.sd[-1, ]

# add column with standard deviations
fore.sd <- cbind(fore.sd, all.sd = apply(SPFT[,fq],2,sd))

# merge to data frame
fore.sd <- data.frame(fq.id = gsub("_1","",row.names(fore.sd)),
                      fore.sd, row.names = NULL)
FO <- merge(FO,fore.sd, by = "fq.id")

# make content numeric and delete unneeded variable
FO[,-(1:2)] = apply(FO[,-(1:2)], 2, function(x) as.numeric(as.character(x)))
rm(fore.sd)


# plotting distribution #######################################################
# idea: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# colors for 3 qualitative categories (colorbrewer2.org): #1b9e77 #d95f02 #7570b3

# prepare data for ploting
FO.plot <- SPFT[,c("ResponseId", "part.group", fq)]
FO.plot2 <- melt(select(FO,-contains(".sd"), -question), id.vars = "fq.id")
colnames(FO.plot2) <- c("fq.id", "part.group", "mean")
FO.plot2$part.group <- gsub(".mean","",FO.plot2$part.group)
FO.plot2$mean <- as.numeric(FO.plot2$mean)

# selection question for printing
# q <- 3

# Different plot versions: 
# ggplot(FO.plot, aes(x=eval(parse(text = fq[q])), fill=part.group)) +
#  geom_histogram(binwidth=.1, alpha=.5, position="identity")

# ggplot(FO.plot, aes(x=eval(parse(text = fq[q])), fill=part.group)) +
#  geom_histogram(binwidth=.05, alpha=.5, position="identity")

# ggplot(FO.plot, aes(x=eval(parse(text = fq[q])), fill=part.group)) +
#  geom_histogram(binwidth=.05, position="dodge")

# this one seems to be the most clear one
response.all2 <- function(q){
ggplot(FO.plot, aes(x=eval(parse(text = fq[q])), fill=part.group)) +
  geom_histogram(binwidth=.1, position="dodge") + # bar type
  geom_vline(data=filter(FO.plot2, 
                         fq.id == paste("fq",as.character(q), sep = "") & 
                           part.group != "all"),
             aes(xintercept=mean,  colour=part.group), 
             linetype="dashed", size=1.5) + # group average
  labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE),
                      paste, collapse="\n" ),
       x = "What is the probability of this event to happen?",
       y = "Number of estimates") # labels
}



# this one is also informative

response.all <- function(q){
ggplot(FO.plot, aes(x=eval(parse(text = fq[q])), fill=part.group)) +
  geom_density(alpha=.3) +
  geom_vline(data=filter(FO.plot2, 
                         fq.id == paste("fq",as.character(q), sep = "") & 
                           part.group != "all"),
             aes(xintercept=mean,  colour=part.group), 
             linetype="dashed", size=1.5) + # group average
  labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE),
                      paste, collapse="\n" ),
       x = "What is the probability of this event to happen?",
       y = "Distribution of estimates") + # labels
  expand_limits(x=c(0,1)) + # set range of x-axis
  scale_x_continuous(labels=percent) # percentages
}
# test plot
# response.all(6)

# density plot for html presentation  
response.uni <- function(q){
  ggplot(filter(FO.plot, part.group == "uni"), 
         aes(x=eval(parse(text = fq[q])), fill=part.group)) +
    geom_density(alpha=.3) +
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "uni"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE),
                        paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("red")) + # legend mean vline
    expand_limits(x=c(0,1)) + # set range of x-axis
    scale_x_continuous(labels=percent) # percentages
}
# test plot
# response.uni(2)

response.uni2 <- function(q){
  ggplot(filter(FO.plot, part.group == "uni"), 
         aes(x=eval(parse(text = fq[q])), fill=part.group)) +
    geom_histogram(binwidth=.10, position="dodge") + # bar type
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "uni"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE),
                        paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "# of estimates") + # labels
    expand_limits(x=c(0,1)) + # set range of x-axis
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("red")) + # legend mean vline
    scale_x_continuous(labels=percent) # percentages
}
# test plot
# response.uni2(2)

###############################################################################
# 3. Score board
###############################################################################

# recode FQ to binary
FQ[,4] <- 0
FQ[FQ[,3] == "yes", 4] <- 1
colnames(FQ) <- c(colnames(FQ)[1:3], "out")

## calculate brier scores for each question/respondent
# Brier Score functions
brierSimple <- function(x, y) {
  r = (x-y)^2
  return(r)
}

brierScore <- function(x, y) {
  r = (x-y)^2 + (y-x)^2
  return(r)
}

# compute brier score for each question and individual
for(i in 1:nrow(FQ)){
  SPFT[,paste("bs.fq",i, sep = ".")] <- SPFT %>% 
    dplyr::select(contains(paste("fq",i,"_1", sep = ""))) %>%
    brierScore(as.numeric(FQ[i,4])) %>% as.vector()
}

# Compute average brier score for each respondent
SPFT[,"brier.avg"] <- rowMeans(select(SPFT, contains("bs.fq")))

# Sort by brier score 
SB <- SPFT %>% select(ResponseId, part.group, brier.avg) %>%
  arrange(brier.avg)

brier.plot <- ggplot(SB, aes(x = brier.avg)) +
     geom_histogram(binwidth=.05, position="dodge", fill = "#C02F39") + # bar type
     theme_bw() +
     theme(axis.title = element_text(size=18, colour = "#696969", family = "serif"), # Labels axis font size
           axis.text = element_text(size=14, colour = "#696969"),
           axis.line = element_line(colour = "#696969"), 
           axis.ticks = element_line(colour = "#696969")) +  
     labs( # title = "Brier score distribution",
          x = "Brier score",
          y = "Frequency") # labels))

# remove late replies from score board (for testing)
SB <- SB[!SB$ResponseId %in% SPFT$ResponseId[SPFT$EndDate >= "2017-02-13"],]

###############################################################################
# 4. Computing control variables and testing
###############################################################################

# remove late submissions
SPFT <- SPFT %>% filter(EndDate < "2017-02-13")

# compute BNT score ###########################################################
# add column for bnt score
SPFT$bnt.s <- 0

# replace NA with wrong answer / place holder
# justification: Most likely missing answer is due to lack of knowledge
SPFT$bnt1[is.na(SPFT$bnt1)] <- -1
SPFT$bnt2[is.na(SPFT$bnt2)] <- -1
SPFT$bnt3[is.na(SPFT$bnt3)] <- -1
SPFT$bnt4[is.na(SPFT$bnt4)] <- -1

# compute bnt score stepwise
SPFT <- SPFT %>% mutate(bnt.s = ifelse(bnt1 == 30, bnt.s + 1, bnt.s))
SPFT <- SPFT %>% mutate(bnt.s = ifelse(bnt2 == 25, bnt.s + 1, bnt.s))
SPFT <- SPFT %>% mutate(bnt.s = ifelse(bnt3 == 20, bnt.s + 1, bnt.s))
# note: in question it was not specified to reply in percentage
SPFT <- SPFT %>% mutate(bnt.s = ifelse(bnt4 == 50 | bnt4 == 0.5, bnt.s + 1, bnt.s))

# plot scatterplot to illustrate correlation between bnt and brier scores
# plot(SPFT$bnt.s, SPFT$brier.avg, main="Scatterplot BNT Score & Brier Scores",
#     xlab="BNT Score", ylab="Brier Scores")
# abline(lm(SPFT$brier.avg~SPFT$bnt.s), col="red")

# Distribution of bnt scores
bnt.plot <- ggplot(SPFT, aes(x = bnt.s)) + 
  geom_bar(fill = "#C02F39") +
  theme_bw() +
  theme(axis.title = element_text(size=18, colour = "#696969", family = "serif"), # Labels axis font size
        axis.text = element_text(size=14, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) +  
  labs(# title = "Distribution of results Berlin Numeracy Test (BNT)",
       x = "BNT Score (# of correct answers)",
       y = "# of respondents") # labels

# T Test
# comparing means
# t.test(y1,y2)

# MCT Scores ##################################################################
# instructions are available here:
# http://www.uni-konstanz.de/ag-moral/mut/mjt-intro.htm

# checking number of missing values
# sum(select(SPFT, contains("mct")) == "")

# reorder the test items by stage (left new name) # for pro Doktor (aus FAQ)
SPFT <- dplyr::rename(SPFT, mct.d.pro1 = mct.d.pro_3,mct.d.pro2 = mct.d.pro_4,
                      mct.d.pro3 = mct.d.pro_6, mct.d.pro4 = mct.d.pro_5,
                      mct.d.pro5 = mct.d.pro_2, mct.d.pro6 = mct.d.pro_1)

# reorder the test items by stage (left new name) # for con Doktor (Aus Statista script)
SPFT <- dplyr::rename(SPFT, mct.d.con1 = mct.d.con_4,mct.d.con2 = mct.d.con_5,
                      mct.d.con3 = mct.d.con_1, mct.d.con4 = mct.d.con_6,
                      mct.d.con5 = mct.d.con_2, mct.d.con6 = mct.d.con_3)

# reorder the test items by stage (left new name) # for pro worker (Aus Statista script)
SPFT <- dplyr::rename(SPFT, mct.w.pro1 = mct.w.pro_1, mct.w.pro2 = mct.w.pro_5,
                      mct.w.pro3 = mct.w.pro_3, mct.w.pro4 = mct.w.pro_2,
                      mct.w.pro5 = mct.w.pro_6, mct.w.pro6 = mct.w.pro_4)

# reorder the test items by stage (left new name) # for con worker (Aus Statista script)
SPFT <- dplyr::rename(SPFT, mct.w.con1 = mct.w.con_6,mct.w.con2 = mct.w.con_3,
                      mct.w.con3 = mct.w.con_5, mct.w.con4 = mct.w.con_1,
                      mct.w.con5 = mct.w.con_4, mct.w.con6 = mct.w.con_2)

# change column name string (keeps order intact)
mct.col <- gsub('_', '', mct.col)

# calculating total sum (3)
SPFT$mct.ts <- rowSums(SPFT[, mct.col])

# calculate ss_m (6)
SPFT$mct.ss.m <- SPFT$mct.ts*SPFT$mct.ts/24

# calculate sums of squares (5)
SPFT$mct.tss <- rowSums((SPFT[, mct.col])^2)

# sums of stage squares (2)
SPFT$mct.sss <- 0

# i <- 1

for(i in 1:6) {
SPFT$mct.sss <- SPFT$mct.sss +  
                rowSums(SPFT[, grep(paste("mct.*.",i,sep = ""), names(SPFT))]) *
                rowSums(SPFT[, grep(paste("mct.*.",i,sep = ""), names(SPFT))])  
}

# calculate competency score
SPFT$mct.c <- (SPFT$mct.sss/4 - SPFT$mct.ss.m)/(SPFT$mct.tss - SPFT$mct.ss.m)

# Distribution of moral competency score
mct.plot <- ggplot(filter(SPFT, is.na(mct.c) == F), aes(x = mct.c)) +
  geom_histogram(binwidth=.05, position="dodge", fill = "#C02F39") + # bar type
  labs(title = "Moral Competency Test (MCT)",
     x = "MCT Score",
     y = "# of respondents") + # labels
  theme(axis.title = element_text(size=18, colour = "#696969", family = "serif"), # Labels axis font size
        axis.text = element_text(size=14, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) +  
  expand_limits(x=c(0,1)) # set range of x-axis

# summary(SPFT$mct.c)
# str(filter(SPFT, part.group == "uni"))

# moral competency distribution by group
mct.plot2 <- ggplot(filter(SPFT, is.na(mct.c) == F), aes(x = mct.c, fill = part.group)) +
  geom_density(alpha=.3) +
  # geom_histogram(binwidth=.05, position="dodge") + # bar type
  labs(title = "Moral Competency Test (MCT)",
       x = "MCT Score",
       y = "# of respondents") + # labels
  theme(axis.title = element_text(size=18, colour = "#696969", family = "serif"), # Labels axis font size
        axis.text = element_text(size=14, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) + 
  expand_limits(x=c(0,1)) # set range of x-axis

# cleaning from interrim calculations 
SPFT <- SPFT %>% select(-mct.ts, -mct.tss, -mct.sss, -mct.ss.m)

# timing #####################################################################

# code to minutes
SPFT$Duration.min <- SPFT$Duration..in.seconds./60
SPFT$time.fq.sec <- SPFT$time.sec2_Page.Submit/60

# levels(as.factor(SPFT$time))
SPFT$time.min <- as.numeric(mapvalues(SPFT$time,
                                      levels(as.factor(SPFT$time)),
                                      c(90,20,180,45,5)))

# Basic Scatterplot Matrix for time (to compare correlation)
# extreme outliers are eliminated
# actual time one forecasting question should be used as 
# full duration and self reported data captures time spend on other sections
# pairs(~Duration.min+time.fq.sec+time.min,data=filter(SPFT, time.fq.sec < 1000),
#      main="Simple Scatterplot Matrix")

time.plot <- ggplot(filter(SPFT, time.fq.sec < 1000), 
                    aes(x = time.fq.sec, fill = part.group)) +
                geom_density(alpha=.3) +
                labs(title = "Distribution time spend on forecasting questions",
                     x = "Time in min",
                     y = "Share of respondents") # labels

# make extreme time as missing data
SPFT$time.fq.sec[SPFT$time.fq.sec > 1000] <- NA


# team ########################################################################

# summary(SPFT$team)

team.plot <- ggplot(SPFT, aes(x = team)) +
                geom_bar() +
                coord_flip() +
                labs(title = "Individual vs team-work (self-selected)",
                     x = "# of individuals making forecast",
                     y = "# of respondents") # labels

# comment: should probably be dropped

# intervention / treatment ####################################################

group.plot <- ggplot(SPFT, aes(x = Group)) +
                geom_bar() +
                labs(title = "Treatments and active checking",
                     x = "Group",
                     y = "# of respondents") # labels

# variety of information ######################################################

SPFT$source.var <- unlist(lapply(strsplit(SPFT$source, 
                                          split = ",",
                                          fixed = TRUE),
                                 length))

source.var.plot <- ggplot(SPFT, aes(x = source.var)) +
                      geom_bar() +
                      labs(title = "Variety of sources used for forecast",
                           x = "Number of Sources",
                           y = "# of respondents") # labels

source.plot.data <- as.data.frame(table(unlist(strsplit(SPFT$source, 
                                                        split = ",",
                                                        fixed = TRUE)
                                               )))
source.plot <- ggplot(source.plot.data, aes(x = Var1)) +
                  geom_bar(aes(y = Freq), stat = "identity") +
                  coord_flip() + # flip sides
                  labs(title = "Sources used for forecast",
                       x = "Sources",
                       y = "# of respondents") # labels

###############################################################################
# 5. demographic data - descriptives
###############################################################################

# SPFT.Demo.Plot <- SPFT %>% select(year, sex)

# correct typos
filter(select(SPFT, year), year < 1900 | year > 2017)
SPFT$year[SPFT$year=="19.061990"] <- 1990
SPFT$year[SPFT$year=="19.06199"] <- 1990
SPFT$year[SPFT$year=="23"] <- 1994
SPFT$year[SPFT$year=="66"] <- 1966

# compute age
SPFT$age = 2017 - as.integer(SPFT$year)
SPFT <- SPFT %>% select(-year)

# compute age groups
SPFT$age.gr<-c( "<14", "15-19", "20-24", "25-29", "30-34",
                 "35-39","40-44", "45-49", "50-54", "55-59", "60-64", "65+")[
                   findInterval(SPFT$age , c(-Inf, 14.5, 19.5, 24.5,
                                                        29.5, 34.5, 39.5, 44.5,
                                                       49.5, 54.5, 59.5, 64.5, 
                                                       Inf))]

# counting the age group / gender occurances
SPFT.Demo.Plot <- SPFT %>% select(age.gr,sex) %>% group_by(age.gr, sex) %>% 
                    dplyr::count()

# plot population pyramid
pop.plot <- ggplot(data = SPFT.Demo.Plot, aes(x = age.gr, y = n, fill = sex)) +
                geom_bar(data = subset(SPFT.Demo.Plot, sex == "Female"),
                         stat = "identity") +
                geom_bar(data = subset(SPFT.Demo.Plot, sex == "Male"),
                         stat = "identity",
                         position = "identity",
                         mapping = aes(y = -n)) +
                scale_y_continuous(labels = abs) +
                labs(title = "Age and gender of participants",
                     x = "Age groups",
                     y = "# of respondents") + # labels
                coord_flip()

# just gender
gender.plot <- ggplot(data = SPFT, aes(x = sex)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent)


# intuition vs. analysis ######################################################

# order replies
SPFT$intu.anal <- factor(SPFT$intu.anal, 
                         levels =  c("Only intuition", 
                                     "Mostly intuition, some analysis",
                                     "About evenly intuition and analysis",
                                     "Mostly analysis, some intuition", 
                                     "Only analysis"))
# plot object
intu.anal.plot <- ggplot(SPFT, aes(x = intu.anal)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Intuition vs. Analysis",
       x = "Approach",
       y = "# of respondents") # labels

intu.share <- (as.data.frame(table(SPFT$intu.anal))[1,2] + 
                 as.data.frame(table(SPFT$intu.anal))[2,2]) / nrow(SPFT) * 100

# self-assessment ############################################################
# order 

# levels(as.factor(SPFT$selfassessment))

selfassessment.plot <- ggplot(SPFT, aes(x = selfassessment)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Self-assessment",
       x = "Assessment",
       y = "# of respondents") # labels

# experience ##################################################################
# order
SPFT$exp <- factor(SPFT$exp, 
                   levels =  c("No, I never participated in any forecasting of events",
                               "Yes, I have tried forecasting events a few times",
                               "Yes, I sometimes forecast events",
                               "Yes, I regularly forecast events"))                      
                            
# levels(as.factor(SPFT$exp))

exp.plot <- ggplot(SPFT, aes(x = exp)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Forecasting experience",
       x = "Assessment",
       y = "# of respondents") # labels

# experience security policy ##################################################
# order
SPFT$exp.sp <- factor(SPFT$exp.sp, 
                   levels =  c("None",                                                                                             
                               "Yes, but less than six months", 
                               "Yes, between six months and two years", 
                               "Yes, more than two years"))

# levels(as.factor(SPFT$exp.sp))

exp.sp.plot <- ggplot(SPFT, aes(x = exp.sp)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Security policy experience",
       x = "Assessment",
       y = "# of respondents") # labels

# education ###################################################################

edu.plot <- ggplot(SPFT, aes(x = edu)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Education",
       x = "Education",
       y = "# of respondents") # labels

# employment ##################################################################

emp.plot <- ggplot(SPFT, aes(x = emp)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Employment / occupation",
       x = "Occupation",
       y = "# of respondents") # labels

# summary table for all numerical variables ###################################
# move it to the online appendix

# stargazer(select(SPFT, bnt.s, mct.c, time.fq.sec, Duration.min, age),
#          type="html", out = "DescStat.html")

###############################################################################
# 6. Testing
###############################################################################

# Skill vs. Luck ##############################################################

# Compute expected Brier score for p = 0.5 for each question 
# 1. Individual Brier score for each possible outcome (0 or 1) 
# 2. Calculate average for each question
# 3. Calculate average over all questions
brier.exp.fq <- 0
for(i in 1:nrow(FQ)){
  brier.exp.fq[i] <- mean(2*((select(SPFT, contains(paste("fq", i, "_1",sep = ""))))^2 +
                              (1 - select(SPFT, contains(paste("fq", i, "_1",sep = ""))))^2 )/2)
 }

# T-Test one-sided (Expected Brier score with p = 50%)
t.test.against.random  <- t.test(SPFT$brier.avg, mu = mean(brier.exp.fq), 
                                 alternative = "less", conf.level=0.95)

# get text string for the paper
t.test.against.random  <- paste("t(", t.test.against.random[[2]], ") = ",
                                round(t.test.against.random[[1]],2),
                                ", p < ", 
                                ifelse(round(t.test.against.random[[3]],4)< 0.001,
                                       0.001,round(t.test.against.random[[3]],4)),
                                sep = "")

# alternative: Use average probability for each question to compute brier score

# alternative skills vs. luck test: correct side of 50% #######################

# score board for correct side
SB.CS <- SPFT  %>% select(ResponseId, 
                          starts_with("fq")) 

## calculate correct side scores for each question/respondent
# i <- 1
 for(i in 1:nrow(FQ)){
  tmp <- paste("fq", i, sep = "")
  # add outcome with 1 of on correct side of 50% and 0 if not 
  SB.CS[,paste(tmp,"cs", sep = ".")] <- 
    ifelse(abs(as.numeric(FQ[FQ[,1] == tmp, 4]) - select(SB.CS, i+1)) > 0.5,0,1)
  rm(tmp)
}
# individual share of being on the correct side with the forecast
SB.CS[,"cs.avg"] <- rowMeans(select(SB.CS, contains("cs")))

# One-sided T-Testing correct side measure
t.test.correct.side <- t.test(SB.CS$cs.avg, mu=0.5, alternative = "greater",
                              conf.level = 0.95, equal.var = T)

# string for paper 
t.test.correct.side  <- paste("t(", t.test.correct.side[[2]], ") = ",
                              round(t.test.correct.side[[1]],2),
                              ", p < ",
                              ifelse(round(t.test.correct.side[[3]],4)< 0.001,
                                     0.001,round(t.test.correct.side[[3]],4)),   
                              sep = "")

# Hypotheses testing ##########################################################

# function to generate a correlation matrix
# Source: http://myowelt.blogspot.de/2008/04/beautiful-correlation-tables-in-r.html
corstarsl <- function(x){
  require(Hmisc)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", 
                    ifelse(p < .01, "** ", 
                           ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew)
}

# remove comment from xtable
options(xtable.comment = FALSE)

# compute log(time)
SPFT$time.fq.sec.log <- log(SPFT$time.fq.sec)

# correlation table with significant test
cor.table <- corstarsl(select(SPFT, brier.avg, bnt.s, mct.c, time.fq.sec.log))
names(cor.table) <- c("Brier score", "BNT score", "MCT score")
rownames(cor.table) <- c("Brier score", "BNT score", "MCT score", "log(time)")

# table to Latex format
cor.plot <- xtable(cor.table, caption = "Correlation Table")

# complete correlation table for data mining
# SPFT$selfassessment.num <- as.numeric(SPFT$selfassessment)
# cor.plot2 <- xtable(corstarsl(select(SPFT, brier.avg, bnt.s, mct.c, time.fq.sec,
#                                     time.fq.sec.log, time.min, source.var, age, selfassessment.num)), 
#                   caption = "Correlation Table")

# testing only for subgroups

SPFT.Time <- SPFT %>% select(brier.avg, bnt.s, mct.c, time.fq.sec, Group) %>% filter(time.fq.sec > 6)

SPFT.Anal <- SPFT %>% select(brier.avg, bnt.s, mct.c, time.fq.sec, Group) %>% filter(time.fq.sec > 6)
# Hypothesis 1a ################################################################

# t test manual
cor(SPFT$brier.avg, SPFT$bnt.s)*
  ((length(SPFT$bnt.s)-2)/(1 - cor(SPFT$brier.avg, SPFT$bnt.s)^2))^(1/2)

# T test for Hyppthesis 1a
t.test.brier.bnt <- paste("t(", cor.test(SPFT$brier.avg, SPFT$bnt.s)[[2]],
                          ") = ", 
                          round(cor.test(SPFT$brier.avg, SPFT$bnt.s)[[1]], 2),
                          ", p = ", 
                          round(cor.test(SPFT$brier.avg, SPFT$bnt.s)[[3]], 3),
                          sep = "")
# Correlation between Brier score and BNT score
cor.brier.bnt <- paste("r = ", round(cor(SPFT$brier.avg, SPFT$bnt.s),2), ", ",
                       t.test.brier.bnt, sep = "")

# Test with only long decision time
# T test for Hyppthesis 1a
t.test.brier.bnt.time <- paste("t(", cor.test(SPFT.Time$brier.avg, SPFT.Time$bnt.s)[[2]],
                          ") = ", 
                          round(cor.test(SPFT.Time$brier.avg, SPFT.Time$bnt.s)[[1]], 2),
                          ", p = ", 
                          round(cor.test(SPFT.Time$brier.avg, SPFT.Time$bnt.s)[[3]], 3),
                          sep = "")
# Correlation between Brier score and BNT score
cor.brier.bnt.time <- paste("r = ", round(cor(SPFT.Time$brier.avg, SPFT.Time$bnt.s),2), ", ",
                       t.test.brier.bnt.time, sep = "")




# Hypothesis 1b ###############################################################

# T test for Hyppthesis 1b
t.test.brier.mct <- paste("t(", cor.test(SPFT$brier.avg, SPFT$mct.c)[[2]],
                          ") = ", 
                          round(cor.test(SPFT$brier.avg, SPFT$mct.c)[[1]], 2),
                          ", p = ",
                          round(cor.test(SPFT$brier.avg, SPFT$mct.c)[[3]], 3),
                          sep = "")
# Correlation between Brier score and BNT score
cor.brier.mct <- paste("r = ", 
                       round(cor(SPFT$brier.avg, SPFT$mct.c, use="complete.obs"), 2),
                       ", ", t.test.brier.mct,
                       sep = "")

# Scatterplot for Hypothesis

cor.brier.mct.plot <- ggplot(filter(SPFT, !is.na(mct.c)), aes(x=mct.c, y=brier.avg)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm, color = "#C02F39") +   # Add linear regression line 
  theme_bw() +
  theme(axis.title = element_text(size=18, colour = "#696969", family = "serif"), # Labels axis font size
        axis.text = element_text(size=14, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) + 
  labs(x = "Moral Competency Score",
       y = "Brier Score") # labels

# with minium Forecasting time: T test for Hyppthesis 1b
t.test.brier.mct.time <- paste("t(", cor.test(SPFT.Time$brier.avg, SPFT.Time$mct.c)[[2]],
                          ") = ", 
                          round(cor.test(SPFT.Time$brier.avg, SPFT.Time$mct.c)[[1]], 2),
                          ", p = ",
                          round(cor.test(SPFT.Time$brier.avg, SPFT.Time$mct.c)[[3]], 3),
                          sep = "")
# Correlation between Brier score and BNT score
cor.brier.mct.time <- paste("r = ", 
                       round(cor(SPFT.Time$brier.avg, SPFT.Time$mct.c, use="complete.obs"), 2),
                       ", ", t.test.brier.mct.time,
                       sep = "")




# Hypothesis 2 ################################################################

# T test for Hypothesis 2
t.test.brier.time <- paste("t(", 
                           cor.test(SPFT$brier.avg, 
                                    SPFT$time.fq.sec.log)[[2]],
                          ") = ", 
                          round(cor.test(SPFT$brier.avg,
                                         SPFT$time.fq.sec.log)[[1]], 2),
                          ", p = ",
                          round(cor.test(SPFT$brier.avg,
                                         SPFT$time.fq.sec.log)[[3]], 3),
                          sep = "")
# Correlation between Brier score and BNT score
cor.brier.time <- paste("r = ",
                        round(cor(SPFT$brier.avg, 
                                  SPFT$time.fq.sec.log, use="complete.obs"),2),
                        ", ",
                        t.test.brier.time, 
                        sep = "")

#
cor.brier.time.linear <- paste("r = ",
                               round(cor(SPFT$brier.avg,
                                         SPFT$time.fq.sec, use="complete.obs"),2)
                               , sep = "")

# scatterplot with non-linear regression line
# Source: http://stackoverflow.com/questions/37329074/geom-smooth-and-exponential-fits
log.model <- lm(brier.avg ~ log(time.fq.sec), SPFT)
log.model.df <- data.frame(x = SPFT$time.fq.sec[!is.na(SPFT$time.fq.sec)],
                           y = fitted(log.model))


cor.brier.time.plot <- ggplot(filter(SPFT, !is.na(time.fq.sec)), 
                              aes(x=time.fq.sec, y=brier.avg)) +
  geom_point(shape=1) +    # Use hollow circles
#  geom_smooth(method="lm",  linetype = 2, color = "#C02F39") +
  # geom_smooth(method=lm, color = "#C02F39") +   # Add linear regression line 
  geom_line(data = log.model.df, 
            aes(x, y, color = "Log Model"), 
            size = 2, linetype = 1, color = "#C02F39") + 
  theme_bw() +
  theme(axis.title = element_text(size=20, colour = "#696969", family = "serif"), # Labels axis font size
        axis.text = element_text(size=16, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) + 
  labs(x = "time in min (linear)",
       y = "Brier score") # labels

cor.brier.time.log.plot <- ggplot(filter(SPFT, !is.na(time.fq.sec)), 
                              aes(x=time.fq.sec.log, y=brier.avg)) +
  geom_point(shape=1) +    # Use hollow circles
#  geom_smooth(method="lm",  linetype = 1) +
  geom_smooth(method=lm, color = "#C02F39", size = 2) +   # Add linear regression line 
  theme_bw() +
  theme(axis.title = element_text(size=20, colour = "#696969", family = "serif"), # Labels axis font size
        axis.text = element_text(size=16, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) +  
  labs(x = "log(time)",
       y = "Brier score") # labels

# Hypothesis 3 ################################################################

# get rid of empthy factor levels
SPFT$Group <- as.factor(as.character(SPFT$Group))

t.test.intervention <- t.test(x = select(filter(SPFT, Group == "Treatment"), brier.avg), 
       y = select(filter(SPFT, Group == "Control"), brier.avg), 
       alternative = "less", var.equal = T,
       conf.level = 0.95)


t.test.intervention.result <- paste("t(", t.test.intervention[[2]], ") = ",
                             round(t.test.intervention[[1]],2),
                             ", p < ",
                             ifelse(round(t.test.intervention[[3]],4)< 0.001,
                                    0.001,round(t.test.intervention[[3]],4)),   
                             sep = "")
# testing reasons for failure
hypo3.time.plot <- ggplot(filter(SPFT, is.na(time.fq.sec.log) == FALSE),
                          aes(x = time.fq.sec.log, fill = Group)) +
  geom_density(alpha = 0.5) +
  #geom_histogram(binwidth=.05, position="dodge", fill = "#C02F39") + # bar type
  theme_bw() +
  theme(text = element_text(colour = "#696969", family = "serif", size = 14),
        axis.title = element_text(size=18), # Labels axis font size
        axis.text = element_text(size=14, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) + 
  scale_fill_manual( values = c("gray","#C02F39")) + #color of fill
  labs( x = "log(time)",
        y = "Frequency") # labels))

# t testing whether the treatment had any impact on the time spend on forecasting
t.test.int.time <-  t.test(SPFT$time.fq.sec.log[SPFT$Group == "Treatment"],
       SPFT$time.fq.sec.log[SPFT$Group == "Control"], 
       alternative = "greater", var.equal = TRUE,
       conf.level = 0.95)

t.test.intervention.time <- paste(round(t.test.int.time[[5]][1],2),
                                  " > ",
                                  round(t.test.int.time[[5]][2],2),
                                  ", t(", t.test.int.time[[2]], ") = ",
                                    round(t.test.int.time[[1]],2),
                                    ", p < ",
                                    ifelse(round(t.test.int.time[[3]],4)< 0.001,
                                           0.001,round(t.test.int.time[[3]],4)),   
                                    sep = "")


###############################################################################
# 7. Aggregating Forecasts
###############################################################################

# Mean Estimates for each question ############################################
probs.mean <- SPFT %>% select(starts_with("fq")) %>% colMeans()

# mean brier score for average probabilities (unweighted)
bs.mean <- round(mean(brierScore(probs.mean, FQ[,4])),2)

# testing average brier score vs. brier score of averaged forecasts
t.test.bs.avg.mean <- t.test(SPFT$brier.avg, mu = bs.mean, alternative = "greater")

# string for paper 
t.test.bs.avg.mean  <- paste("t(", t.test.bs.avg.mean[[2]], ") = ",
                             round(t.test.bs.avg.mean[[1]],2),
                             ", p < ",
                             ifelse(round(t.test.bs.avg.mean[[3]],4)< 0.001,
                                    0.001,round(t.test.bs.avg.mean[[3]],4)),   
                             sep = "")

# 0. Subsample of (super)forecasters ##########################################

# drop BNT score 0,1 and 25% lowest share of time spend on forecasting
probs.mean.agg0 <- SPFT %>% 
  filter(bnt.s > 2 & time.fq.sec.log > summary(SPFT$time.fq.sec.log)[3]) %>% 
  select(starts_with("fq")) %>% colMeans()

# brier score of the aggregated forecasts from the sub group.
bs.cutoff.mean <- round(mean(brierScore(probs.mean.agg0, FQ[,4])),2)

# average brier score of the sub group
bs.agg0 <- 0
for(i in 1:nrow(FQ)){
  bs.agg0[i] <- SPFT %>% 
  filter(bnt.s > 1 & time.fq.sec.log > summary(SPFT$time.fq.sec.log)[2]) %>%
  dplyr::select(contains(paste("fq",i,"_1", sep= ""))) %>%
  brierScore(as.numeric(FQ[i,4])) %>% as.vector() %>% mean()
}

# mean brier score of group smaller group
bs.cutoff <- round(mean(bs.agg0),2)

# 1. computing individual weights #############################################

SPFT.agg1 <- SPFT %>% select(starts_with("fq"), bnt.s,mct.c, time.fq.sec.log,
                             Group, brier.avg) %>%
                      filter(!is.na(time.fq.sec.log))

# pre-calculations to see correlations
# lm(brier.avg ~ bnt.s, data = SPFT.agg1)
# lm(brier.avg ~ time.fq.sec.log, data = SPFT.agg1)
reg.brier.bnt.time <- lm(brier.avg ~ bnt.s + time.fq.sec.log, data = SPFT.agg1)

# SPFT.agg1$d <-  0
# SPFT.agg1$d[SPFT.agg1$Group == "Treatment"] <- 1
# lm(brier.avg ~ bnt.s + mct.c + time.fq.sec.log + d, data = SPFT.agg1)

# weightening using only BNT score
# simplest version: weight = score
# SPFT$w.bnt <- SPFT$bnt.s
probs.mean.w.bnt <- apply(select(SPFT.agg1, starts_with("fq")), 2,
      weighted.mean, w = SPFT.agg1$bnt.s)

mean(brierScore(probs.mean.w.bnt, FQ[,4]))

# weightening using only time.fq.sec.log score
probs.mean.w.time <- apply(select(SPFT.agg1, starts_with("fq")), 2,
                           weighted.mean, w = SPFT.agg1$time.fq.sec.log)
mean(brierScore(probs.mean.w.time, FQ[,4]))

# weightening using bnt and time.fq.sec.log score #############################

# contruct weigts (other weights possible)
# SPFT.agg1$w.bnt.time <- reg.brier.bnt.time[[1]][2]*SPFT.agg1$bnt.s +
#                        reg.brier.bnt.time[[1]][3]*SPFT.agg1$time.fq.sec.log

SPFT.agg1$w.bnt.time <- SPFT.agg1$bnt.s * SPFT.agg1$time.fq.sec.log

# weighted probabilties
probs.mean.w.bnt.time <- apply(select(SPFT.agg1, starts_with("fq")),
                               2, weighted.mean, w = SPFT.agg1$w.bnt.time)

# brier score of weigthed probabities
bs.mean.w.bnt.time <- round(mean(brierScore(probs.mean.w.bnt.time, FQ[,4])),2)

# 1.b BNT, MCT and time weightening ###########################################

# data selection
SPFT.agg1b <- SPFT %>% select(starts_with("fq"), bnt.s,mct.c, time.fq.sec.log,
                              Group, brier.avg) %>% 
  filter(!is.na(time.fq.sec.log) & !is.na(mct.c))

# compute weight
SPFT.agg1b$w.bnt.time.mct <- SPFT.agg1b$bnt.s * SPFT.agg1b$mct.c *SPFT.agg1b$time.fq.sec.log

# weighted probabilities
probs.mean.w.bnt.time.mct <- apply(select(SPFT.agg1b, starts_with("fq")),
                                   2, weighted.mean, w = SPFT.agg1b$w.bnt.time.mct)

# brier score
bs.mean.w.bnt.time.mct <- round(mean(brierScore(probs.mean.w.bnt.time.mct, FQ[,4])),2)
rm(SPFT.agg1b)

# 2. extremizing  #############################################################
# like Satopaa et al 2014

#create data.frame for extremizing (remove 0 and 1 as logit will cause problems)
SPFT.agg2 <- SPFT %>% select(starts_with("fq")) %>% as.matrix() 
SPFT.agg2[SPFT.agg2 == 0] <- 0.001
SPFT.agg2[SPFT.agg2 == 1] <- 0.999

# function to create extremized means for each question
# possible to include geometric weights (q^w) -> check theory
probsLogitExtrem <-function(p,a){
  q<-p/(1-p)
  geo.mean<-prod(q^(1/length(q))) # every element to the power of N?
  mod.prob<-(geo.mean^a)/(1+geo.mean^a)
  return(mod.prob)
}

# true outcomes 
Z<- as.vector(as.matrix(FQ[,4]))

# brier score average 
brierScoresAvg<-function(p,z){
  return(sum((p-z)^2)/length(p) + sum((z-p)^2)/length(p))
}

# optimize brier score average function to find minimizing a (bias correction)
bias.a <- optimise(function(a) brierScoresAvg(apply(SPFT.agg2,
                                                    2,
                                                    function(z) probsLogitExtrem(z, a)),
                                              Z),
                   interval=c(0,10))

# plot brierscore depending on bias correction
a<-seq(-1,5,by=0.1)
plot(a,sapply(a,
              function(l) brierScoresAvg(apply(SPFT.agg2,
                                               2,
                                               function(z) probsLogitExtrem(z, l)),
                                         Z)),
     type = "l",lwd=2, col="#C02F39",
     xlab="Systematic Bias a",
     ylab="Brier score")
brier.ext.plot <- recordPlot()

# p <- ggplot(data = data.frame(a = 0), mapping = aes(x = a))
# fun.1 <- function(a) a^2 + a
# fun.2 <- function(l) brierScoresAvg(apply(SPFT.agg2,2,function(z) probsLogitExtrem(z, l)),Z)
# p + stat_function(fun = fun.1) + xlim(-5,5)
# p + stat_function(fun = fun.2) + xlim(0,5)
# compute aggregated probabilities
probs.extrem<-apply(SPFT.agg2,2,function(z) probsLogitExtrem(z, a=bias.a$minimum))

# plot simple means against extremized values
plot(probs.mean, probs.extrem, asp=1,ylim=c(0,1),xlim=c(0,1))
abline(0,1,lty=2)
# points(Z,probs.mean,col=2,pch=16)

# 3. combining aggregation and weightening ###################################




# 4. displaying aggregated forecasts #########################################

# plot with aggregated probabilties
# add: labels for vlines
# vlines at the wrong place, e.g. for 13 the extremized value is above .5
agg.plot <- function(q) {
ggplot(SPFT, aes(x=eval(parse(text = fq[q])))) +
    geom_density(alpha=.3, fill="#C02F39") +
    geom_vline(data=SPFT,
               aes(xintercept=mean(eval(parse(text = fq[q])))), 
               linetype="dashed", size=1.5) + # group average
    # vertical line for dropping case
    geom_vline(data=filter(SPFT, bnt.s > 1 &
                           time.fq.sec.log > summary(SPFT$time.fq.sec.log)[2]),
               aes(xintercept=mean(eval(parse(text = fq[q])))), 
               linetype="dashed", size=1.5, color = "yellow") + # group average
    # vertical line for weighted probabilities
    geom_vline(aes(xintercept=probs.mean.w.bnt.time[q]), 
               linetype="dashed", size=1.5, color = "orange") + # group average
    # vertical line for extremized probability
    geom_vline(aes(xintercept=probs.extrem[q]), 
               linetype="dashed", size=1.5, color = "red") + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE),
                        paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    expand_limits(x=c(0,1)) + # set range of x-axis
    theme_bw() +
    scale_x_continuous(labels=percent) # percentages
}

agg.plot(13)
###############################################################################
# 8. presentation forecasts
###############################################################################

# plot scores distributed by groups
brierPlot <- function(x){ggplot(SPFT, aes(x = brier.avg, fill = x)) +
  geom_density(alpha = 0.3) +
  #geom_histogram(binwidth=.05, position="dodge", fill = "#C02F39") + # bar type
  theme_bw() +
  theme(axis.title = element_text(size=18), # Labels axis font size
        axis.text = element_text(size=14)) +  
  labs( title = paste("Brier score distribution by ", substring(deparse(substitute(x)), 6)),
    x = "Brier score",
    y = "Frequency") # labels))
}


# formulars for trying out
# brierPlot(as.factor(SPFT$team))
# summary(as.factor(SPFT$team))
