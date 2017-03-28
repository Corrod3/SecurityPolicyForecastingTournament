###############################################################################
## File for extracting data, cleaning and calculating scores
## by: Alexander Sacharow
###############################################################################

###############################################################################
# CONTENT
# 0. Preparations
# 1. Get Data
# 2. Clean data
# 3. MTurk
# 4. Forecast summary
# 5. Scoreboard
# 6. control variables
# 7. demographics
# 8. testing
###############################################################################

###############################################################################
# ToDo
# 1.
###############################################################################

###############################################################################
# 0. Preparations
###############################################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "plyr" ,"dplyr", "ggplot2", "reshape2", "scales", "stargazer")
# package and why it is needed
# readxl: import excel files
# plyr: mapvalues function
# dyplyr: data manipulation
# ggplot: plots (e.g. density)
# reshape2: melt function
# scales: label transformation in ggplot

# install packages if not installed before
for (p in packages) {
  if (p %in% installed.packages()[,1]) {
    require(p, character.only=T)
  }
  else {
    install.packages(p, repos="http://cran.rstudio.com", dependencies = TRUE)
    require(p, character.only=T)
  }
}
rm(p, packages)

###############################################################################
# 1. Import Data
###############################################################################

# Import responses
SPFT.Main <- read.csv2("raw/SPFT-20170217.csv", sep = ",")
SPFT.MTurk <- read.csv2("raw/SPFT_MTurk_20170210.csv", sep = ",")


# Import realized outcomes
FQ <- read_excel("raw/SPFT-questions-test.xlsx")

# import MTurk batch results
MTurk.batch <- read.csv2("raw/Batch_2677782_batch_results.csv", sep = ",")
MTurk.batch <- MTurk.batch %>% select(WorkerId, WorkTimeInSeconds, 
                                      Answer.surveycode, Approve, Reject)

###############################################################################
# 2. Clean Data
###############################################################################

# Merge SPFT and SPFT Turk (identify Hertie later)
SPFT <- dplyr::bind_rows(SPFT.Main[-(1:2),], SPFT.MTurk[-(1:2),], .id = "part.group")
SPFT[SPFT$part.group == 1,]["part.group"] <- "other"
SPFT[SPFT$part.group == 2,]["part.group"] <- "mturk"

# Remove unfinished surveys
SPFT <- SPFT %>% dplyr::filter(Finished == "True")

# identify hertie students
SPFT[!is.na(SPFT$id.hertie) & SPFT$id.hertie != "",][,"part.group"] <- "hertie"

# Change variable types #######################################################

# Dates + times
SPFT$StartDate <- as.Date(as.character(SPFT$StartDate))
SPFT$EndDate <- as.Date(as.character(SPFT$EndDate))
SPFT$Duration..in.seconds. <- as.numeric(SPFT$Duration..in.seconds.)
SPFT$time.sec2_Page.Submit <- as.numeric(SPFT$time.sec2_Page.Submit)

# make estimates numeric
fq <- colnames(select(SPFT, contains("fq")))
SPFT[,fq] = apply(SPFT[,fq], 2, function(x) as.numeric(as.character(x)))

# transform estimates to percentage
SPFT[,fq] <- SPFT[,fq]/100

# bnt responses to numeric
bnt.col <- colnames(select(SPFT, contains("bnt")))
SPFT[,bnt.col] = apply(SPFT[,bnt.col], 2, function(x) as.numeric(as.character(x)))

# mct responses to numeric
mct.col <- colnames(select(SPFT, contains("mct.")))
SPFT[,mct.col] = apply(SPFT[,mct.col], 2, function(x) as.numeric(as.character(x)))

# column names only pro + con arguments
mct.col.pc <- mct.col[-c(1,14)]
names(SPFT)[names(SPFT)=="mct.w_1"] <- "mct.w"
names(SPFT)[names(SPFT)=="mct.d_1"] <- "mct.d"

# demographic variables to factor

SPFT$sex <- as.factor(SPFT$sex)

# Remove not needed data ######################################################

# Remove test surveys
SPFT <- SPFT %>% filter(StartDate > "2017-02-05")

# Drop empthy forecasts (Mainly MTurk failed attention check)
SPFT <- SPFT %>% filter(time.sec2_First.Click != "")

# delete unnecessary information
SPFT <- SPFT %>% select(-Status, -Progress, -Finished,
                        -RecipientLastName, -RecipientFirstName, 
                        -RecipientEmail, -LocationLatitude, -LocationLongitude,
                        -DistributionChannel, -StartDate, -ExternalReference,
                        -further.info...Topics, -contains("Click") )
# clean FQ file
FQ <- FQ %>% filter(!is.na(fq.id))

# delete raw files
rm(SPFT.Main, SPFT.MTurk)

###############################################################################
# 3. Identify problematic MTurk users & removing them
###############################################################################

# only MTurk
MTurk <- SPFT %>% filter(part.group == "mturk")

# Criterion 1: Check failed attention checks
MTurk$c1 <- 0
MTurk[MTurk$att.check != 48,]$c1 <- 1

# Criterion 2: All BNT questions wrong
MTurk <- MTurk %>% 
          mutate(c2 = ifelse(bnt1 != 30 & bnt2 != 25 & bnt3 != 20 & bnt4 != 50,
                             1, 0))

# Criterion 3: Short time
# summary(MTurk$Duration..in.seconds.)
MTurk <- MTurk %>% mutate(c3 = ifelse(Duration..in.seconds. < 600, 1, 0))

# Criterion 4: Doublication in unique identifiers (IP, email, MTurk ID)
MTurk$c4.ip <- 0
MTurk$c4.email <- 0

email.double <- data.frame(table(MTurk$personal.report))
# email.double[email.double$Freq > 1,]
MTurk[MTurk$personal.report %in% 
        email.double$Var1[email.double$Freq > 1 & email.double$Freq < 10],][,"c4.email"] <- 1

IP.double <- data.frame(table(MTurk$IPAddress))
# IP.double[IP.double$Freq > 1,]
MTurk[MTurk$IPAddress %in% IP.double$Var1[IP.double$Freq > 1],][,"c4.ip"] <- 1

# merge dataframe by completion code
MTurk <- merge(MTurk, MTurk.batch, by.x = "CompletionCode",
               by.y = "Answer.surveycode", all = TRUE)

# check WorkerID code
MTurk$WorkerId <- trimws(as.character(MTurk$WorkerId))
MTurk$id.mturk <- trimws(as.character(MTurk$id.mturk))
MTurk$compWorkID <- ifelse(MTurk$WorkerId == MTurk$id.mturk, 1, 0)

# compute total control fails
MTurk$c.total <- MTurk$c1 + MTurk$c2 + MTurk$c3 + MTurk$c4.ip + MTurk$c4.email

MTurk.batch <- merge(MTurk.batch, MTurk[,c("ResponseId", "id.mturk", 
                                           "CompletionCode", "c1", "c2", "c3",
                                           "c4.ip", "c4.email", "c.total")],
                     by.x = "Answer.surveycode",
                     by.y = "CompletionCode")

# Reject if attention check fail and (indication for double user or 3 total control fails)
MTurk.batch[MTurk.batch$c1 == 1 & MTurk.batch$c4.email == 1, "Reject"] <- TRUE
MTurk.batch[MTurk.batch$c1 == 1 & MTurk.batch$c4.ip == 1, "Reject"] <- TRUE
MTurk.batch[MTurk.batch$c1 == 1 & MTurk.batch$c.total > 2, "Reject"] <- TRUE

# View rejected Mturk users
# View(MTurk.batch %>% filter(Reject == T)) 

# Removing rejected MTurks from sample
SPFT <- merge(SPFT, MTurk.batch[,c("ResponseId","Reject")], by = "ResponseId",  
              all.x = TRUE)
SPFT <- SPFT %>% filter(is.na(Reject))

# cleaning
rm(IP.double, email.double, MTurk, MTurk.batch)

###############################################################################
# 4. Forecasting summaries
###############################################################################

# List of Hertie Students
hertie <- SPFT %>% filter(part.group == "hertie") %>% select(id.hertie)

# export list
# write.table(hertie, "D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament/hertiepart.txt", sep="\t") 
write.csv(hertie, "hertiepart.csv", row.names=FALSE) 

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
  labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE), paste, collapse="\n" ),
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
  labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE), paste, collapse="\n" ),
       x = "What is the probability of this event to happen?",
       y = "Distribution of estimates") + # labels
  expand_limits(x=c(0,1)) + # set range of x-axis
  scale_x_continuous(labels=percent) # percentages
}
# test plot
# response.all(6)

# density plot for html presentation  
response.hertie <- function(q){
  ggplot(filter(FO.plot, part.group == "hertie"), aes(x=eval(parse(text = fq[q])), fill=part.group)) +
    geom_density(alpha=.3) +
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "hertie"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE), paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("red")) + # legend mean vline
    expand_limits(x=c(0,1)) + # set range of x-axis
    scale_x_continuous(labels=percent) # percentages
}
# test plot
# response.hertie(2)

response.hertie2 <- function(q){
  ggplot(filter(FO.plot, part.group == "hertie"), aes(x=eval(parse(text = fq[q])), fill=part.group)) +
    geom_histogram(binwidth=.10, position="dodge") + # bar type
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "hertie"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE), paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "# of estimates") + # labels
    expand_limits(x=c(0,1)) + # set range of x-axis
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("red")) + # legend mean vline
    scale_x_continuous(labels=percent) # percentages
}
# test plot
# response.hertie2(2)

###############################################################################
# 5. Score board
###############################################################################

# recode FQ to binary
FQ[,4] <- 0
FQ[FQ[,3] == "yes", 4] <- 1
colnames(FQ) <- c(colnames(FQ)[1:3], "out")

# score board data
SB <- SPFT  %>% select(ResponseId, id.hertie, id.other, id.mturk,
                       starts_with("fq")) 

# transform responses to percentages
# SB[,-(1:4)] <- sapply(sapply(SB[,-(1:4)],as.character),as.numeric)
# SB[,-(1:4)] <- SB[,-(1:4)]/100

## calculate brier scores for each question/respondent

# number of questions
q.num <- ncol(SB) -4

#i <- 1
for(i in 1:q.num){
tmp <- paste("fq", i, sep = "")
# add outcome in new brier score column (from question xlsx)
SB[,paste(tmp,"bs", sep = ".")] <- as.numeric(FQ[FQ[,1] == tmp, 4])
# compute difference outcome and quess 
SB[,paste(tmp,"tmp1", sep = ".")] <- select(SB, i+4+q.num) - select(SB, i+4)
# compute difference outcome  and counterfactual
SB[,paste(tmp,"tmp2", sep = ".")] <- (1 - select(SB, i+4+q.num)) - (1- select(SB, i+4))
# Square differences and sum them
SB[,paste(tmp,"bs", sep = ".")] <- (SB[,paste(tmp,"tmp1", sep = ".")])^2 +
                                   (SB[,paste(tmp,"tmp2", sep = ".")])^2 
# delete unneccessary columns
SB <- SB %>% select(-contains("tmp"))
rm(tmp)
}

# Compute average brier score for each respondent
SB[,"brier.avg"] <- rowMeans(select(SB, contains("bs")))

# Sort by brier score 
SB <- SB %>% arrange(brier.avg)

brier.plot <- ggplot(SB, aes(x = brier.avg)) +
     geom_histogram(binwidth=.05, position="dodge") + # bar type
     theme_bw() +
     labs(title = "Brier score distribution",
          x = "Brier score",
          y = "Frequency") # labels))

# Hertie score board
SB.Hertie <- SB %>% filter(!is.na(id.hertie) & id.hertie != "")

# remove late replies from score board (for testing)
SB <- SB[!SB$ResponseId %in% SPFT$ResponseId[SPFT$EndDate >= "2017-02-13"],]

###############################################################################
# 6. Computing control variables and testing
###############################################################################

# remove late submissions
SPFT <- SPFT %>% filter(EndDate < "2017-02-13")

# merge brier scores in
SPFT <- merge(SPFT, SB[, c("ResponseId", "brier.avg")], by = "ResponseId")

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
plot(SPFT$bnt.s, SPFT$brier.avg, main="Scatterplot BNT Score & Brier Scores",
     xlab="BNT Score", ylab="Brier Scores")
abline(lm(SPFT$brier.avg~SPFT$bnt.s), col="red")

# Distribution of bnt scores
bnt.plot <- ggplot(SPFT, aes(x = bnt.s)) + 
  geom_bar() +
  theme_bw() + 
  labs(title = "Distribution of results Berlin Numeracy Test (BNT)",
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

# calculating total sum (3)
SPFT$mct.ts <- rowSums(SPFT[, mct.col.pc])

# calculate ss_m (6)
SPFT$mct.ss.m <- SPFT$mct.ts*SPFT$mct.ts/24

# calculate sums of squares (5)
SPFT$mct.tss <- rowSums((SPFT[, mct.col.pc])^2)

# sums of stage squares (2)
SPFT$mct.sss <- 0

for(i in 1:6) {
SPFT$mct.sss <- SPFT$mct.sss +  
                rowSums(SPFT[, grep(paste("mct.*._",i,sep = ""), names(SPFT))]) *
                rowSums(SPFT[, grep(paste("mct.*._",i,sep = ""), names(SPFT))])  
}

# calculate competency score
SPFT$mct.c <- (SPFT$mct.sss/4 - SPFT$mct.ss.m)/(SPFT$mct.tss - SPFT$mct.ss.m)

# Distribution of moral competency score
mct.plot <- ggplot(filter(SPFT, is.na(mct.c) == F), aes(x = mct.c)) +
  geom_histogram(binwidth=.05, position="dodge") + # bar type
  labs(title = "Moral Competency Test (MCT)",
     x = "MCT Score",
     y = "# of respondents") + # labels
  expand_limits(x=c(0,1)) # set range of x-axis

# summary(SPFT$mct.c)
# str(filter(SPFT, part.group == "hertie"))

# timeing #####################################################################

# code to minutes
SPFT$Duration.min <- SPFT$Duration..in.seconds./60
SPFT$time.fq.sec <- SPFT$time.sec2_Page.Submit/60

levels(as.factor(SPFT$time))
SPFT$time.min <- as.numeric(mapvalues(SPFT$time,
                                      levels(as.factor(SPFT$time)),
                                      c(90,20,180,45,5)))

# Basic Scatterplot Matrix for time (to compare correlation)
# extreme outliers are eliminated
# actual time one forecasting question should be used as 
# full duration and self reported data captures time spend on other sections
pairs(~Duration.min+time.fq.sec+time.min,data=filter(SPFT, time.fq.sec < 1000),
      main="Simple Scatterplot Matrix")

time.plot <- ggplot(filter(SPFT, time.fq.sec < 1000), 
                    aes(x = time.fq.sec, fill = part.group)) +
                geom_density(alpha=.3) +
                labs(title = "Distribution time spend on forecasting questions",
                     x = "Time in min",
                     y = "Share of respondents") # labels

# make extreme time as missing data
SPFT$time.fq.sec[SPFT$time.fq.sec > 1000] <- NA


# team ########################################################################

summary(SPFT$team)

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
# 7. demographic data - descriptives
###############################################################################

# SPFT.Demo.Plot <- SPFT %>% select(year, sex)

# correct typos
filter(SPFT, year < 1900 | year > 2017)
SPFT$year[SPFT$year=="19.061990"] <- 1990
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
                geom_bar(data = subset(SPFT.Demo.Plot, sex == "Female"), stat = "identity") +
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

# self-assessment ############################################################
# order 
SPFT$selfassessment <- factor(SPFT$selfassessment, 
                         levels =  c("Extremely bad", "Moderately bad",
                                     "Slightly bad", "Neither good nor bad", 
                                     "Slightly good", "Moderately good",
                                     "Extremely good"))

levels(as.factor(SPFT$selfassessment))

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
stargazer(select(SPFT, bnt.s, mct.c, time.fq.sec, Duration.min, age),
          type="html", out = "DescStat.html")

###############################################################################
# 8. Testing
###############################################################################

# Skill vs. Luck ##############################################################

# all possible combinations of 0,1 in a matrix
# for 24 events there are more than 16 Mio: 2^24
 
# idea: take expected value of each question to calculate random distribution
# brier score

# score board for randum distributed events
SB.R <- SPFT  %>% select(ResponseId,
                       starts_with("fq")) 

## calculate brier scores for each question/respondent

for(i in 1:q.num){
  tmp <- paste("fq", i, sep = "")
  # add expected value in new brier score column and compute Brier score
  SB.R[,paste(tmp,"bs", sep = ".")] <- 2*(0.5 - select(SB.R, i+1))^2
  rm(tmp)
}

# Compute average brier score for each respondent
SB.R[,"brier.avg"] <- rowMeans(select(SB.R, contains("bs")))

hist(SB.R$brier.avg)

# T-Statistik (root(n*m/(n+m)*(x_m-y_m)/sd(S)))
# https://de.wikipedia.org/wiki/Zweistichproben-t-Test
(nrow(SB.R)*nrow(SPFT)/(nrow(SB.R)+nrow(SPFT)))^(1/2)*(mean(SB.R$brier.avg) - mean(SPFT$brier.avg))/
  ((((nrow(SPFT)-1)*sd(SPFT$brier.avg)^2 + (nrow(SB.R)-1)*sd(SB.R$brier.avg)^2)/(nrow(SPFT)+nrow(SB.R)-2)))^(1/2)

# T-Test One-sided: Whether brier score for true events is smaller than for random events
# http://statistics.berkeley.edu/computing/r-t-tests
t.test(SPFT$brier.avg, y=SB.R$brier.avg, mu = 0, alternative = "less",  paired=F, conf.level=0.95)

# or should the test in this case be paired? 
t.test(SPFT$brier.avg, y=SB.R$brier.avg, mu = 0, alternative = "less",  paired=T, conf.level=0.95)

# alternative skills vs. luck test: correct side of 50% #######################

# recode FQ to binary

# score board for correct side
SB.CS <- SPFT  %>% select(ResponseId, id.hertie, id.other, id.mturk,
                       starts_with("fq")) 

## calculate correct side scores for each question/respondent
# i <- 1
 for(i in 1:q.num){
  tmp <- paste("fq", i, sep = "")
  # add outcome with 1 of on correct side of 50% and 0 if not 
  SB.CS[,paste(tmp,"cs", sep = ".")] <- 
    ifelse(abs(as.numeric(FQ[FQ[,1] == tmp, 4]) - select(SB.CS, i+4)) > 0.5,0,1)
  rm(tmp)
}
# individual share of being on the correct side with the forecast
SB.CS[,"cs.avg"] <- rowMeans(select(SB.CS, contains("cs")))

# One-sided T-Testing correct side measure
t.test(SB.CS$cs.avg, mu=0.5, alternative = "greater", conf.level = 0.95)





