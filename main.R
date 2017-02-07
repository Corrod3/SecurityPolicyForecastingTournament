##################################################
## File for extracting data, cleaning and calculating scores
## by: Alexander Sacharow
##################################################

##################################################
# CONTENT
# 0. Preparations
# 1. Get Data
##################################################

##################################################
# 0. Preparations
##################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "dplyr")
# package and why it is needed
# readxl: import excel files
# dyplyr: data manipulation

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

################################################
# 1. Import Data
################################################

# Import responses
SPFT.Main <- read.csv2("raw/SPFT-20170207-backup.csv", sep = ",")
SPFT.MTurk <- read.csv2("raw/SPFT_MTurk_20170207.csv", sep = ",")


# Import realized outcomes
FQ <- read_excel("raw/SPFT-questions-test.xlsx")

# import MTurk batch results
MTurk.batch <- read.csv2("raw/Batch_2677782_batch_results.csv", sep = ",")
MTurk.batch <- MTurk.batch %>% select(id.mturk = WorkerId, WorkTimeInSeconds, Answer.surveycode)

###############################################
# 2. Clean Data
###############################################

# Merge SPFT and SPFT Turk
SPFT <- dplyr::bind_rows(SPFT.Main, SPFT.MTurk[-(1:2),])


# Remove unfinished surveys
SPFT <- SPFT %>% dplyr::filter(Finished == "True")

# Change variable types ########################

# Dates
SPFT$StartDate <- as.Date(as.character(SPFT$StartDate))
SPFT$EndDate <- as.Date(as.character(SPFT$EndDate))
SPFT$Duration..in.seconds. <- as.numeric(SPFT$Duration..in.seconds.)

# Remove test surveys
SPFT <- SPFT %>% filter(StartDate > "2017-02-05")

# Drop empthy forecasts (Mainly MTurk failed attention check)
SPFT <- SPFT %>% filter(time.sec2_First.Click != "")

# delete unnecessary information
SPFT <- SPFT %>% select(-Status, -IPAddress, -Progress, -Finished,
                        -RecipientLastName, -RecipientFirstName, 
                        -RecipientEmail, -LocationLatitude, -LocationLongitude,
                        -DistributionChannel, -StartDate, ExternalReference,
                        -further.info...Topics, -contains("Click") )
# delete raw files
rm(SPFT.Main, SPFT.MTurk)

####################################################
# Identify problematic MTurk users 
###################################################

# only MTurk
MTurk <- SPFT %>% filter(id.mturk != "")

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

MTurk.Prob <- MTurk %>% select(ResponseId, id.mturk, CompletionCode, c1, c2, c3) %>% filter(c1 == 1 & c2 == 1 & c3 == 1)

# merge dataframe
MTurk <- merge(MTurk, MTurk.batch, by = "id.mturk", all = TRUE)



################################################
# 2. Score board
################################################

# recode FQ to binary
FQ[,4] <- 0
FQ[FQ[,3] == "yes", 4] <- 1
colnames(FQ) <- c(colnames(FQ)[1:3], "out")

# score board data
SB <- SPFT %>% select(ResponseId, id.hertie, id.other, starts_with("fq"))
SB <- SB[-(1:2),]
# transform responses to percentages
SB[,-(1:3)] <- sapply(sapply(SB[,-(1:2)],as.character),as.numeric)
SB[,-(1:3)] <- SB[,-(1:3)]/100

## calculate brier scores for each question/respondent

#number of questions
q.num <- 24

# i <- 1
for(i in 1:q.num){
tmp <- paste("fq", i, sep = "")
# add outcome in new brier score column
SB[,paste(tmp,"bs", sep = ".")] <- as.numeric(FQ[FQ[,1] == tmp, 4])
# compute difference outcome and quess
SB[,paste(tmp,"tmp1", sep = ".")] <- select(SB, i+2+q.num) - select(SB, i+2)
# compute difference outcome  and counterfactual
SB[,paste(tmp,"tmp2", sep = ".")] <- select(SB, i+2+q.num) - (1- select(SB, i+2))
# Square differences and sum them
SB[,paste(tmp,"bs", sep = ".")] <- SB[,paste(tmp,"tmp1", sep = ".")]*SB[,paste(tmp,"tmp1", sep = ".")] +
                                   SB[,paste(tmp,"tmp2", sep = ".")]* SB[,paste(tmp,"tmp2", sep = ".")]
# delete unneccessary columns
SB <- SB %>% select(-contains("tmp"))
rm(tmp)
}

