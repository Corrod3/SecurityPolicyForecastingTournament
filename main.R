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
###############################################################################

###############################################################################
# ToDo
# 1. Exclude Mturk rejects from sample  DONE
# 2. Restrict sample to Feb. 12 DONE
# 3. Export MTurk script NOT NEEDED
# 4. Hertie Response summary
# 5. Compute MCT and BNT scores
# 6. Upload aggregates forecasting responses 
#    a. Compute group means Hertie, Volunteers, MTurks
# 7. add group identifiers (hertie, other, mturk) DONE
###############################################################################



###############################################################################
# 0. Preparations
###############################################################################

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
SPFT <- dplyr::bind_rows(SPFT.Main, SPFT.MTurk[-(1:2),], .id = "part.group")
SPFT[SPFT$part.group == 1,]["part.group"] <- "other"
SPFT[SPFT$part.group == 2,]["part.group"] <- "mturk"

# Remove unfinished surveys
SPFT <- SPFT %>% dplyr::filter(Finished == "True")

# identify hertie students
SPFT[!is.na(SPFT$id.hertie) & SPFT$id.hertie != "",][,"part.group"] <- "hertie"

# Change variable types #######################################################

# Dates
SPFT$StartDate <- as.Date(as.character(SPFT$StartDate))
SPFT$EndDate <- as.Date(as.character(SPFT$EndDate))
SPFT$Duration..in.seconds. <- as.numeric(SPFT$Duration..in.seconds.)

# make estimates numeric
fq <- colnames(select(SPFT, contains("fq")))
SPFT[,fq] = apply(SPFT[,fq], 2, function(x) as.numeric(as.character(x)))

# transform estimates to percentage
SPFT[,fq] <- SPFT[,fq]/100

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

# Create data frame for aggregated forecasts
F <- FQ[,(1:2)]

# compute means for each question
fore.means <- t(aggregate(SPFT[,fq], list(part.group = SPFT$part.group), mean))

# rename columns to include mean and participants group
colnames(fore.means) =  paste(fore.means[1,], "mean", sep = ".")
fore.means = fore.means[-1, ]

# compute average of all (careful: how to ensure the proper order?)
fore.means <- cbind(fore.means, all.mean = colMeans(SPFT[,fq]))
fore.means <- data.frame(fq.id = gsub("_.*$","",row.names(fore.means)), fore.means, row.names = NULL)

# combine forecasting means with questions
F <- merge(F,fore.means, by = "fq.id")
rm (fore.means)

# compute standard deviations and add to data frame
fore.sd <- t(aggregate(SPFT[,fq], list(Part = SPFT$part.group), sd))
colnames(fore.sd) =  paste(fore.sd[1,], "sd", sep = ".")
fore.sd = fore.sd[-1, ]

# add column with standard deviations (could be in a different order?!)
fore.sd <- cbind(fore.sd, all.sd = apply(SPFT[,fq],2,sd))

fore.sd <- data.frame(fq.id = gsub("_.*$","",row.names(fore.sd)), fore.sd, row.names = NULL)
F <- merge(F,fore.sd, by = "fq.id")

# Compute histograms for each question


# create graphs

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
q.num <- 24

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
SB[,paste(tmp,"bs", sep = ".")] <- SB[,paste(tmp,"tmp1", sep = ".")] *
                                   SB[,paste(tmp,"tmp1", sep = ".")] +
                                   SB[,paste(tmp,"tmp2", sep = ".")] * 
                                   SB[,paste(tmp,"tmp2", sep = ".")]
# delete unneccessary columns
SB <- SB %>% select(-contains("tmp"))
rm(tmp)
}

# Compute average brier score for each respondent
SB[,"brier.avg"] <- rowMeans(select(SB, contains("bs")))

# Sort by brier score 
SB <- SB %>% arrange(brier.avg)


###############################################################################
# 6. Computing control variables and testing
###############################################################################

# remove late submissions
SPFT <- SPFT %>% filter( StartDate < "2017-02-13")