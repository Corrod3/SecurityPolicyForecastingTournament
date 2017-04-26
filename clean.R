###############################################################################
## File for extracting data, cleaning and anonymizing data + MTurk 
## by: Alexander Sacharow
###############################################################################

###############################################################################
# 0. Preparations
###############################################################################

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# load packages / clean environment
source("prep.R")

###############################################################################
# 1. Import Data
###############################################################################

# Import responses
SPFT.Main <- read.csv2("raw/SPFT-20170217.csv", sep = ",")
SPFT.MTurk <- read.csv2("raw/SPFT_MTurk_20170210.csv", sep = ",")

# import MTurk batch results
MTurk.batch <- read.csv2("raw/Batch_2677782_batch_results.csv", sep = ",")
MTurk.batch <- MTurk.batch %>% select(WorkerId, WorkTimeInSeconds, 
                                      Answer.surveycode, Approve, Reject)

###############################################################################
# 2. Clean Data
###############################################################################

# Merge SPFT and SPFT Turk (identify students later)
SPFT <- dplyr::bind_rows(SPFT.Main[-(1:2),], SPFT.MTurk[-(1:2),], .id = "part.group")
SPFT[SPFT$part.group == 1,]["part.group"] <- "other"
SPFT[SPFT$part.group == 2,]["part.group"] <- "mturk"

# Remove unfinished surveys
SPFT <- SPFT %>% dplyr::filter(Finished == "True")

# rename id column
names(SPFT)[names(SPFT) == 'id.hertie'] <- 'id.uni'

# identify students
SPFT[!is.na(SPFT$id.uni) & SPFT$id.uni != "",][,"part.group"] <- "uni"

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
SPFT$selfassessment <- factor(SPFT$selfassessment, 
                              levels =  c("Extremely bad", "Moderately bad",
                                          "Slightly bad", "Neither good nor bad", 
                                          "Slightly good", "Moderately good",
                                          "Extremely good"))

# Remove not needed data ######################################################

# Remove test surveys
SPFT <- SPFT %>% filter(StartDate > "2017-02-05")

# Drop empthy forecasts (Mainly MTurk failed attention check)
SPFT <- SPFT %>% filter(time.sec2_First.Click != "")



# delete unnecessary information
SPFT <- SPFT %>% select(-Status, -Progress, -Finished,  
                        -RecipientLastName, -RecipientFirstName, -RecordedDate,
                        -RecipientEmail, -LocationLatitude, -LocationLongitude,
                        -DistributionChannel, -StartDate, -ExternalReference,
                        -further.info...Topics, -contains("Click") )
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
rm(IP.double, email.double, MTurk, MTurk.batch, bnt.col, fq, mct.col, mct.col.pc)

###############################################################################
# Exporting relevant data
###############################################################################



# List of Students
# uni <- SPFT %>% filter(part.group == "uni") %>% select(id.uni)

# export list
# write.csv(uni, "unipart.csv", row.names=FALSE) 

# export feedback, further info, 
write.csv(select(SPFT,  
                 ResponseId, part.group, id.uni, id.mturk, id.other, feedback, 
                 further.info, personal.report),
          "personalData.csv", row.names=FALSE) 

# anonmymizing the data
SPFT.clean <- SPFT %>% select(-id.uni, -id.other, -id.mturk, -further.info,
                              -personal.report, -feedback, -Reject, -IPAddress,
                              -att.check, -att.check2)

write.csv(SPFT.clean, "raw/SPFT-clean.csv", row.names=FALSE)