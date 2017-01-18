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
SPFT <- read.csv2("raw/SPFT-20170118.csv", sep = ",")

# Import realized outcomes
FQ <- read_excel("raw/SPFT-questions.xlsx")

################################################
# 2. Score board
################################################

# recode FQ to binary
FQ[,4] <- 0
FQ[FQ[,3] == "yes", 4] <- 1
colnames(FQ) <- c(colnames(FQ)[1:3], "out")

# score board data
SB <- SPFT %>% select(ResponseId, ID.Hertie, starts_with("fq"), -contains(".c"))
SB <- SB[-(1:2),]
# transform responses to percentages
SB[,-(1:2)] <- sapply(sapply(SB[,-(1:2)],as.character),as.numeric)
SB[,-(1:2)] <- SB[,-(1:2)]/100

## calculate brier scores for each question/respondent

#number of questions
q.num <- 19

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



