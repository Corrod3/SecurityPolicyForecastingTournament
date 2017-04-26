###############################################################################
## File for preparing R 
## by: Alexander Sacharow
###############################################################################

# Clear Global environment
rm(list=ls())

# Collect packages/libraries we need:
packages <- c("readxl", "plyr" ,"dplyr", "ggplot2", "reshape2", "scales", 
              "stargazer", "Hmisc", "xtable")
# package and why it is needed
# readxl: import excel files
# plyr: mapvalues function
# dyplyr: data manipulation
# ggplot: plots (e.g. density)
# reshape2: melt function
# scales: label transformation in ggplot
# Hmisc: Correlation table 
# xtable: create latex table

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