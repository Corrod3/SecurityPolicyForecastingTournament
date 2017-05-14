### Script for poster plots

# Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# load main script
source("main.R")


# save brier score distribution
ggsave(filename="plots/brierPlot.pdf", plot=brier.plot)

# BNT Scores distribution

# Distribution of bnt scores
bnt.plot2 <- ggplot(SPFT, aes(x = bnt.s)) + 
  geom_bar(fill = "#C02F39") +
  theme_bw() +
  theme(axis.title = element_text(size=22, colour = "#696969"), # Labels axis font size
        axis.text = element_text(size=18, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969"),
        title = element_text(size=24, colour = "#696969")) +  
  labs(title = "Distribution of Berlin Numeracy Test results",
    x = "BNT Score (# of correct answers)",
    y = "# of respondents") # labels


ggsave(filename="plots/bntPlot.pdf", plot=bnt.plot2)

# Correlation Mct-Brier Scores
ggsave(filename="plots/corBrierMctPlot.pdf", plot=cor.brier.mct.plot)

# Correlation Brier log(time) correlation
ggsave(filename="plots/corBrierTimeLogPlot.pdf", plot=cor.brier.time.log.plot)

# Correlation Brier linear time
ggsave(filename="plots/corBrierTimePlot.pdf", plot=cor.brier.time.plot)

# Hypothesis 3 time
ggsave(filename="plots/hypo3TimePlot.pdf", plot=hypo3.time.plot)


