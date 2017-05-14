### Script for poster plots ###################################################

# Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# load main script
source("main.R")

### Tournament section ########################################################

response.poster <- function(q){
  ggplot(FO.plot, aes(x=eval(parse(text = fq[q])))) +
    geom_density(alpha=.3, fill="#C02F39") +
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "all"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 70, simplify=FALSE),
                        paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    theme(axis.text = element_text(size = 13),
          axis.title=element_text(size=14),
          plot.title = element_text(size=18),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey"),
          aspect.ratio=2/4) + # axis + background
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("#C02F39")) + # legend mean vline
    expand_limits(x=c(0,1)) + # set range of x-axis
    scale_x_continuous(labels=percent) + # percentages
    geom_vline(xintercept=as.numeric(FQ[q,4]),
               colour="#696969", 
               linetype="dashed", size=1.5,
               show.legend = TRUE) # correct answer 
}

response.poster(8)

ggsave(filename="plots/reponseFQ8.pdf", plot=response.poster(8), device=cairo_pdf)
ggsave(filename="plots/reponseFQ9.pdf", plot=response.poster(9), device=cairo_pdf)
ggsave(filename="plots/reponseFQ16.pdf", plot=response.poster(16), device=cairo_pdf)
ggsave(filename="plots/reponseFQ13.pdf", plot=response.poster(13), device=cairo_pdf)
ggsave(filename="plots/reponseFQ6.pdf", plot=response.poster(6), device=cairo_pdf)


# Brier distribution
brier.plot2 <- ggplot(SB, aes(x = brier.avg)) +
  geom_histogram(binwidth=.05, position="dodge", fill = "#C02F39") + # bar type
  theme_bw() +
  theme(axis.title = element_text(size=18, colour = "#696969"), # Labels axis font size
        axis.text = element_text(size=14, colour = "#696969"),
        axis.line = element_line(colour = "#696969"), 
        axis.ticks = element_line(colour = "#696969")) +  
  labs( # title = "Brier score distribution",
    x = "Brier score",
    y = "Frequency") # labels))

# save brier score distribution
ggsave(filename="plots/brierPlot.pdf", plot=brier.plot2, device=cairo_pdf)


### Research section ##########################################################

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


ggsave(filename="plots/bntPlot.pdf", plot=bnt.plot2, device=cairo_pdf)

# Correlation Mct-Brier Scores
ggsave(filename="plots/corBrierMctPlot.pdf", plot=cor.brier.mct.plot, device=cairo_pdf)

# Correlation Brier log(time) correlation
ggsave(filename="plots/corBrierTimeLogPlot.pdf", plot=cor.brier.time.log.plot, device=cairo_pdf)

# Correlation Brier linear time
ggsave(filename="plots/corBrierTimePlot.pdf", plot=cor.brier.time.plot, device=cairo_pdf)

# Hypothesis 3 time
ggsave(filename="plots/hypo3TimePlot.pdf", plot=hypo3.time.plot, device=cairo_pdf)


