###############################################################################
## Script for illustrations on the website
## by: Alexander
###############################################################################

###############################################################################
# Content
# 1. Preparation
# 2. Plots
###############################################################################

###############################################################################
# 1. Preparation
###############################################################################

source("main.R")

###############################################################################
# 2. Plots
###############################################################################

# density plot for github page  
response.web <- function(q){
  ggplot(FO.plot, aes(x=eval(parse(text = fq[q])))) +
    geom_density(alpha=.3, fill="blue") +
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "all"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 80, simplify=FALSE), paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    theme(axis.text = element_text(size = 12),
          axis.title=element_text(size=13),
          plot.title = element_text(size=14),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey"),
          aspect.ratio=2/4) + # axis + background
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("blue")) + # legend mean vline
    expand_limits(x=c(0,1)) + # set range of x-axis
    scale_x_continuous(labels=percent) # percentages
}

response.web(12)

response.web2 <- function(q){
  ggplot(FO.plot, aes(x=eval(parse(text = fq[q])))) +
    geom_density(alpha=.3, fill="blue") +
    geom_vline(data=filter(FO.plot2, 
                           fq.id == paste("fq",as.character(q), sep = "") & 
                             part.group == "all"),
               aes(xintercept=mean,  colour=part.group), 
               linetype="dashed", size=1.5) + # group average
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 50, simplify=FALSE), paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey")) + # axis + background
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("blue")) + # legend mean vline
    expand_limits(x=c(0,1)) + # set range of x-axis
    scale_x_continuous(labels=percent) # percentages
}

response.web2(12)