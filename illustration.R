###############################################################################
## Script for illustrations on the website
## by: Alexander
###############################################################################

###############################################################################
# Content
# 1.
# 2.
###############################################################################

###############################################################################
# 1. Preparation
###############################################################################

source("main.R")

###############################################################################
# 2. Plot
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
    labs(title = sapply(strwrap(as.character(FQ[q,2]), 40, simplify=FALSE), paste, collapse="\n" ),
         x = "What is the probability of this event to happen?",
         y = "Distribution of estimates") + # labels
    guides(fill=guide_legend(title="Participants")) + # legend title
    scale_color_manual("Mean", values = c("red")) + # legend mean vline
    expand_limits(x=c(0,1)) + # set range of x-axis
    scale_x_continuous(labels=percent) # percentages
}

response.web(12)