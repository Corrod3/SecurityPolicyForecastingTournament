###############################################################################
## File for exporting plots to word
## by: Alexander Sacharow
###############################################################################

###############################################################################
# CONTENT
# 0. Preparations
###############################################################################

###############################################################################
# 0. Preparations
# 1. Plot export
###############################################################################

source("main.R")

packages <- c("ReporteRs")
# package and why it is needed
# ReporteRs: vector graphs for docx


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

##############################################################################
# 1. Plots to .docx
##############################################################################

# create .docx file and add plot to it
# instructions: http://stackoverflow.com/questions/28088544/export-plot-with-high-quality-from-r-to-word
doc = docx()
doc = addPlot( doc = doc, fun = print, x = brier.plot, 
               vector.graphic = T, # vector graphic instruction
               #               fontname = "Calibri",  # font specification
               width = 5, height = 3 #dim. are in inches
)
writeDoc( doc, file = "plots.docx" )
