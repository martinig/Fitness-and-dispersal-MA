#devtools::install_github("daniel1noble/metaDigitise")
library(metaDigitise)
library(dplyr)

data <- metaDigitise(dir = "~/Documents/Files/Manuscripts/Martinig et al. 2025 (meta analysis)/Extracting data from figures/")

#helpful webpage: https://cran.r-project.org/web/packages/metaDigitise/vignettes/metaDigitise.html

data 

data%>%filter(filename=="Reynolds et al. 2025femalenondis.png")
data%>%filter(filename=="Reynolds et al. 2025femaledis.png")
data%>%filter(filename=="Reynolds et al. 2025malenondis.png")
data%>%filter(filename=="Reynolds et al. 2025maledis.png")

data%>%filter(filename=="Veenstra & Byrne 1999 SE.png")
data%>%filter(filename=="Ward & Weatherhead 2005.png")
data%>%filter(filename=="Williams & Rabenold 2005 SD?.png")