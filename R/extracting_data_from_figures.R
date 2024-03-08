#devtools::install_github("daniel1noble/metaDigitise")
library(metaDigitise)
library(dplyr)

data <- metaDigitise(dir = "~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Extracting data from figures/")

#helpful webpage: https://cran.r-project.org/web/packages/metaDigitise/vignettes/metaDigitise.html

data 

data%>%filter(filename=="Martin et al. 2014.png")
data%>%filter(filename=="Veenstra & Byrne 1999 SE.png")
data%>%filter(filename=="Ward & Weatherhead 2005.png")
data%>%filter(filename=="Williams & Rabenold 2005 SD?.png")