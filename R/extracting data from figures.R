#devtools::install_github("daniel1noble/metaDigitise")
library(metaDigitise)
library(dplyr)

data <- metaDigitise(dir = "~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Extracting data from figures/")

#helpful webpage: https://cran.r-project.org/web/packages/metaDigitise/vignettes/metaDigitise.html

data 

data%>%filter(filename=="Barbraud & Delord 2023 Figure 4a.png")
data%>%filter(filename=="Barbraud & Delord 2023 Figure 4b.png")
data%>%filter(filename=="Barbraud & Delord 2023 Figure 4c.png")