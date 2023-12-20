pacman::p_load(tidyverse, # tidy family and related pacakges below
               kableExtra, 
               gridExtra, # may not use this
               pander,   # nice tables
               metafor,  # package for meta-analysis
               ape,      # phylogenetic analysis
               MuMIn,  # multi-model inference
               patchwork,   # putting ggplots together - you need to install via devtool
               here,         # making reading files easy
               orchaRd # plotting orchard plots

)

## reading in a function

source(here("R", "effect_size.R"),chdir = TRUE)

## test

dat <- read.csv(here("data", "clean_data.csv"), )

dim(dat)
head(dat)

tree <- read.tree(here("data", "species_tree.tre"))

cor_tree <- vcv(tree, corr=T)

# check tip labels match with data


# fixing data a bit

dat$direction <- ifelse(grepl("negative", dat$effect_size_direction), -1, 1)

# use mapply to apply the above fundion effect_size to the data frame
# effect1 <- mapply(effect_size, dat$mean_group_1, dat$mean_group_2, 
#                       dat$variance_group_1, dat$variance_group_2, 
#                       dat$n_group_1, dat$n_group_2, dat$n, 
#                        dat$effect_size_value, dat$effect_size_variance, 
#                        dat$effect_size_p_value_numeric, dat$direction, dat$function_needed)

effect2 <- pmap_dfr(list(dat$mean_group_1, dat$mean_group_2, 
                         dat$variance_group_1, dat$variance_group_2, 
                         dat$n_group_1, dat$n_group_2, dat$n, 
                         dat$effect_size_value, dat$effect_size_variance, 
                         dat$effect_size_p_value_numeric, dat$direction, dat$function_needed), 
                    effect_size)                    

# dat$Zr <- unlist(effect1[1,])
# dat$VZr <- unlist(effect1[2,])

# merging two data frames
dat <- cbind(dat, effect2)

# renaming X to effectID
colnames(dat)[colnames(dat) == "X"] <- "effectID"

# creating the phylogeny column

dat$phylogeny <-  gsub(" ", "_", dat$species)

match(unique(dat$phylogeny), tree$tip.label)
match(tree$tip.label, unique(dat$phylogeny))

intersect(unique(dat$phylogeny), tree$tip.label)
setdiff(unique(dat$phylogeny), tree$tip.label)

# looking at data
# which is NA and NaN
which(is.na(dat$yi)) 


# visualsing 
hist(dat$yi)
hist(log(dat$vi))

# meta-analysis - basic model

mod <- rma.mv(yi = yi, V = vi, 
              data = dat, 
              random = list(~ 1 | effectID,
                            ~ 1 | paperID),
              test = "t")

i2_ml(mod)                              

summary(mod)

orchard_plot(mod, xlab = "Effect Size: Zr", group = "paperID")
funnel(mod, yaxis = "seinv")



