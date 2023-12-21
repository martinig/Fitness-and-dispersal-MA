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

dat <- read.csv(here("data", "clean_data.csv"))

dim(dat)
head(dat)

tree <- read.tree(here("data", "species_tree.tre"))

# adding branche length
tree <- compute.brlen(tree)

# turning into correlation matrix
cor_tree <- vcv(tree, corr=T)

# check tip labels match with data


# fixing data a bit

#dat$direction <- ifelse(grepl("negative", dat$effect_size_direction), -1, 1)

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
                         dat$effect_size_p_value_numeric, dat$direction_change, dat$function_needed), 
                    effect_size)                    

# dat$Zr <- unlist(effect1[1,])
# dat$VZr <- unlist(effect1[2,])

# merging two data frames
dat <- cbind(dat, effect2)


# renaming X to effectID
colnames(dat)[colnames(dat) == "X"] <- "effectID"

# creating the phylogeny column

dat$phylogeny <-  gsub(" ", "_", dat$species_cleaned)

match(unique(dat$phylogeny), tree$tip.label)
match(tree$tip.label, unique(dat$phylogeny))

intersect(unique(dat$phylogeny), tree$tip.label)
setdiff(unique(dat$phylogeny), tree$tip.label)

###
match(unique(dat$phylogeny), tree$tip.label)
sum(is.na(match(unique(dat$phylogeny), tree$tip.label)))

# looking at data
dat$yi

# which is NA and NaN
#which(is.na(dat$yi)) 


# visualsing 
hist(dat$yi)
hist(log(dat$vi))

# meta-analysis - basic model

mod <- rma.mv(yi = yi, V = vi,
              mod = ~ 1,
              data = dat, 
              random = list(
                            ~ 1 | effectID,
                            ~ 1 | paperID,
                            ~ 1 | species_cleaned,
                           ~ 1 | phylogeny),
              R= list(phylogeny = cor_tree),
              test = "t",
              sparse = TRUE)

summary(mod)

round(i2_ml(mod),2)   

# phylogeney accounts nothing (0) so we can take it out
mod1 <- rma.mv(yi = yi, V = vi,
              mod = ~ 1,
              data = dat, 
              random = list(
                ~ 1 | effectID,
                ~ 1 | paperID,
                ~ 1 | species_cleaned),
               # ~ 1 | phylogeny),
              #R= list(phylogeny = cor_tree),
              test = "t",
              sparse = TRUE)

summary(mod1)

round(i2_ml(mod1),2)   

orchard_plot(mod1, xlab = "Effect Size: Zr", group = "paperID")
funnel(mod1, yaxis = "seinv")
funnel(mod1)

# TOOD
# Moderators:
#   sex
# study_type
# dispersal_type
# dispersal_phase
# age_class
# generation
# fitness_metric_clean

# sex
mod2 <- rma.mv(yi = yi, V = vi,
              mod = ~ sex - 1,
              data = dat, 
              random = list(
                ~ 1 | effectID,
                ~ 1 | paperID,
                ~ 1 | species_cleaned),
              #              ~ 1 | phylogeny),
              #R= list(phylogeny = cor_tree),
              test = "t",
              sparse = TRUE)
summary(mod2)

r2_ml(mod2)

orchard_plot(mod2, mod = "sex", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4)

# study_type

mod3 <- rma.mv(yi = yi, V = vi,
               mod = ~ study_type - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               #              ~ 1 | phylogeny),
               #R= list(phylogeny = cor_tree), # phylogenetic tree
               test = "t",
               sparse = TRUE)
summary(mod3)

r2_ml(mod3)

orchard_plot(mod3, mod = "study_type", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4)


# ageclass
mod4 <- rma.mv(yi = yi, V = vi,
               mod = ~ age_class - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               #              ~ 1 | phylogeny),
               #R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod4)
r2_ml(mod4)


orchard_plot(mod4, mod = "age_class", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4)

# generation
mod5 <- rma.mv(yi = yi, V = vi,
               mod = ~ generation - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               #              ~ 1 | phylogeny),
               #R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod5)
r2_ml(mod5)


mod5c <- rma.mv(yi = yi, V = vi,
               mod = ~ generation - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               #              ~ 1 | phylogeny),
               #R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod5c)

orchard_plot(mod5, mod = "generation", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4)


# fitness_metric_clean

mod6 <- rma.mv(yi = yi, V = vi,
               mod = ~ fitness_metric_clean - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
                # ~ 1 | phylogeny),
              # R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod6)
r2_ml(mod6)

orchard_plot(mod6, mod = "fitness_metric_clean", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 45)


# fitness_metric_clean

mod7 <- rma.mv(yi = yi, V = vi,
               mod = ~ higher_level_fitness - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               # ~ 1 | phylogeny),
               # R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod7)

orchard_plot(mod7, mod = "higher_level_fitness", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 45)


# dispersal_type
mod8 <- rma.mv(yi = yi, V = vi,
               mod = ~ dispersal_type - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               # ~ 1 | phylogeny),
               # R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod8)
r2_ml(mod8)

orchard_plot(mod8, mod = "dispersal_type", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 45)

# dispersal_phase

mod9 <- rma.mv(yi = yi, V = vi,
               mod = ~ dispersal_phase - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               # ~ 1 | phylogeny),
               # R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod9)
r2_ml(mod9)

orchard_plot(mod9, mod = "dispersal_phase", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 45)

# it would be interesting to see what are these studies with Zr > 0.95 and < -0.95
