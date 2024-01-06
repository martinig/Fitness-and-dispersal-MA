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

dat_short <- dat %>% filter(yi < 0.5, yi > - 0.5) 

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


# creating VCV
VCV <- vcalc(vi = dat$vi, cluster = dat$shared_group, rho = 0.5)

VCV <- nearPD(VCV)$mat

#shared_group

# meta-analysis - basic model
###########
# do not run.....
##########
mod <- rma.mv(yi = yi, 
              V = VCV,
              mod = ~ 1,
              data = dat,
              random = list(
                            ~ 1 | effectID,
                            ~ 1 | paperID,
                            ~ 1 | species_cleaned,
                           ~ 1 | phylogeny),
              R = list(phylogeny = cor_tree),
              test = "t",
              sparse = TRUE)

# saving the runs
saveRDS(mod, here("Rdata", "mod.RDS"))

# getting saved Rdata
mod <- readRDS(here("Rdata", "mod.RDS"))

summary(mod)

round(i2_ml(mod),2)   


# phylogeney accounts nothing (0) so we can take it out
mod1 <- rma.mv(yi = yi, 
               V = VCV,
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

#####
# VCV_s <- vcalc(vi = dat_short$vi, 
#                cluster = dat_short$shared_group, rho = 0.5)
# 
# VCV_s <- nearPD(VCV_s)$mat
# 
# 
# mod1s <- rma.mv(yi = yi, 
#                V = VCV_s,
#                mod = ~ 1,
#                data = dat_short, 
#                random = list(
#                  ~ 1 | effectID,
#                  ~ 1 | paperID,
#                  ~ 1 | species_cleaned),
#                # ~ 1 | phylogeny),
#                #R= list(phylogeny = cor_tree),
#                test = "t",
#                sparse = TRUE)
# 
# summary(mod1s)
# 
# round(i2_ml(mod1s),2)   
# 
# orchard_plot(mod1s, xlab = "Effect Size: Zr", group = "paperID")


#funnel(mod1, yaxis = "seinv")
#funnel(mod1)

# TOOD
# Moderators:
#   sex
# study_type
# dispersal_type
# dispersal_phase
# age_class
# generation - whose_fitness
# fitness_metric_clean
# other things.... 

# sex
mod2 <- rma.mv(yi = yi, 
               V = VCV,
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

mod3 <- rma.mv(yi = yi,  
               V = VCV,
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
mod4 <- rma.mv(yi = yi,
               V = VCV,
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
mod5 <- rma.mv(yi = yi,
               V = VCV,
               mod = ~ whose_fitness - 1,
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


mod5c <- rma.mv(yi = yi, 
                V = VCV,
               mod = ~ whose_fitness - 1,
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

orchard_plot(mod5, mod = "whose_fitness", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4)


# fitness_metric_clean

mod6 <- rma.mv(yi = yi, 
               V = VCV,
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

mod7 <- rma.mv(yi = yi, 
               V = VCV,
               mod = ~ fitness_higher_level - 1,
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

orchard_plot(mod7, mod = "fitness_higher_level", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 45)


# dispersal_type
mod8 <- rma.mv(yi = yi, 
               V = VCV,
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


# dispersal_phase

mod10 <- rma.mv(yi = yi, 
                V = VCV,
               mod = ~ species_class - 1,
               data = dat, 
               random = list(
                 ~ 1 | effectID,
                 ~ 1 | paperID,
                 ~ 1 | species_cleaned),
               # ~ 1 | phylogeny),
               # R= list(phylogeny = cor_tree),
               test = "t",
               sparse = TRUE)
summary(mod10)
r2_ml(mod10)

orchard_plot(mod10, mod = "species_class", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 90)


# 


##########
# functions for absolute values


# folded mean
folded_mu <-function(mean, variance){
  mu <- mean
  sigma <- sqrt(variance)
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_mu
} 

# folded variance
folded_v <-function(mean, variance){
  mu <- mean
  sigma <- sqrt(variance)
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
  # adding se to make bigger mean
  fold_v <-fold_se^2
  fold_v
} 


# absolute values
dat <- dat %>% mutate(
  abs_yi = abs(yi), # we use this one (conservative)
  abs_yi2 = folded_mu(yi, vi), # alternative way
  abs_vi = folded_v(yi, vi))


##
# creating VCV
VCV_abs <- vcalc(vi = dat$abs_vi, cluster = dat$shared_group, rho = 0.5)
VCV_abs <- nearPD(VCV_abs)$mat

# fitness_main_focus

# need to look at absoulte values

mod11 <- rma.mv(yi = abs_yi2, 
                V = VCV_abs,
                mod = ~ fitness_main_focus - 1,
                data = dat, 
                random = list(
                  ~ 1 | effectID,
                  ~ 1 | paperID,
                  ~ 1 | species_cleaned),
                # ~ 1 | phylogeny),
                # R= list(phylogeny = cor_tree),
                test = "t",
                sparse = TRUE)
summary(mod11)
r2_ml(mod11)

mod11a <- rma.mv(yi = abs_yi2, 
                V = VCV_abs,
                mod = ~ fitness_main_focus,
                data = dat, 
                random = list(
                  ~ 1 | effectID,
                  ~ 1 | paperID,
                  ~ 1 | species_cleaned),
                # ~ 1 | phylogeny),
                # R= list(phylogeny = cor_tree),
                test = "t",
                sparse = TRUE,
                method = "ML",
                control = list(optimizer = "Nelder-Mead"))
                
summary(mod11a)


mod11b <- rma.mv(yi = abs_yi2, 
                 V = VCV_abs,
                 mod = ~ fitness_main_focus,
                 data = dat, 
                 random = list(
                   ~ fitness_main_focus | effectID,
                   ~ 1 | paperID,
                   ~ 1 | species_cleaned),
                 struct = "DIAG",
                 # ~ 1 | phylogeny),
                 # R= list(phylogeny = cor_tree),
                 method = "ML",
                 test = "t",
                 sparse = TRUE,
                 control = list(optimizer = "Nelder-Mead"))
summary(mod11b)

anova(mod11a, mod11b)


orchard_plot(mod11b, mod = "fitness_main_focus", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 90)

#

mod12 <- rma.mv(yi = yi, 
                V = VCV,
                mod = ~ fitness_main_focus - 1,
                data = dat, 
                random = list(
                  ~ 1 | effectID,
                  ~ 1 | paperID,
                  ~ 1 | species_cleaned),
                # ~ 1 | phylogeny),
                # R= list(phylogeny = cor_tree),
                test = "t",
                sparse = TRUE)
summary(mod12)
r2_ml(mod12)
orchard_plot(mod12, mod = "fitness_main_focus", xlab = "Effect Size: Zr", group = "paperID", branch.size = 4, angle = 90)


##############
# Mulit-moderator models 
##############
# TODO - should we do this at all

####################
# publication bias
####################

funnel(mod1, 
       yaxis="seinv",
       # = "rstudent",
       xlab = "Standarized residuals",
       ylab = "Precision (inverse of SE)",
       ylim = c(0.001, 16),
       xlim = c(-10,15),
       digits=c(0,1)
)

# funnel(mod1, 
#        #yaxis="seinv",
#        # = "rstudent",
#        xlab = "Standarized residuals",
#        ylab = "Precision (inverse of SE)",
#        ylim = c(0.001, 16),
#        xlim = c(-10,15),
#        digits=c(0,1)
# )


## Egger regression: uni-moderator

# write conditional statement here

# if each group n is avaiable - assume n/2
dat$effectN <- ifelse(is.na(dat$n_group_1), (dat$n/2)*2/dat$n,  
                      (dat$n_group_1 * dat$n_group_2) / (dat$n_group_1 + dat$n_group_2))
  
dat$sqeffectN <- sqrt(dat$effectN)

mod_egger <- rma.mv(yi = yi, 
                    V = VCV,
                mod = ~ sqeffectN,
                data = dat, 
                random = list(
                  ~ 1 | effectID,
                  ~ 1 | paperID,
                  ~ 1 | species_cleaned),
                # ~ 1 | phylogeny),
                # R= list(phylogeny = cor_tree),
                test = "t",
                sparse = TRUE)
summary(mod_egger)

round(r2_ml(mod_egger)*100, 2)

small <- bubble_plot(mod_egger,
                     mod = "sqeffectN",
                     group = "paperID",
                     xlab = "sqrt(Effective N)",
                     ylab = "Effect Size: Zr",
                     g = TRUE)

small

## Decline effect: uni-moderator

# creating publication year from "reference"
dat$year <- with(dat, substr(reference, nchar(reference)-4, nchar(reference)))
dat$year <- as.integer(ifelse(dat$year == "2017a" | dat$year == "2017b", 2017, dat$year))
# decline effect
mod_dec <- rma.mv(yi = yi, V = vi,
                     mod = ~ year,
                     data = dat, 
                     random = list(
                       ~ 1 | effectID,
                       ~ 1 | paperID,
                       ~ 1 | species_cleaned),
                     # ~ 1 | phylogeny),
                     # R= list(phylogeny = cor_tree),
                     test = "t",
                     sparse = TRUE)
summary(mod_dec)

round(r2_ml(mod_dec)*100, 2)

decline <- bubble_plot(mod_dec,
                       mod = "year",
                       group = "paperID",
                       xlab = "Publication year",
                       ylab = "Effect Size: Zr",
                       g = TRUE)
decline

## All together

mod_comb <- rma.mv(yi = yi, 
                   V = VCV,
                   mod = ~ year + sqeffectN,
                   data = dat, 
                   random = list(
                     ~ 1 | effectID,
                     ~ 1 | paperID,
                     ~ 1 | species_cleaned),
                   # ~ 1 | phylogeny),
                   # R= list(phylogeny = cor_tree),
                   test = "t",
                   sparse = TRUE)
                   
summary(mod_comb)

round(r2_ml(mod_comb)*100, 2)

# small-study
bubble_plot(mod_egger,
            mod = "sqeffectN",
            group = "paperID",
            xlab = "sqrt(Effective N)",
            ylab = "Effect Size: Zr",
            g = TRUE)

# decline
bubble_plot(mod_comb,
            mod = "year",
            group = "paperID",
            xlab = "Publication year",
            ylab = "Effect Size: Zr",
            g = TRUE)



## Leave-1study-out (sensitivity analysis)

dat <- dat %>%
  mutate(leave_out = reference)

dat$leave_out <- as.factor(dat$leave_out)


LeaveOneOut_effectsize <- list()
for (i in 1:length(levels(dat$leave_out))) {
  temp_dat <- dat %>%
    filter(leave_out != levels(dat$leave_out)[i])
  
  VCV_leaveout <- vcalc(vi = temp_dat$vi, cluster = temp_dat$shared_group, rho = 0.5)
  
  LeaveOneOut_effectsize[[i]] <-  rma.mv(yi = yi,
                                         V = VCV_leaveout, 
                                         random = list(
                                           ~ 1 | effectID,
                                           ~ 1 | paperID,
                                           ~ 1 | species_cleaned),
                                         # ~ 1 | phylogeny),
                                         # R= list(phylogeny = cor_tree),
                                         test = "t",
                                         sparse = TRUE,
                                         data = temp_dat[temp_dat$leave_out != levels(temp_dat$leave_out)[i], ])
}

# writing function for extracting est, ci.lb, and ci.ub from all models
est.func <- function(model) {
  df <- data.frame(est = model$b, lower = model$ci.lb, upper = model$ci.ub)
  return(df)
}

# using dplyr to form data frame
MA_oneout <- lapply(LeaveOneOut_effectsize,function(x) est.func(x)) %>%
  bind_rows %>%
  mutate(left_out = levels(dat$leave_out))


# telling ggplot to stop reordering factors
MA_oneout$left_out <- as.factor(MA_oneout$left_out)
MA_oneout$left_out <- factor(MA_oneout$left_out, levels = MA_oneout$left_out)

# saving the runs
saveRDS(MA_oneout, here("Rdata", "MA_oneout.RDS"))



MA_oneout <- readRDS(here("Rdata", "MA_oneout.RDS"))

# plotting
leaveoneout <- ggplot(MA_oneout) + geom_hline(yintercept = 0, lty = 2, lwd = 1) +
  geom_hline(yintercept = mod_ma2$ci.lb, lty = 3, lwd = 0.75, colour = "black") +
  geom_hline(yintercept = mod_ma2$b, lty = 1, lwd = 0.75, colour = "black") + 
  geom_hline(yintercept = mod_ma2$ci.ub,
             lty = 3, lwd = 0.75, colour = "black") + 
  geom_pointrange(aes(x = left_out, y = est,
                      ymin = lower, ymax = upper)) + 
  xlab("Study left out") + 
  ylab("Zr (effect size), 95% CI") +
  coord_flip() + 
  theme(panel.grid.minor = element_blank()) + theme_bw() + theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) + theme(axis.text.y = element_text(size = 6))

leaveoneout



