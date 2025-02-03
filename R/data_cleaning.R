#code to prepare the data for analysis
#written by A. R. Martinig
#last edited February 3, 2025 by A. R. Martinig

#Delete previous information stored 
rm(list=ls(all=T))

options(scipen=999, dplyr.width = Inf, tibble.print_min = 50, repos='http://cran.rstudio.com/') #scipen forces outputs to not be in scientific notation #dplyr.width will show all columns for head() function and tibble.print_min sets how many rows are printed and repos sets the cran mirror

#load libraries
pacman::p_load(
          here,
					dplyr,
					googlesheets4, 
					tidyverse, 
					rotl, 
					dplyr, 
					ape, 
					ggplot2, 
					ggnewscale, 
					ggtree, 
					ggtreeExtra,
					taxize,
					rentrez
)

select<-dplyr::select


# Read in dataframe
df <- read.csv(here("data", "Aim 1 - Aim 1.csv")) %>% #read.csv (title=here(”folder name”, “data.csv”)) → keeping these separate means you don’t have to rewrite your pathway or change anything when you switch between apple and PC
  filter(
    !reference=="Saatoglu et al. 2025",
  	!composite_variable=="Y",
		!n_group_1 %in% c(0, 1), 
		!n_group_2 %in% c(0, 1)) %>%
  separate(lat_lon, into = c("lat", "lon"), sep = ", ", convert = TRUE) %>%	
  separate(year, into = c("start_year", "end_year"), sep = "-", fill = "right", convert=TRUE) %>%	
	mutate_if(is.character, as.factor) %>%
	mutate(
	  start_year=as.numeric(start_year),
	  end_year = ifelse(is.na(end_year), start_year, as.numeric(end_year)),
	  study_duration=end_year-start_year,
		publication_year =str_sub(reference, start = -4),
		publication_year = case_when(reference %in% c("Germain et al. 2017a", "Germain et al. 2017b") ~"2017", TRUE~ publication_year),
		publication_year=as.numeric(publication_year),
    	publication_authors = as.factor(str_squish(str_sub(reference, end = -5))),
    	publication_authors = as.factor(case_when(reference %in% c("Germain et al. 2017a", "Germain et al. 2017b") ~"Germain et al.", TRUE~ publication_authors)),    	
    	age_class_clean=as.factor(
    		case_when(age_class %in% c("A", "YA", "Y") ~ "adult",
    			age_class %in% c("JYA", "JA", "JY") ~ "mix",
    			age_class=="J" ~ "juvenile",
    			TRUE ~ age_class)),
    			
    	fitness_metric_clean = as.factor(
			case_when(
				
		#number of offspring produced over a lifetime - including breeding success (i.e., bred at least once); grouping clutch size (number of eggs laid) and brood size (number of eggs that hatch)/number emerged	
    		fitness_metric %in% c("clutch size", "brood size", "number of juveniles at emergence", "breeding probability", "nest success (at least once hatchling alive at banding)", "attained breeding success (individual bred)", "attaining breeder position", "bred at least once", "breed at least once in lifetime", "total number of offspring across lifetime", "relative clutch size", "produced offspring", "produced chicks in season", "percent of clutch hatched", "number of offspring", "number of hatchlings", "n offspring", "number of offspring per year", "number of mature ova", "number of eggs", "lifetime reproductive success (total eggs)", "probability of reproducing", "probability of conception",  "percent males bred", "reproduced", "reproductive", "litter size", "hatching success (at least one egg hatched)", "first egg stage (laid an egg)", "emerged pups sired per month", "eggs per individual", "clutch volume (instead of size)", "brood size at hatching", "breeding probability (not defined)", "birth rate", "annual number of offspring", "percentage of females known to nest successfully", "reproductive effort (see comments)", "reproductive success", "reproductive success (she survived and hatched at least one egg)", "reproduced as adult", "reproduced as yearling", "survival to reproduction",  "gain breeding position (survivors only)", "did not lose whole clutch", "male paternity loss/extrapair young in the nest (yes/no)", "breeding (y/n)")  ~ "lifetime breeding success", 
	    			    		
		#number of offspring produced over a lifetime that survive until weaning or fledging (i.e., still dependent on parents)
    		fitness_metric %in% c("number of fledglings", "annual number of fledglings", "number of fledglings per breeding attempt", "weaning success", "reproductive success (at least one fledgling)", "lifetime n of pups reared to weaning", "n pups reared to weaning", "at least one fledgling",  "at least one young fledged", "total number of offspring fledged", "percent of clutch fledged", "number of weaned offspring", "nestlings fledged", "no young fledged", "nest success", "proportion of eggs producing fledglings", "number of fledglings among successful nests", "number of fledglings (first breeding attempt)", "n offspring successfully weaned", "lifetime reproductive success (total fledglings)", "produced offspring that survived to weaning", "fledging probablity", "daily nest survival (at least one chick fledged)", "breeding success (ratio of chicks fledged to number of eggs laid)", "breeding performance (all eggs fledged or partial clutch fledged)", "number fledged per laid egg", "nest success (fledged at least one offspring)")  ~ "lifetime reproductive success", 

		#how the offspring themselves survive - often until recruitment (when they are "added" to a population) or after reaching independence from parents (i.e., post-fledging)
    		fitness_metric %in% c("offspring survival to yearling", "offspring survival to independence", "offspring survival to age 4", "survival of offspring to yearling", "offspring survived two years", "offspring survived summer",  "offspring survival (father)", "offspring survival (mother)", "son longevity (father)", "son longevity (mother)", "number of surviving offspring per year", "number of recruits per birth", "n of juveniles alive in late summer per individual", "number of yearlings at emigration", "number of offspring recruited", "n of yearling daughters", "number of recruits per nest", "number of recruits", "number of daughters reaching sexual maturity", "at least one pup recruited", "total number of recruits", "total recruits across lifetime", "number of offspring surviving to subadult",  "number of offspring surviving", "lifetime reproductive success (total recruits)", "daughters longevity (mother)", "daughters longevity (father)", "number of offspring surviving to 30 days after leaving nest", "proportion of offspring surviving to 15 years old", "offspring survival to 7 days", "offspring survival to 30 days", "relative number of recruits", "reproductive success (yearlings raised in first year)", "offspring recruitment probability") ~ "offspring survival", 
    	
    		#how old an individual is when they first have offspring - including how old an individual is when they reach sexual maturity
    		    fitness_metric %in% c("age at first reproduction", "age at maturity", "year of first nest")  ~ "age at first reproduction",  

			#Time between first and last reproductive attempt OR number of reproductive attempts (i.e., number of breeding attempts (litters, clutches, etc.) an individual has over their lifetime) - grouped these together
    		    fitness_metric %in% c("number of years breeding", "number of breeding events", "number of breeding attempts", "lifetime breeding effort (number of breeding attempts as a proportion of an individual's lifespan)", "n of cluthes per year", "number of clutches")  ~ "reproductive lifespan and/or attempts",  
    		    
    		#time between birth and death or a set point (defined by authors)
   				fitness_metric %in% c("survival winter", "survival to yearling", "survival to maturity", "survival spring", "survival autumn", "survival annual", "survival beyond 663 days", "survival to 298 days", "survival to 298 to 663 days", "survival 1st to 2nd year", "lifespan", "age at death", "longevity", "survival to reproductive maturity (overwinter)", "survival to following year", "survival to following spring", "survival to age 3", "survival to 35 days old", "survival summer", "survival rate", "survival >1 year", "survival (shooting season)", "survival (monthly)", "monthly survival", "successfully settling", "recruitment probability", "recruitment", "lifetime survival") ~ "survival", 	
   			
   			#offspring that go on to reproduce themselves #includes offspring number of reproductive attempts (i.e., number of breeding attempts (litters, clutches, etc.) an individual has over their lifetime)
    			fitness_metric %in% c("offspring successfully bred at least once", "number of offspring that bred", "offspring lifetime reproductive success", "son lifetime reproductive success (father)", "son lifetime reproductive success (mother)", "daughter lifetime reproductive success (mother)", "daughter lifetime reproductive success (father)", "daughter number of breeding attempts (mother)", "daughter number of breeding attempts (father)", "daughter litter size (mother)", "daughter litter size (father)", "total number of offspring that survived to breed") ~ "offspring reproduction", 
    		
  		TRUE ~ fitness_metric)),
  		
  		fitness_higher_level = as.factor(
		case_when(
				fitness_metric_clean %in% c("survival", "offspring survival") ~ "survival",
				fitness_metric_clean %in% c ("age at first reproduction", "lifetime breeding success", "lifetime reproductive success", "offspring reproduction", "reproductive lifespan and/or attempts") ~ "reproduction",
				TRUE ~ fitness_metric_clean)),

  		whose_fitness=as.factor(ifelse(fitness_metric_clean %in% c("offspring survival", "offspring reproduction"), "descendant", "focal"))) %>%
  	select(-c(title, DOI, journal, composite_variable, effect_size_p_value, data_source, comments)) 
  			
#CHECK THE STR() and for spaces with variables

table(df$function_needed)	
table(df$effect_size)	
table(df$effect_size_details)	
table(df$effect_size_type)
table(df$effect_size_direction)	
table(df$effect_size_df)
table(df$fitness_metric)
table(df$fitness_metric_clean)
table(df$fitness_higher_level)
table(df$year)
table(df$publication_year)
table(df$age_class)
table(df$age_class_clean)

hist(df$publication_year, breaks=50)

tail(df)
	
####################################
#summary stats - sample sizes
####################################

nrow(df) #700 effect sizes
df %>% as_tibble() %>% count(paperID) %>% nrow() #206 studies			
df %>% as_tibble() %>% count(species_cleaned) %>% nrow() #148 species		
length(unique(df$species_cleaned)) #148

df %>% as_tibble() %>% count(species_class) %>% nrow() #6 taxonomic classes		
table(df$species_class)

table(df$fitness_higher_level)

table((df %>% group_by(paperID) %>% filter (row_number()==1))$fitness_main_focus) #206 total
table((df %>% group_by(paperID) %>% filter (row_number()==1))$confirmation_bias) #206 total

table((df %>% group_by(paperID, study_type) %>% filter (row_number()==1))$study_type) #208 total 
#because some studies include both natural and semi-natural conditions


####################################
#summary stats - observer-level
####################################

#extracted data
df %>% filter(obsID =="ARM") %>% as_tibble() %>% count(paperID) %>% nrow() #185 studies
df %>% filter(obsID =="ARM") %>% as_tibble() %>% nrow() #625 effect sizes
df %>% filter(obsID =="SLPB") %>% as_tibble() %>% count(paperID) %>% nrow() #21 studies
df %>% filter(obsID =="SLPB") %>% as_tibble() %>% nrow() #75 effect sizes

#cross checked data
df %>% filter(cross_checked=="SN") %>% as_tibble() %>% count(paperID) %>% nrow() #45 papers (need to do 20 % of total papers, so crosscheck at least 40)
df %>% filter(cross_checked=="SN") %>% as_tibble() %>% nrow() #217 effect sizes
df %>% filter(cross_checked=="ML") %>% as_tibble() %>% count(paperID) %>% nrow() #24 studies 
df %>% filter(cross_checked=="ML") %>% as_tibble() %>% nrow() #75 effect sizes
df %>% filter(cross_checked=="SLPB") %>% as_tibble() %>% count(paperID) %>% nrow() #15 studies
df %>% filter(cross_checked=="SLPB") %>% as_tibble() %>% nrow() #37 effect sizes
df %>% filter(cross_checked=="ARM") %>% as_tibble() %>% count(paperID) %>% nrow() #21 studies 
df %>% filter(cross_checked=="ARM") %>% as_tibble() %>% nrow() #75 effect sizes



####################################
#sanity checks - species sample sizes
####################################

df %>% as_tibble() %>% count(speciesID) %>% nrow() #151 species		
df %>% as_tibble() %>% count(common_name) %>% nrow() #151 species		
df %>% as_tibble() %>% count(species) %>% nrow() #151 species		
df %>% as_tibble() %>% count(species_cleaned) %>% nrow() #148 species		
length(unique(df$species_cleaned)) #148

#there were some mismatches across columns, so here is how I checked this (instead of checking manually)

aa<- df %>% select(speciesID, common_name, species, species_cleaned)

distinct(aa) |>
  filter(.by = speciesID, n() > 1) #if this has anything except 0 as an output this means there is a problem 

distinct(aa) |>
  filter(.by = common_name, n() > 1) #if this has anything except 0 as an output this means there is a problem

distinct(aa) |>
  filter(.by = species, n() > 1) #if this has anything except 0 as an output this means there is a problem

distinct(aa) |>
  filter(.by = species_cleaned, n() > 1) %>% arrange(species_cleaned) 
#in this case, the non-zero stuff here is ok. I explicitly made these changes 


####################################
#sanity checks - taxonomic tree
####################################

#to check that the taxonomic tree is the same between mytree and what is in my dataset	
#phylogeny <- gsub(" ", "_", df$species_cleaned)
#length(unique(phylogeny)) #145
#mytips<-mytree$tip.label
#length(unique(mytips)) #144

#setdiff(phylogeny, mytips)
#setdiff(mytips, phylogeny)

#setdiff(df$species_cleaned, Species_info$species_cleaned)
#setdiff(Species_info$species_cleaned, df$species_cleaned)


####################################
#sanity checks - biased species sampling
####################################

#checking to see if species are oddly distributed across moderators
study_level<-df%>%group_by(paperID, subsetID, speciesID, dispersal_type, dispersal_phase, age_class_clean, fitness_higher_level)%>%filter(row_number()==1)
table(study_level$species_class, study_level$sex)
table(study_level$species_class, study_level$dispersal_type) #most breeding dispersal studies are birds
table(study_level$species_class, study_level$dispersal_phase) #majority of data on settlement phase comes from birds
table(study_level$species_class, study_level$age_class_clean) #same idea here
table(study_level$species_class, study_level$fitness_higher_level) #majority of data on reproduction phase comes from birds
table(study_level$species_class, study_level$fitness_main_focus)
table(study_level$species_class, study_level$confirmation_bias)
#birds being 50% of the data contribute to nearly all moderators proportionally more than any other taxonomic group

####################################
#sanity checks - mean and SD
####################################

df_means<-df%>%filter(function_needed=="mean_method") %>% mutate(ratio_1=variance_group_1/mean_group_1, ratio_2=variance_group_2/mean_group_2)
summary(df_means)

#group 1
hist(df_means$ratio_1) #values above 4 or below zero should be investigated

above4_1<-df_means%>%filter(ratio_1>3.9) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
above4_1
#studies flagged:
#Engh et al. 2002 -> had to convert SE to SD
#Cotto et al. 2015 -> They measured fitness as reproductive effort (realized value relative to what would be expected given body size).
#Green & Hatchwell 2018 -> authors calculated this and sent us the mean and SD 
#Saatoglu et al. 2025 -> authors calculated this and sent us the mean and SD 
#we checked all of these and concluded they are ok

below0_1<-df_means%>%filter(ratio_1<0) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_1
#studies flagged:
#Cotto et al. 2015
#we concluded this is OK. They measured fitness as reproductive effort (realized value relative to what would be expected given body size). It allows for negative values.


#group 2
hist(df_means$ratio_2) #values above 4 or below zero should be investigated
summary(df_means)

above4_2<-df_means%>%filter(ratio_2>3.9) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
above4_2
#studies flagged:
#Pärn et al. 2009 -> had to convert SE to SD
#Qu et al. 2018 -> extracted from figure using an estimated n
#Green & Hatchwell 2018 -> authors calculated this and sent us the mean and SD 
#Spence-Jones et al. 2021 -> data extracted from figure
#we checked all of these and concluded they are ok

below0_2<-df_means%>%filter(ratio_2<0) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_2
#studies flagged:
#Cotto et al. 2015
#it is OK. They measured fitness as reproductive effort (realized value relative to what would be expected given body size). It allows for negative values.


####################################
#sanity checks - percentages
####################################

summary((df%>%filter(function_needed=="percent_method1"))$mean_group_1)
summary((df%>%filter(function_needed=="percent_method2"))$mean_group_1)

summary((df%>%filter(function_needed=="percent_method1"))$mean_group_2)
summary((df%>%filter(function_needed=="percent_method2"))$mean_group_2)

#checking values under "1%"
summary((df%>%filter(function_needed=="percent_method1", mean_group_1<=1))$mean_group_1)
summary((df%>%filter(function_needed=="percent_method2", mean_group_1<=1))$mean_group_1)

summary((df%>%filter(function_needed=="percent_method1", mean_group_2<=1))$mean_group_2)
summary((df%>%filter(function_needed=="percent_method2", mean_group_2<=1))$mean_group_2)

#the percentages look good

####################################
#sanity checks - percentages and SD
####################################

#check this in the same way you do for mean and SD (use the coefficient of variation)
df_percent<-df%>%filter(function_needed=="percent_method2") %>% mutate(ratio_1=variance_group_1/mean_group_1, ratio_2=variance_group_2/mean_group_2)
summary(df_percent)

#group 1
hist(df_percent$ratio_1) #values above 4 or below ~0.05 should be investigated

below0_1_percent<-df_percent%>%filter(ratio_1<0.05) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_1_percent

#group 2
hist(df_percent$ratio_2) #values above 4 or below ~0.05 should be investigated

below0_2_percent<-df_percent%>%filter(ratio_2<0.05) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_2_percent

####################################
#sanity checks - proportions
####################################

summary((df%>%filter(function_needed=="proportion_method1"))$mean_group_1)
summary((df%>%filter(function_needed=="proportion_method2"))$mean_group_1)

summary((df%>%filter(function_needed=="proportion_method1"))$mean_group_2)
summary((df%>%filter(function_needed=="proportion_method2"))$mean_group_2)

#the proportions look good

####################################
#sanity checks - proportions and SD
####################################

#check this in the same way you do for mean and SD (use the coefficient of variation)

df_proportion<-df%>%filter(function_needed=="proportion_method2") %>% mutate(ratio_1=variance_group_1/mean_group_1, ratio_2=variance_group_2/mean_group_2)
summary(df_proportion)

#group 1
hist(df_proportion$ratio_1) #values above 4 or below ~0.05 should be investigated

above4_1_prop<-df_proportion%>%filter(ratio_1>3.9) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
above4_1_prop

below0_1_prop<-df_proportion%>%filter(ratio_1<0.05) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_1_prop
#studies flagged:
#Devillard & Bray 2009 -> extracted values from figure
#we checked and concluded they are ok

#group 2
hist(df_proportion$ratio_2) #values above 4 or below zero should be investigated

above4_2_prop<-df_proportion%>%filter(ratio_2>3.9) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
above4_2_prop

below0_2_prop<-df_proportion%>%filter(ratio_2<0.05) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_2_prop
#studies flagged:
#Devillard & Bray 2009 -> extracted values from figure
#we checked and concluded they are ok


####################################
#final data export
####################################

write.csv(df, here("data", "clean_data.csv"))
