#code to prepare the data for analysis
#written by A. R. Martinig
#last edited March 8, 2024 by A. R. Martinig

#Delete previous information stored 
rm(list=ls(all=T))

##set wd to the folder with all your csv's in it
#setwd("~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1")

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
		!composite_variable=="Y", 	#for now I marked Green & Hatchwell 2018 direct and indirect metrics as composite_variable=="Y" so they are excluded and I will only keep the inclusive fitness metric - if the authors reply with the direct fitness metrics, then I can break it down more and switch what gets excluded (i.e., keep the indirect and the more specific direct measures and exclude the inclusive fitness stuff)
		!obsID=="TBD", 
		!n_group_1 %in% c(0, 1), 
		!n_group_2 %in% c(0, 1)) %>%
  separate(lat_lon, into = c("lat", "lon"), sep = ", ", convert = TRUE) %>%	
	mutate_if(is.character, as.factor) %>%
	mutate(
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
    		fitness_metric %in% c("clutch size", "brood size", "number of juveniles at emergence", "breeding probability", "nest success (at least once hatchling alive at banding)", "attained breeding success (individual bred)", "attaining breeder position", "bred at least once", "breed at least once in lifetime", "total number of offspring across lifetime", "relative clutch size", "produced offspring", "produced chicks in season", "percent of clutch hatched", "number of offspring", "number of hatchlings", "n offspring", "number of offspring per year", "number of mature ova", "number of eggs", "lifetime reproductive success (total eggs)", "probability of reproducing", "probability of conception",  "percent males bred", "reproduced", "reproductive", "litter size", "hatching success (at least one egg hatched)", "first egg stage (laid an egg)", "emerged pups sired per month", "eggs per individual", "clutch volume (instead of size)", "brood size at hatching", "breeding probability (not defined)", "birth rate", "annual number of offspring", "percentage of females known to nest successfully", "reproductive effort (see comments)", "reproductive success", "reproductive success (she survived and hatched at least one egg)", "reproduced as adult", "reproduced as yearling", "survival to reproduction (successfully weaned at least one offspring)",  "gain breeding position (survivors only)", "did not lose whole clutch", "male paternity loss/extrapair young in the nest (yes/no)", "breeding (y/n)")  ~ "lifetime breeding success", 
	    			    	
	    			    		
		#number of offspring produced over a lifetime that survive until weaning or fledging (i.e., still dependent on parents)
    		fitness_metric %in% c("number of fledglings", "annual number of fledglings", "number of fledglings per breeding attempt", "weaning success", "reproductive success (at least one fledgling)", "lifetime n of pups reared to weaning", "n pups reared to weaning", "at least one fledgling",  "at least one young fledged", "total number of offspring fledged", "percent of clutch fledged", "number of weaned offspring", "nestlings fledged", "no young fledged", "nest success", "proportion of eggs producing fledglings", "number of fledglings among successful nests", "number of fledglings (first breeding attempt)", "n offspring successfully weaned", "lifetime reproductive success (total fledglings)", "produced offspring that survived to weaning", "fledging probablity", "daily nest survival (at least one chick fledged)", "breeding success (ratio of chicks fledged to number of eggs laid)", "breeding performance (all eggs fledged or partial clutch fledged)", "number fledged per laid egg", "nest success (fledged at least one offspring)")  ~ "lifetime reproductive success", 

    		
		#how the offspring themselves survive - often until recruitment (when they are "added" to a population) or after reaching independence from parents (i.e., post-fledging)
    		fitness_metric %in% c("offspring survival to yearling", "offspring survival to independence", "offspring survival to age 4", "survival of offspring to yearling", "offspring survived two years", "offspring survived summer",  "offspring survival (father)", "offspring survival (mother)", "son longevity (father)", "son longevity (mother)", "number of surviving offspring per year", "number of recruits per birth", "n of juveniles alive in late summer per individual", "number of yearlings at emigration", "number of offspring recruited", "n of yearling daughters", "number of recruits per nest", "number of recruits", "number of daughters reaching sexual maturity", "at least one pup recruited", "total number of recruits", "total recruits across lifetime", "number of offspring surviving to subadult",  "number of offspring surviving", "lifetime reproductive success (total recruits)", "daughters longevity (mother)", "daughters longevity (father)", "number of offspring surviving to 30 days after leaving nest", "proportion of offspring surviving to 15 years old", "offspring survival to 7 days", "offspring survival to 30 days", "relative number of recruits", "reproductive success (yearlings raised in first year)", "offspring recruitment probability") ~ "offspring survival", 
    	
    		#how old an individual is when they first have offspring - including how old an individual is when they reach sexual maturity
    		    fitness_metric %in% c("age at first reproduction", "age at maturity")  ~ "age at first reproduction",  

			#Time between first and last reproductive attempt OR number of reproductive attempts (i.e., number of breeding attempts (litters, clutches, etc.) an individual has over their lifetime) - grouped these together
    		    fitness_metric %in% c("number of years breeding", "number of breeding events", "number of breeding attempts", "lifetime breeding effort (number of breeding attempts as a proportion of an individual's lifespan)", "n of cluthes per year", "number of clutches")  ~ "reproductive lifespan and/or attempts",  
    		    
    		#time between birth and death or a set point (defined by authors)
   				fitness_metric %in% c("survival winter", "survival to yearling", "survival to maturity", "survival spring", "survival autumn", "survival annual", "survival 1st to 2nd year", "lifespan", "age at death", "longevity", "survival to reproductive maturity (overwinter)", "survival to following year", "survival to following spring", "survival to age 3", "survival to 35 days old", "survival summer", "survival rate", "survival >1 year", "survival (shooting season)", "survival (monthly)", "monthly survival", "successfully settling", "recruitment probability", "recruitment", "lifetime survival") ~ "survival", 	

   			
   			#offspring that go on to reproduce themselves #includes offspring number of reproductive attempts (i.e., number of breeding attempts (litters, clutches, etc.) an individual has over their lifetime)
    			fitness_metric %in% c("offspring successfully bred at least once", "offspring lifetime reproductive success", "son lifetime reproductive success (father)", "son lifetime reproductive success (mother)", "daughter lifetime reproductive success (mother)", "daughter lifetime reproductive success (father)", "daughter number of breeding attempts (mother)", "daughter number of breeding attempts (father)", "daughter litter size (mother)", "daughter litter size (father)", "total number of offspring that survived to breed") ~ "offspring reproduction", 
    		
  		TRUE ~ fitness_metric)),
  		
  		fitness_higher_level = as.factor(
		case_when(
				fitness_metric_clean %in% c("survival", "offspring survival") ~ "survival",
				fitness_metric_clean %in% c ("age at first reproduction", "lifetime breeding success", "lifetime reproductive success", "offspring reproduction", "reproductive lifespan and/or attempts") ~ "reproduction",
				TRUE ~ fitness_metric_clean)),

  		whose_fitness=as.factor(ifelse(fitness_metric_clean %in% c("offspring survival", "offspring reproduction"), "offspring", "individual"))) %>%
  	select(-c(title, DOI, journal, year, composite_variable, effect_size_p_value, data_source, comments))
  		  			 
summary(df)
  			
table(df$function_needed)	
table(df$effect_size)	
table(df$effect_size_details)	
table(df$effect_size_type)
table(df$effect_size_direction)	
table(df$effect_size_df)
table(df$fitness_metric)
table(df$fitness_metric_clean)
table(df$fitness_higher_level)

hist(df$publication_year, breaks=50)

head(df)
	
nrow(df) #666 effect sizes
df %>% as_tibble() %>% count(paperID) %>% nrow() #199 studies	
#df %>% as_tibble() %>% count(speciesID) %>% nrow() #148 species		
#df %>% as_tibble() %>% count(common_name) %>% nrow() #150 species		
#df %>% as_tibble() %>% count(species) %>% nrow() #147 species		
df %>% as_tibble() %>% count(species_cleaned) %>% nrow() #144 species		
length(unique(df$species_cleaned)) #144

#extracted data
df %>% filter(obsID =="ARM") %>% as_tibble() %>% count(paperID) %>% nrow() #178 studies
df %>% filter(obsID =="ARM") %>% as_tibble() %>% nrow() #591 effect sizes
df %>% filter(obsID =="SLPB") %>% as_tibble() %>% count(paperID) %>% nrow() #21 studies
df %>% filter(obsID =="SLPB") %>% as_tibble() %>% nrow() #75 effect sizes

#cross checked data
df %>% filter(cross_checked=="SN") %>% as_tibble() %>% count(paperID) %>% nrow() #199 studies #40 papers (need to do 20 % of toal papers, so crosscheck at least 40)
df %>% filter(cross_checked=="SN") %>% as_tibble() %>% nrow() #182 effect sizes
df %>% filter(cross_checked=="ML") %>% as_tibble() %>% count(paperID) %>% nrow() #23 studies 
df %>% filter(cross_checked=="ML") %>% as_tibble() %>% nrow() #75 effect sizes
df %>% filter(cross_checked=="SLPB") %>% as_tibble() %>% count(paperID) %>% nrow() #11 studies
df %>% filter(cross_checked=="SLPB") %>% as_tibble() %>% nrow() #21 effect sizes
df %>% filter(cross_checked=="ARM") %>% as_tibble() %>% count(paperID) %>% nrow() #20 studies 
df %>% filter(cross_checked=="ARM") %>% as_tibble() %>% nrow() #74 effect sizes


table(df$species_class)


#sanity checks

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


#means and error
df_means<-df%>%filter(function_needed=="mean_method") %>% mutate(ratio_1=variance_group_1/mean_group_1, ratio_2=variance_group_2/mean_group_2)
summary(df_means)

#group 1
hist(df_means$ratio_1) #values above 4 or below zero should be investigated

above4_1<-df_means%>%filter(ratio_1>3.9) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
above4_1
#studies flagged:
#Krohne & Burgin 1987 (line 41) -> had to convert SE to SD using an estimated n
#Engh et al. 2002 (line 242) -> had to convert SE to SD
#Pärn et al. 2009 (line 302) -> had to convert SE to SD
#Cotto et al. 2015 (line 383) -> They measured fitness as reproductive effort (realized value relative to what would be expected given body size).
#Green & Hatchwell 2018 (line 465) -> authors calculated this and sent us the mean and SD 

below0_1<-df_means%>%filter(ratio_1<0) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_1
#studies flagged:
#Cotto et al. 2015 (line 384) -> it is OK. They measured fitness as reproductive effort (realized value relative to what would be expected given body size). It allows for negative values.


#group 2
hist(df_means$ratio_2) #values above 4 or below zero should be investigated
summary(df_means)

above4_2<-df_means%>%filter(ratio_2>3.9) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
above4_2
#studies flagged:
#Krohne & Burgin 1987 (line 41) -> had to convert SE to SD using an estimated n
#Pärn et al. 2009 (line 301) -> had to convert SE to SD
#Qu et al. 2018 (line 458-459) -> extracted from figure using an estimated n
#Green & Hatchwell 2018 (line 465) -> authors calculated this and sent us the mean and SD 
#Spence-Jones et al. 2021 (line 662-663) -> data extracted from figure

below0_2<-df_means%>%filter(ratio_2<0) %>% select(reference, paperID, subsetID, speciesID, common_name, fitness_metric, age_class, sex, n, group_1, group_2, n_group_1, n_group_2, mean_group_1, mean_group_2, mean_units, type_of_variance, variance_group_1, variance_group_2, ratio_1, ratio_2)
below0_2
#studies flagged:
#Cotto et al. 2015 (line 383 & 385) -> it is OK. They measured fitness as reproductive effort (realized value relative to what would be expected given body size). It allows for negative values.


#percentages
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


#proportions
summary((df%>%filter(function_needed=="proportion_method1"))$mean_group_1)
summary((df%>%filter(function_needed=="proportion_method2"))$mean_group_1)

summary((df%>%filter(function_needed=="proportion_method1"))$mean_group_2)
summary((df%>%filter(function_needed=="proportion_method2"))$mean_group_2)

#the proportions look good



write.csv(df, here("data", "clean_data.csv"))
		

#to check that the taxonomic tree is the same between mytree and what is in my dataset	
#phylogeny <- gsub(" ", "_", df$species_cleaned)
#length(unique(phylogeny)) #145
#mytips<-mytree$tip.label
#length(unique(mytips)) #144

#setdiff(phylogeny, mytips)
#setdiff(mytips, phylogeny)


setdiff(df$species_cleaned, Species_info$species_cleaned)
setdiff(Species_info$species_cleaned, df$species_cleaned)
