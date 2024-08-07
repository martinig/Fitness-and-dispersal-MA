#code to creat a taxonomic tree
#written by M. Lagisz and A. R. Martinig
#last edited August 7, 2024 by A. R. Martinig


# Specify the URL of your Google Sheet
url <- "https://docs.google.com/spreadsheets/d/1ZWQUCG5wuLIPJbhYfRW_rvFVb4qFiWtFm2e0BZ5WBkE/edit#gid=1046848762"

# Read the Google Sheet into a dataframe
Species_info <- read_sheet(url) %>%
	filter(
		!composite_variable=="Y", 
		!obsID=="TBD", 
		!n_group_1 %in% c(0, 1), 
		!n_group_2 %in% c(0, 1)) %>%
  select(c("reference", "paperID", "common_name", "species_cleaned", "dispersal_type")) 


length(unique(Species_info$species_cleaned)) #146
Species_info %>% as_tibble() %>% count(species_cleaned) %>% nrow() #146 species		
unique(Species_info$species_cleaned) #needs cleaning 
table(Species_info$species_cleaned)

taxa <- tnrs_match_names(unique(Species_info$species_cleaned)) 

length(unique(Species_info$species_cleaned)) #146

taxa <- tnrs_match_names(unique(Species_info$species_cleaned)) #runs fine
names(taxa)
#synonyms(taxa) ##list of all synonyms
taxa$unique_name # main TOL names
table(taxa$approximate_match) ##0 approximate match
taxa %>% filter(approximate_match==TRUE)
table(taxa$flags) # flags 23 names with problems (hidden, hybrid, incertae_sedis_inherited, infraspecific, infraspecific) - will need fixing

mytree <- tol_induced_subtree(ott_ids = taxa$ott_id, label_format= "name") 
#this will fail

plot(mytree, show.tip.label = T, cex = 0.8, no.margin = TRUE)

is.binary(mytree) #if it returns FALSE that means you have several branches coming from the same node

mytree$tip.label

## Tree tip labels need some cleaning:
mytree$tip.label <- gsub("\\(.*", "", mytree$tip.label) #remove comments
mytree$tip.label <- gsub("_"," ", mytree$tip.label) #get rid of the underscores
mytree$tip.label <- trimws(mytree$tip.label) #getting rid of the trailing whitespace

mytree$tip.label

#this lets me check which species names don't match up
#you want to get 100% matching records between the two (your original labels and the labels in the tree)
sort(intersect(as.character(mytree$tip.label), unique(Species_info$species_cleaned))) ## 144 names are matching -  got quite a few left to fix
sort(setdiff(unique(Species_info$species_cleaned), as.character(mytree$tip.label))) ## names from species column not matching with tip label
sort(setdiff(as.character(mytree$tip.label), unique(Species_info$species_cleaned))) ## names from tip.label not matching in animal column

#check matching records again
sort(intersect(as.character(mytree$tip.label), unique(Species_info$species_cleaned))) ## 144 names are matching - all fixed 

plot(mytree, show.tip.label = T, cex = 0.8, no.margin = TRUE)
str(mytree) #290 tips


####################################
#final data export
####################################

write.tree(mytree, here("data", "species_tree.csv"))


#write.tree(mytree, file = "~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Fitness-and-dispersal-MA/data/species_tree.tre") #save the tree 
# mytree <- read.tree(file = "~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Fitness-and-dispersal-MA/data/species_tree.tre") #if you need to read the tree


head(Species_info)
