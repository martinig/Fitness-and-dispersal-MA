#code to create my phylogenetic plot
#code written by P. Pottier and A. R. Martinig
#last edited March 27, 2024 by A. R. Martinig

#basic plots that do work

plot(mytree, show.tip.label = T, cex = 0.8, no.margin = TRUE)

 ggtree(mytree, layout="circular") +                    
   geom_tiplab( ) 

ggtree(mytree, layout="circular", branch.length = "none")
   
   
      
#the fancy plot

data_for_tree <- df %>% 
  mutate(
    natal = as.numeric(dispersal_type == "natal"),
    breeding = as.numeric(dispersal_type == "breeding"),
    both = as.numeric(dispersal_type == "B"),
    survival = as.numeric(fitness_higher_level == "survival"),
    reproduction = as.numeric(fitness_higher_level == "reproduction"),
    indirect = as.numeric(fitness_higher_level == "indirect fitness")
  ) %>%
  group_by(species_cleaned) %>%
  mutate(
    #tip.label = as.factor(gsub("_", " ", species_cleaned)),
    n_es = sum(n = n()),
    across(c(natal, breeding, both, survival, reproduction, indirect), ~ ifelse(sum(.) > 1, 1, .), .names = "sum_{.col}")) %>%  # In some cases, values were repeated, so need to replace values above 1 by '1'
  mutate(across(all_of(c("natal", "breeding", "both", "survival", "reproduction", "indirect")), as.factor), # convert back to factor for the plot
    # If both natal and breeding, indicate 'both', if only natal, indicate 'natal', otherwise indicate 'breeding'
    dispersal_pattern = case_when(
      sum_natal == 1 & sum_breeding == 0 & sum_both == 0 ~ "Natal dispersal",
      sum_natal == 0 & sum_breeding == 1 & sum_both == 0 ~ "Breeding dispersal",
      TRUE ~ "Both types"
    ),
# If survival, reproduction, or indirect fitness, indicate 'multiple', if only survival, indicate 'survival', otherwise indicate 'reproductive'    
    fitness_measure = case_when(
      sum_survival == 1 & sum_reproduction == 0 & sum_indirect == 0 ~ "Survival",
      sum_survival == 0 & sum_reproduction == 1 & sum_indirect == 0 ~ "Reproduction",
      sum_survival == 0 & sum_reproduction == 0 & sum_indirect == 1 ~ "Indirect",
      TRUE ~ "Multiple")) %>%
  ungroup() %>%
  dplyr::select(species_cleaned, n_es, dispersal_pattern, fitness_measure) %>%
  distinct()

str(data_for_tree)
head(data_for_tree)

data_for_tree_plot<-left_join(data_for_tree, (df%>%select(species_cleaned, species_class)), by ="species_cleaned") %>%
  distinct()

p <- ggtree(mytree, layout = "circular", lwd = 0.75) %<+% data_for_tree_plot  # link plot to data
p # Circular tree

p2 <- p + 
	geom_fruit(geom = geom_tile, mapping = aes(fill = dispersal_pattern), width = 2, offset = 0.085) + 
    	scale_fill_manual(values = c("#8c510a", "#bf812d", "#dfc27d"), name="Dispersal type")  # Create tiles to indicate which metric was used for this species
#p2

p3 <- p2 + new_scale_fill() + 
	geom_fruit(geom = geom_tile, 
		mapping = aes(fill = fitness_measure), 
		offset = 0.1, 
		width = 2) + 
	scale_fill_manual(values = c("#01665e", "#80cdc1", "#c7eae5"), name="Fitness measure")  # Create tiles to indicate which metric was used for this species
#p3

# Display number of effect sizes and species classes
p4 <- p3 + 
	new_scale_fill() + 
	geom_fruit(geom = geom_bar, 
		mapping = aes(x = n_es, fill=species_class), 
		stat = "identity", 
		col = "gray97", 
		orientation = "y", 
		axis.params = list(axis = "x", text.angle = -90, hjust = 0, text.size = 3), 
		grid.params = list(alpha = 0.35), 
		offset = 0.085, 
		pwidth = 0.55, 
		alpha = 0.8) + 
	scale_fill_manual(values = c("#762a83", "black", "#c51b7d", "gray47", "#f1b6da", "#c2a5cf"), name = "Class", breaks=c( "Arachnida", "Insecta", "Actinopterygii", "Mammalia", "Reptilia", "Aves")) + 
	theme(legend.position = "right", 
		legend.spacing.y = unit(5, "pt"), 
		legend.box = "vertical", 
		legend.box.just = "left", 
		legend.box.spacing = unit(-5, "pt"), 
		legend.text = element_text(size = 12), 
		legend.title = element_text(size = 14, face = "bold"), 
		legend.key.size = unit(20, "pt")) + 
	guides(fill = guide_legend(order = 1))  
p4

ggsave("~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Fitness-and-dispersal-MA/figures/phylogenetic plots/phylo plot.pdf")
#export image as 800x700


library(ggimage)

setwd("~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Fitness-and-dispersal-MA/figures/silhouettes")
p5 <- p4 + geom_image(x=10, y=50, image=paste0("red squirrel.png"), size=.05)
p5
