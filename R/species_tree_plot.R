#code to create my phylogenetic plot
#code written by P. Pottier and A. R. Martinig
#last edited October 31, 2025 by A. R. Martinig


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

p <- ggtree(mytree, layout = "circular", lwd = 0.75, aes(colour=species_class), key_glyph = 'rect')
p <- p %<+% data_for_tree_plot + # link plot to data
  scale_colour_manual(values = c("#EE3377", "#332288", "#EECC66", "#6699CC", "#EE7733", "#882255"), #colours for taxa
      name="Class", 
      breaks=c( "Arachnida", "Insecta", "Actinopterygii", "Mammalia", "Reptilia", "Aves")) + 
  guides(colour=guide_legend(order = 1))
p # Circular tree

p2 <- p + 
	geom_fruit(geom = geom_tile, mapping = aes(fill = dispersal_pattern), width = 2, offset = 0.085) + 
  scale_fill_manual(values = c("#dfc27d", "#bf812d", "#8c510a"), 
      name="Dispersal type", 
      breaks=c("Natal dispersal", "Breeding dispersal", "Both types")) + # Create tiles to indicate which dispersal type occurred for this species
  guides(fill=guide_legend(order = 2))
p2

p3 <- p2 + 
  new_scale_fill() + 
	geom_fruit(geom = geom_tile, 
		mapping = aes(fill = fitness_measure), 
		offset = 0.1, 
		width = 2) + 
	scale_fill_manual(values = c("#c7eae5", "#80cdc1", "#01665e"), 
	 name="Fitness measure", 
	 breaks=c("Survival", "Reproduction", "Multiple"))+ # Create tiles to indicate which fitness metric was used for this species
  guides(fill=guide_legend(order = 3))
p3

# Display number of effect sizes and species classes as bars
p4 <- p3 + 
	new_scale_fill() + 
	geom_fruit(geom = geom_bar, 
	  mapping = aes(x = log(n_es+0.25)), 
		#mapping = aes(x = n_es, fill=species_class), 
		stat = "identity", 
		col = "gray97", 
		fill="black",
		orientation = "y", 
		axis.params = list(axis = "x", text.angle = -90, hjust = 0, text.size = 3), 
		grid.params = list(alpha = 0.35), 
		offset = 0.085, 
		pwidth = 0.55, 
		alpha = 0.8) + 
	#scale_fill_manual(values = c("#EE3377", "#332288", "#EECC66", "#6699CC", "#EE7733", "#882255"), name = "Class", breaks=c( "Arachnida", "Insecta", "Actinopterygii", "Mammalia", "Reptilia", "Aves")) + 
	theme(legend.position = "right", 
		legend.spacing.y = unit(5, "pt"), 
		legend.box = "vertical", 
		legend.box.just = "left", 
		legend.box.spacing = unit(-5, "pt"), 
		legend.text = element_text(size = 12), 
		legend.title = element_text(size = 14, face = "bold"), 
		legend.key.size = unit(20, "pt")) #+ 
	#guides(fill = guide_legend(order = 4))
p4

#0 = 1, 1=2.718282, 2=7.389056, 3=20.08554, 4=54.59815, 5=148.4132


ggsave("~/Documents/Files/Manuscripts/Martinig et al. 2025 (meta analysis)/Fitness-and-dispersal-MA/figures/phylogenetic plots/phylo plot.pdf")
#export image as 800x700


library(ggimage)

setwd("~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Fitness-and-dispersal-MA/figures/silhouettes")
p5 <- p4 + geom_image(x=10, y=50, image=paste0("red squirrel.png"), size=.05)
p5

