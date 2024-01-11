#code to create my world map plot
#last edited Jan 11, 2024 by A. R. Martinig

# Load the necessary packages
  pacman::p_load(  
  		metafor, 
  		flextable, 
  		raster, 
  		rasterVis,  
  		patchwork, 
  		RColorBrewer, 
  		phytools, 
  		viridis, 
  		brms, 
  		latex2exp, 
  		orchaRd, 
  		multcomp, 
  		ggbeeswarm, 
  		magick, 
  		ggimage, 
  		gt, 
  		performance, 
  		ggthemes, 
  		mapproj)



map_df<-df %>%
	group_by(species_cleaned) %>%
	mutate(n_es = sum(n = n())) %>%
	ungroup() %>%
	select(species_cleaned, species_class, country, lon, lat, dispersal_type, fitness_metric_clean, n_es) %>%
	distinct()

head(map_df)



world_map = map_data("world")  %>% filter(! long > 180)


map <- ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(map_id = region),
    color = "black", fill = "lightgray") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  geom_point(
    data = map_df,
    aes(lon, lat, color = n_es),
    alpha = 0.7, size = 3) + 
  labs(x = "Longitude", y = "Latitude", color = TeX("Effect sizes")) + 
  scale_color_viridis(option="viridis") + 
  theme_map(base_size = 15)
  
map  + theme(legend.justification=c(0,-1)) +  theme(plot.margin = unit(c(-4, -1, -5, 0), "cm"))



ggsave(p, filename = "./output/figures/fig-preds.png", width = 7.8, height = 10.1)

image_read("./output/figures/fig-preds.png")
