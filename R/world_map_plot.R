#code to create my world map plot
#last edited July 16, 2024 by A. R. Martinig

# Load the necessary packages
  pacman::p_load(  
  		dplyr, 
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



map_df<-read.csv("~/Documents/Files/Post-docs/UNSW 2022-2024/Aim 1/Fitness-and-dispersal-MA/data/clean_data.csv") %>%
	group_by(species_cleaned) %>%
	mutate(n_es = sum(n = n()),
	       n_set=ifelse(n_es>19, 20, n_es),
	       n_log=log(n_es)) %>%
	ungroup() %>%
	dplyr::select(species_cleaned, species_class, country, lon, lat, dispersal_type, fitness_metric_clean, n_es, n_set, n_log) %>%
	distinct()

names(map_df)
head(map_df)
summary(map_df$n_es)
hist(map_df$n_es, breaks=30)
hist(map_df$n_set, breaks=10)
hist(map_df$n_log, breaks=10)




world_map = map_data("world")  %>% filter(! long > 180)

map <- ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(map_id = region),
    color = "black", fill = "grey90") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  geom_point(
    data = map_df,
    aes(lon, lat, color = n_log),
    alpha = 0.7, size = 3) + 
  labs(x = "Longitude", y = "Latitude", color = TeX("Effect sizes")) + 
   #scale_color_gradientn(
    #colors = brewer.pal(11, "RdYlBu"),
    #colors = brewer.pal(11, "PiYG"),
    #limits = c(min(map_df$n_es), max(map_df$n_es))) +
  scale_color_viridis(option="viridis", direction=-1) + 
  theme_map(base_size = 15)
map  

#0 = 1, 1=2.71, 2= 7.389056, 3= 20.08554, 4= 54.59815
  
map  + theme(legend.justification=c(0,-1)) +  theme(plot.margin = unit(c(-4, -1, -5, 0), "cm"))

#export image as 800x700


ggsave(map, filename = "./output/figures/fig-preds.png", width = 7.8, height = 10.1)


image_read("./output/figures/fig-preds.png")
