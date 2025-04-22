#getting all the plots pretty for a final publication

plot_overall<-orchard_plot(mod1, xlab = "Effect Size: Zr", group = "paperID", 
                           trunk.size = 1.2,
                           branch.size = 1.5,
                           alpha = 0.2,
                           angle = 90, 
                           legend.pos ="none",
                           k.pos="none") + 
  scale_fill_manual(values = c("darkgrey"))+
  scale_colour_manual(values = c("darkgrey"))+
    theme(
    legend.direction = "vertical",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))



plot_species_class<-orchard_plot(mod_class, 
    mod = "species_class", xlab = "Effect Size: Zr", group = "paperID", 
    trunk.size = 1.2,
    branch.size = 1.5,
    alpha = 0.2,    
    angle = 0, 
    legend.pos ="none",
    #tree.order=c("Aves", "Reptilia", "Mammalia", "Actinopterygii", "Insecta", "Arachnida"),
    k.pos="none") +
  scale_fill_manual(values = c("#8C6D31", "#6E8B92", "#997B8A", "#5C715E", "#CBB994", "#A4C2A8"))+
  scale_colour_manual(values = c("#8C6D31", "#6E8B92", "#997B8A", "#5C715E", "#CBB994", "#A4C2A8"))+
    theme(
    legend.direction = "vertical",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))


plot_mod_fit<-orchard_plot(mod_fit1, mod = "fitness_higher_level", xlab = "Effect Size: Zr", group = "paperID", 
                           trunk.size = 1.2,
                           branch.size = 1.5,
                           alpha = 0.2,
                           angle = 0, 
                           legend.pos ="none",
                           k.pos="none") +
  scale_fill_manual(values = c("#6E8B92", "#A1C181", "#B4C5B9"))+
  scale_colour_manual(values = c("#6E8B92", "#A1C181", "#B4C5B9"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))

plot_mod_sex<-orchard_plot(mod_sex, mod = "sex", xlab = "Effect Size: Zr", group = "paperID", 
                           trunk.size = 1.2,
                           branch.size = 1.5,
                           alpha = 0.2, 
                           angle = 0, 
                           tree.order=c("Both", "Male", "Female"),
                           legend.pos ="none",
                           k.pos="none") +
  scale_fill_manual(values = c("#997B8A", "#CBB994", "#A1C181"))+
  scale_colour_manual(values = c("#997B8A", "#CBB994", "#A1C181"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))

plot_mod_age<-orchard_plot(mod_age1, mod = "age_class_clean", xlab = "Effect Size: Zr", group = "paperID", 
                           trunk.size = 1.2,
                           branch.size = 1.5,
                           alpha = 0.2,
                           angle = 0, 
                           tree.order=c("Mix", "Juvenile", "Adult"),
                           legend.pos ="none",
                           k.pos="none") +
  scale_fill_manual(values = c("#5C715E", "#A4C2A8", "#8C6D31"))+
  scale_colour_manual(values = c("#5C715E", "#A4C2A8", "#8C6D31"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))




plot_mod_phase<-orchard_plot(mod_phase, mod = "dispersal_phase", xlab = "Effect Size: Zr", group = "paperID", 
                             trunk.size = 1.2,
                             branch.size = 1.5,
                             alpha = 0.2, 
                             angle = 0, 
                             legend.pos ="none",
                             tree.order=c("Settlement", "Prospection"),
                             k.pos="none") +
  scale_fill_manual(values = c("#6B9080", "#B06C4F"))+
  scale_colour_manual(values = c("#6B9080", "#B06C4F"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))


plot_mod_disp<-orchard_plot(mod_disp, 
                            mod = "dispersal_type", xlab = "Effect Size: Zr", group = "paperID", 
                            trunk.size = 1.2,
                            branch.size = 1.5,
                            alpha = 0.2, 
                            angle = 0, 
                            legend.pos ="none",
                            k.pos="none") +
  scale_fill_manual(values = c("#6E8B92", "#CBB994", "#A4C2A8"))+
  scale_colour_manual(values = c("#6E8B92", "#CBB994", "#A4C2A8"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))


plot_mod_gen<-orchard_plot(mod_gen, mod = "whose_fitness", xlab = "Effect Size: Zr", group = "paperID", 
                           trunk.size = 1.2,
                           branch.size = 1.5,
                           alpha = 0.2,
                           angle = 0, 
                           legend.pos ="none",
                           k.pos="none") +
  scale_fill_manual(values = c("#5C715E", "#997B8A"))+
  scale_colour_manual(values = c("#5C715E", "#997B8A"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))

plot_mod_dur<-bubble_plot(mod_dur, mod = "ln_study_duration", group = "paperID", xlab = "Study duration", ylab = "Effect Size: Zr", g = TRUE, 
                          legend.pos ="none",
                          k.pos="none") +
  coord_flip() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", angle=0), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))




plot_mod_type<-orchard_plot(mod_type, 
                            mod = "study_type", xlab = "Effect Size: Zr", group = "paperID", 
                            trunk.size = 1.2,
                            branch.size = 1.5,
                            alpha = 0.2,      
                            angle = 0, 
                            tree.order=c("Semi-natural", "Natural"),
                            legend.pos ="none",
                            k.pos="none") +
  scale_fill_manual(values = c("#C29B6B", "#88A096"))+
  scale_colour_manual(values = c("#C29B6B", "#88A096"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))

plot_mod_design<-orchard_plot(mod_design, 
                              mod = "study_design", xlab = "Effect Size: Zr", group = "paperID", 
                              trunk.size = 1.2,
                              branch.size = 1.5,
                              alpha = 0.2, 
                              angle = 0, 
                              legend.pos ="none",
                              k.pos="none") +
  scale_fill_manual(values = c("#678D58", "#B6B8B1"))+
  scale_colour_manual(values = c("#678D58", "#B6B8B1"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))

plot_mod_focus<-orchard_plot(mod_focus, mod = "fitness_main_focus", xlab = "Effect Size: Zr", group = "paperID", 
                             trunk.size = 1.2,
                             branch.size = 1.5,
                             alpha = 0.2,    
                             angle = 0, 
                             legend.pos ="none",
                             k.pos="none") +
  scale_fill_manual(values = c("#9EAC9D", "#4E5F66"))+
  scale_colour_manual(values = c("#9EAC9D", "#4E5F66"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))

plot_mod_confirm<-orchard_plot(mod_confirm, mod = "confirmation_bias", xlab = "Effect Size: Zr", group = "paperID", 
                               trunk.size = 1.2,
                               branch.size = 1.5,
                               alpha = 0.2, 
                               angle = 0, 
                               legend.pos ="none",
                               k.pos="none") +
  scale_fill_manual(values = c("#6E8B92", "#B7C9A8", "#BC5A45", "#997B8A"))+
  scale_colour_manual(values = c("#6E8B92", "#B7C9A8", "#BC5A45", "#997B8A"))+
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), #eliminates background grid
    panel.grid.minor = element_blank(), #eliminates background grid
    panel.border = element_blank(), #eliminates plot border
    panel.background = element_blank(),
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"), 
    axis.text.y = element_text(size = 12, colour = "black", hjust=0.95), 
    legend.title = element_text(size = 12, colour = "black"), 
    legend.text = element_text(size = 11, colour = "black"))



#figure 1, biological context
plot_grid(plot_species_class, plot_mod_fit, 
          plot_mod_sex, plot_mod_age, 
          labels= "AUTO", label_x=0.1, label_y=1,
          rel_heights=c(1.6, 1), ncol=2, align = "v", axis = 'l')
#export 600x500

#figure 2, behavioural and environmental context
plot_grid(plot_mod_phase, plot_mod_disp,
          plot_mod_gen, plot_mod_dur,  
          labels= "AUTO", label_x=0.1, label_y=1,
          rel_heights=c(1, 1), ncol=2, align = "v", axis = 'l')
#export 600x500

#figure 3, methodological/analytical dimensions
plot_grid(plot_mod_type, plot_mod_design,
          plot_mod_focus, plot_mod_confirm, 
          labels= "AUTO", label_x=0.1, label_y=1,
          rel_heights=c(1, 1), ncol=2, align = "v", axis = 'l')
#export 600x500

#figure 2 and 3 together, behavioural and environmental context
plot_grid(plot_mod_disp, plot_mod_phase,
          plot_mod_gen, plot_mod_dur,
          plot_mod_type, plot_mod_design,
          plot_mod_focus, plot_mod_confirm, 
          labels= "AUTO", label_x=0.1, label_y=1,
          ncol=2, align = "v", axis = 'l')
#export 600x800



#plot without overall effect, but with study duration
plot_grid(plot_species_class, plot_mod_fit, 
  plot_mod_sex, plot_mod_age, 
  plot_mod_disp, plot_mod_phase,
  plot_mod_type, plot_mod_design,
  plot_mod_gen, plot_mod_dur, 
  plot_mod_focus, plot_mod_confirm, 
  labels= "AUTO", label_x=0.1, label_y=1,
  rel_heights=c(1.6, 1, 1, 1, 1, 1), ncol=2, nrow=6, align = "v", axis = 'l')

#export 800 x 1000