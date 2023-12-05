plot_flipper_figure <- function(flipper_data){
  flipper_data %>% 
    ggplot(aes(x = species, y = flipper_length_mm)) +
    geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
    geom_jitter(aes(color = species), alpha = 0.3, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_x_discrete(labels=c("Adelie","Chinstrap","Gentoo")) +
    labs(x = "Penguin Species",
         y = "Flipper length (mm)") +
    theme_bw()
}

# Save the plot as a png and define the size, resolution, and scaling
save_flipper_plot_png <- function(penguins_clean, filename, size, res, scaling){
  agg_png(filename, width = size, 
          height = size, 
          units = "cm", 
          res = res, 
          scaling = scaling)
  penguinexplor <- plot_exploratory_figure(penguins_clean)
  print(penguinexplor)
  dev.off()
}

# Save the plot as a svg and define the size and scaling
save_flipper_plot_svg <- function(penguins_flippers, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  flipper_box <- plot_flipper_figure(penguins_flippers)
  print(flipper_box)
  dev.off()
}

plot_exploratory_figure <- function(penguins_clean) {
  ggplot(data = penguins_clean, aes(x = body_mass_g, y = culmen_length_mm, color = species)) +
    geom_point()
}
