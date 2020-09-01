

# function takes sf object and character string and creates plot
tess_plots = function(sf_data, name) {
  ggplot() +
    geom_sf(data = sf_data, fill = 'white', size = 0.2, col = 'navy') +
    labs(title = name,
         caption = paste(max(sf_data$id), ' grid objects represented')) +
    theme_void()
}
