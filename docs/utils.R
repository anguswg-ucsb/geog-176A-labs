

# function takes sf object and character string and creates plot
tess_plots = function(sf_data, name) {
  ggplot() +
    geom_sf(data = sf_data, fill = 'white', size = 0.2, col = 'navy') +
    labs(title = name,
         caption = paste(max(sf_data$id), ' grid objects represented')) +
    theme_void()
}

# returns conus object
get_conus = function(data, var){
  filter(data, !get(var) %in%
           c("Hawaii", "Puerto Rico", "Alaska",
             "Guam", "District of Columbia"))
}
# Point- in - polygon analysis
pip_function = function(points, polygon, bar){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(bar)) %>%
    setNames(c(bar, "n")) %>%
    left_join(polygon, by = bar) %>%
    st_as_sf()
}


# Removes all whitespace from dataframe
remove_all_ws = function(string){
  return(gsub(" ", "", str_squish(string)))
}


plot_pip = function(data, text){
  ggplot() +
    geom_sf(data = data, aes(fill = n), alpha = .9, size = .2) +
    viridis::scale_fill_viridis() +
    theme_void() +
    theme(plot.title = element_text(face = "bold", color = "black", size = 24), plot.subtitle = element_text(size = 12),
          plot.caption = element_text(face = 'bold', size = 12), legend.title = element_text(face = 'bold'),
          legend.text = element_text(face = 'bold')) +
    labs(title = text,
         subtitle = 'Data source: National Inventory of Dams',
         fill = 'Number of dams',
         caption = paste0(sum(data$n), " dams")) +
    theme(aspect.ratio = .5)
}


buffer_fun = function(df, well, buff, state) {
  buffer = st_buffer(df[well,], buff)
  near = st_intersection(df[,], buffer)

  plot = ggplot() +
    geom_sf(data = state) +
    geom_sf(data = buffer, fill = NA) +
    geom_sf(data = near, col = "darkred", size = .5) +
    theme_void()
  print(plot)
  return(near)
}


























