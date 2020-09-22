

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
