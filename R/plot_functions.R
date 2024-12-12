# R/plot_functions.R
library(ggplot2)
library(sf)

plot_study_area <- function(study_area, bounding_box, coral_reefs) {
  ggplot() +
    # Study area
    geom_sf(data = study_area, fill = "lightblue", color = "steelblue") +
    # Coral reefs
    geom_sf(data = coral_reefs, fill = "coral", color = "coral") +
    # Bounding box
    geom_sf(data = st_as_sfc(bounding_box), fill = NA, color = "red", linetype = "dashed") +
    # Coordinate limits and labels
    coord_sf(
      xlim = c(st_bbox(study_area)$xmin, st_bbox(study_area)$xmax),
      ylim = c(st_bbox(study_area)$ymin, st_bbox(study_area)$ymax),
      expand = FALSE
    ) +
    # Theme
    theme_minimal() +
    # Title
    labs(title = "Study Area: Japan, Ryukyu Archipelago, Taiwan, Izu, Ogasawara, and Coral Reefs")
}