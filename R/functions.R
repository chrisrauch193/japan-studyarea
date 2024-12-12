# R/functions.R
library(sf)
library(dplyr)
library(mregions2)
library(lwgeom)
library(rmapshaper)

# Helper function to transform and simplify geometries
prepare_geom <- function(geom) {
  geom |>
    st_make_valid() |>
    st_transform(crs = st_crs(4326)) 
  # We'll do final simplification steps later in aggregate_areas 
  # to ensure consistent smoothing across all datasets.
}

# Function to check if a region is within the bounding box
check_region_in_bbox <- function(region, bounding_box) {
  if (nrow(region) > 0) {
    any(st_intersects(st_as_sfc(bounding_box), st_union(region), sparse = FALSE))
  } else {
    FALSE
  }
}

# Function to tidy up mregions2 results, specifically for gaz_search
mr_tidy <- function(res, source_name = NULL) {
  if (inherits(res, "data.frame") && "geometry" %in% names(res)) {
    geom <- st_as_sf(res, crs = 4326)
  } else if (inherits(res, "list") && !is.null(res[["geometry"]])) {
    geom <- st_as_sfc(res[["geometry"]], crs = 4326)
    res <- as.data.frame(res)
    geom <- st_set_geometry(res, geom)
  } else if (inherits(res, "sf")) {
    geom <- res
  } else {
    stop("The result from gaz_search does not contain a valid geometry.")
  }
  
  if (!is.null(source_name)) {
    geom$source <- source_name
  }
  
  data <- geom |>
    select(any_of(c("MRGID", "preferredGazetteerName", "latitude", "longitude", "status", "accepted", "source", "geometry"))) |>
    rename_with(~ tolower(gsub("([a-z])([A-Z])", "\\1_\\2", .x))) |>
    rename(name = preferred_gazetteer_name) |>
    prepare_geom()
  
  return(data)
}


# Function to aggregate areas
aggregate_areas <- function(areas) {
  valid_areas <- areas[sapply(areas, function(x) !is.null(x) && inherits(x, "sf") && nrow(x) > 0)]
  
  if (length(valid_areas) == 0) {
    stop("No valid areas to aggregate.")
  }
  
  first_crs <- st_crs(valid_areas[[1]])
  valid_areas <- lapply(valid_areas, function(x) st_transform(x, first_crs))
  
  all_colnames <- unique(unlist(lapply(valid_areas, names)))
  
  unified_areas <- lapply(valid_areas, function(area) {
    area <- st_make_valid(area)
    missing_cols <- setdiff(all_colnames, names(area))
    for (col in missing_cols) {
      area[[col]] <- NA
    }
    area <- area[, all_colnames]
    area
  })
  
  combined <- do.call(rbind, unified_areas)
  
  # Union all areas
  study_area <- st_union(combined)
  study_area <- st_make_valid(study_area)
  
  # Extract polygonal geometries only
  study_area <- st_collection_extract(study_area, "POLYGON")
  
  # Remove very small polygons (<10km²)
  area_threshold <- units::set_units(1e7, "m^2")
  study_area <- study_area[st_area(study_area) > area_threshold, ]
  
  # Attempt to remove holes by polygonizing:
  # 1. Cast to MULTILINESTRING
  study_area_lines <- st_cast(study_area, "MULTILINESTRING", warn = FALSE)
  
  # 2. Polygonize to reconstruct polygons from boundary lines
  study_area_polygonized <- st_polygonize(study_area_lines)
  
  # 3. Extract polygons again
  study_area <- st_collection_extract(study_area_polygonized, "POLYGON")
  
  # Remove small polygons again if polygonization introduced any small slivers
  study_area <- study_area[st_area(study_area) > area_threshold, ]
  
  # Union again after polygonization and cleaning
  study_area <- st_union(study_area)
  
  # Smooth the geometry using mapshaper
  study_area <- rmapshaper::ms_simplify(study_area, keep = 0.95, keep_shapes = TRUE)
  
  # Final validity check and clean-up
  study_area <- st_make_valid(study_area)
  study_area <- st_buffer(study_area, 0)
  
  return(study_area)
}

