# R/functions.R
library(sf)
library(dplyr)
library(mregions2)
library(lwgeom)

# Helper function to transform and simplify geometries
prepare_geom <- function(geom) {
  geom |>
    st_make_valid() |>
    st_transform(crs = st_crs(4326)) |>
    st_simplify(dTolerance = 0.01)
}

# Function to check if a region is within the bounding box
check_region_in_bbox <- function(region, bounding_box) {
  if (nrow(region) > 0) {
    any(st_intersects(st_as_sfc(bounding_box), st_union(region), sparse = FALSE))
  } else {
    FALSE
  }
}

# Function to read and clip coral reefs
read_clip_coralreefs <- function(coral_reefs_path, bounding_box) {
  # Read coral reefs
  coral_reefs <- st_read(coral_reefs_path)
  
  # Make sure the geometries are valid
  coral_reefs <- st_make_valid(coral_reefs)
  
  # Clip to bounding box
  coral_reefs_clipped <- st_intersection(coral_reefs, st_as_sfc(bounding_box))
  
  # Select only POLYGON features
  coral_reefs_polygons <- coral_reefs_clipped |>
    filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
  
  # Prepare geometry
  coral_reefs_prepared <- coral_reefs_polygons |>
    prepare_geom()
  
  return(coral_reefs_prepared)
}

# ... (other functions remain the same)

# Function to tidy up mregions2 results, specifically for gaz_search
mr_tidy <- function(res, source_name = NULL) {
  # Check if the result is an sf object and convert it to sf if necessary
  if (inherits(res, "data.frame") && "geometry" %in% names(res)) {
    # Directly use st_as_sf if it's already a data frame with a geometry column
    geom <- st_as_sf(res, crs = 4326)
  } else if (inherits(res, "list") && !is.null(res[["geometry"]])) {
    # Convert to sf object if the geometry is in a list element
    geom <- st_as_sfc(res[["geometry"]], crs = 4326)
    # Convert the list to a data frame and set the geometry
    res <- as.data.frame(res)
    geom <- st_set_geometry(res, geom)
  } else if (inherits(res, "sf")) {
    # If it's already an sf object, no conversion needed
    geom <- res
  } else {
    stop("The result from gaz_search does not contain a valid geometry.")
  }
  
  # Add a source column if provided
  if (!is.null(source_name)) {
    geom$source <- source_name
  }
  
  # Select necessary columns and rename for consistency
  # Only select columns that are likely to be common across all records
  data <- geom |>
    select(MRGID, preferredGazetteerName, latitude, longitude, status, accepted, source) |>
    rename(name = preferredGazetteerName) |>
    prepare_geom()
  
  return(data)
}

# Function to aggregate areas
aggregate_areas <- function(areas) {
  # Filter out NULL or empty elements
  valid_areas <- areas[sapply(areas, function(x) !is.null(x) && inherits(x, "sf") && nrow(x) > 0)]
  
  # Check if there are any valid areas left
  if (length(valid_areas) == 0) {
    stop("No valid areas to aggregate.")
  }
  
  # Ensure all areas have the same CRS
  first_crs <- st_crs(valid_areas[[1]])
  valid_areas <- lapply(valid_areas, function(x) st_transform(x, first_crs))

  # Prepare a list of data frames with a unified set of columns
  unified_areas <- lapply(valid_areas, function(area) {
    # Ensure the geometry is valid
    area <- st_make_valid(area)
    
    # Ensure the presence of all required columns, add missing with NA
    required_cols <- c("mrgid", "name", "latitude", "longitude", "source", "geometry")
    for (col in required_cols) {
      if (!col %in% names(area)) {
        area[[col]] <- NA
      }
    }
    
    # Select only the required columns
    area |>
      select(all_of(required_cols))
  })
  
  # Combine all layers
  combined <- do.call(rbind, unified_areas)
  
  # Unary union to dissolve overlapping boundaries
  study_area <- st_union(combined)
  
  study_area <- st_make_valid(study_area)
  
  # Simplify the geometry
  study_area <- st_simplify(study_area, dTolerance = 0.01)
  
  return(study_area)
}