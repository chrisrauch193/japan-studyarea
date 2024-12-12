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
    select(any_of(c("MRGID", "preferredGazetteerName", "latitude", "longitude", "status", "accepted", "source", "geometry"))) |>
    rename_with(~ tolower(gsub("([a-z])([A-Z])", "\\1_\\2", .x))) |> # Convert column names to snake_case
    rename(name = preferred_gazetteer_name) |>
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
  
  # 1. Identify all unique column names across all data frames
  all_colnames <- unique(unlist(lapply(valid_areas, names)))
  
  # 2. Ensure each data frame has all identified columns
  unified_areas <- lapply(valid_areas, function(area) {
    # Ensure the geometry is valid
    area <- st_make_valid(area)
    
    # Add missing columns with NA values
    missing_cols <- setdiff(all_colnames, names(area))
    for (col in missing_cols) {
      area[[col]] <- NA
    }
    
    # Reorder columns to match the order of all_colnames
    area <- area[, all_colnames]
    return(area)
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