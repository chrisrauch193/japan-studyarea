# _targets.R
library(targets)
library(tarchetypes)
library(mregions2)
library(sf)
library(dplyr)
library(ggplot2)
library(archive)

# Load functions
lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

# Define target pipeline
list(
  # --- Bounding Box ---
  tar_target(
    name = bounding_box,
    command = {
      # Define the bounding box for the study area
      st_bbox(c(xmin = 115, xmax = 155, ymin = 0, ymax = 45), crs = st_crs(4326))
    }
  ),
  
  # --- Ogasawara Islands ---
  tar_target(
    name = ogasawara_islands,
    command = {
      # Retrieve Ogasawara Islands from Marine Regions
      gaz_search("Ogasawara", like = TRUE) |>
        gaz_geometry() |>
        mr_tidy(source_name = "Ogasawara Islands")
    }
  ),
  tar_target(
    name = is_ogasawara_in_bbox,
    command = {
      # Check if Ogasawara Islands are within the bounding box
      check_region_in_bbox(ogasawara_islands, bounding_box)
    }
  ),
  
  # --- Izu Islands ---
  tar_target(
    name = izu_islands,
    command = {
      # Retrieve Izu Islands from Marine Regions, handling deleted records
      izu_islands <- gaz_search("Izu", like = TRUE)
      
      # Check if any records were found and have status "deleted"
      if (nrow(izu_islands) > 0) {
        izu_islands <- izu_islands[izu_islands$status != "deleted", ]
        if (nrow(izu_islands) > 0) {
          # Return the geometry for the valid records
          return(gaz_geometry(izu_islands) |>
                   mr_tidy(source_name = "Izu Islands"))
        } else {
          warning("All found records for Izu Islands are deleted.")
          return(NULL)
        }
      } else {
        warning("No records found for Izu Islands.")
        return(NULL)
      }
    }
  ),
  tar_target(
    name = is_izu_in_bbox,
    command = {
      # Check if Izu Islands are within the bounding box
      check_region_in_bbox(izu_islands, bounding_box)
    }
  ),
  
  # --- Ryukyu Archipelago ---
  tar_target(
    name = ryukyu_archipelago,
    command = {
      # Retrieve Ryukyu Archipelago from Marine Regions
      gaz_search("Ryukyu", like = TRUE) |>
        gaz_geometry() |>
        mr_tidy(source_name = "Ryukyu Archipelago")
    }
  ),
  
  # --- Japan EEZ ---
  tar_target(
    name = japan_eez,
    command = {
      # Retrieve Japan's EEZ from Marine Regions using type ID 70 (EEZ)
      eez_records <- gaz_search_by_type(70)
      
      # Filter for Japan's EEZ specifically
      japan_eez <- eez_records |>
        dplyr::filter(grepl("Japan", preferredGazetteerName, ignore.case = TRUE)) |>
        dplyr::filter(status == "standard")
      
      # Check if any records were found
      if (nrow(japan_eez) == 0) {
        stop("No records found for Japan's EEZ.")
      }
      
      # Return the geometry for Japan's EEZ, adding source information, and then tidy
      gaz_geometry(japan_eez) |> 
        mr_tidy(source_name = "Japan EEZ")
    }
  ),
  
  # --- Taiwan ---
  tar_target(
    name = taiwan,
    command = {
      # Retrieve Taiwan from Marine Regions, handling deleted records
      taiwan <- gaz_search("Taiwan")
      
      # Check if any records were found and have status "deleted"
      if (nrow(taiwan) > 0) {
        taiwan <- taiwan[taiwan$status != "deleted", ]
        if (nrow(taiwan) > 0) {
          # Return the geometry for the valid records
          return(gaz_geometry(taiwan) |>
                   mr_tidy(source_name = "Taiwan"))
        } else {
          warning("All found records for Taiwan are deleted.")
          return(NULL)
        }
      } else {
        warning("No records found for Taiwan")
        return(NULL)
      }
    }
  ),
  
  # --- Coral Reefs (WCMC Polygons) ---
  tar_target(
    name = coral_reefs,
    command = {
      # Read the shapefile
      coral_reefs_all <- st_read("data/WCMC008_CoralReef2021_Py_v4_1.shp")
      
      # Make the geometries valid
      coral_reefs_all <- st_make_valid(coral_reefs_all)
      
      # Transform to the same CRS as the bounding box
      coral_reefs_all <- st_transform(coral_reefs_all, st_crs(bounding_box))
      
      # Clip to the bounding box
      coral_reefs_clip <- st_intersection(coral_reefs_all, st_as_sfc(bounding_box))
      
      coral_reefs_clip$mrgid <- NA
      coral_reefs_clip$name <- "Coral Reef"
      coral_reefs_clip$latitude <- NA
      coral_reefs_clip$longitude <- NA
      coral_reefs_clip$source <- "UNEP-WCMC"
      
      # Select and rename columns to match other datasets
      coral_reefs_clip <- coral_reefs_clip |>
        select(mrgid, name, latitude, longitude, source, geometry)
      
      # Simplify the geometry
      coral_reefs_clip <- st_simplify(coral_reefs_clip, dTolerance = 0.01)
      
      return(coral_reefs_clip)
    }
  ),
  
  # --- Aggregate Layers ---
  tar_target(
    name = study_area,
    command = {
      # Combine all layers
      study_area <- aggregate_areas(list(japan_eez, ryukyu_archipelago, taiwan, izu_islands, ogasawara_islands, coral_reefs))
      study_area
    }
  ),
  
  # --- Clip to Bounding Box ---
  tar_target(
    name = study_area_clip,
    command = {
      # Clip study area to bounding box
      st_intersection(study_area, st_as_sfc(bounding_box))
    }
  ),
  
  # --- Save Study Area ---
  tar_target(
    name = save_study_area,
    command = {
      # Save study area as GeoPackage
      st_write(study_area_clip, "data/output.gpkg", driver = "GPKG", delete_dsn = TRUE)
      "data/output.gpkg"
    },
    format = "file"
  ),
  
  # --- Create and Save Map ---
  tar_target(
    name = study_area_map,
    command = {
      # Create a basic map of the study area
      plot_study_area(study_area_clip, bounding_box, coral_reefs)
    }
  ),
  tar_target(
    name = save_map,
    command = {
      ggsave("output/study_area.png", plot = study_area_map, width = 8, height = 6)
      "output/study_area.png"
    },
    format = "file"
  ),
  
  # --- Generate Report (Optional) ---
  tar_render(report, "report.Rmd")
)