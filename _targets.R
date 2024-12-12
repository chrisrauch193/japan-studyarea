# _targets.R
library(targets)
library(tarchetypes)
library(mregions2)
library(sf)
library(dplyr)
library(ggplot2)
library(archive)
library(rmapshaper) # for ms_simplify
library(lwgeom)     # for st_remove_holes

# Load functions
lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

# Define target pipeline
list(
  # --- Bounding Box ---
  tar_target(
    name = bounding_box,
    command = {
      # Expanded bounding box to better capture Kuroshio Current region
      st_bbox(c(xmin = 110, xmax = 160, ymin = -5, ymax = 50), crs = st_crs(4326))
    }
  ),
  
  # --- Ogasawara Islands ---
  tar_target(
    name = ogasawara_islands,
    command = {
      gaz_search("Ogasawara", like = TRUE) |>
        gaz_geometry() |>
        mr_tidy(source_name = "Ogasawara Islands")
    }
  ),
  tar_target(
    name = is_ogasawara_in_bbox,
    command = {
      check_region_in_bbox(ogasawara_islands, bounding_box)
    }
  ),
  
  # --- Izu Islands ---
  tar_target(
    name = izu_islands,
    command = {
      izu_islands <- gaz_search("Izu", like = TRUE)
      if (nrow(izu_islands) > 0) {
        izu_islands <- izu_islands[izu_islands$status != "deleted", ]
        if (nrow(izu_islands) > 0) {
          gaz_geometry(izu_islands) |>
            mr_tidy(source_name = "Izu Islands")
        } else {
          warning("All found records for Izu Islands are deleted.")
          NULL
        }
      } else {
        warning("No records found for Izu Islands.")
        NULL
      }
    }
  ),
  tar_target(
    name = is_izu_in_bbox,
    command = {
      check_region_in_bbox(izu_islands, bounding_box)
    }
  ),
  
  # --- Ryukyu Archipelago ---
  tar_target(
    name = ryukyu_archipelago,
    command = {
      gaz_search("Ryukyu", like = TRUE) |>
        gaz_geometry() |>
        mr_tidy(source_name = "Ryukyu Archipelago")
    }
  ),
  
  # --- Japan EEZ ---
  tar_target(
    name = japan_eez,
    command = {
      eez_records <- gaz_search_by_type(70)
      japan_eez <- eez_records |>
        dplyr::filter(grepl("Japan", preferredGazetteerName, ignore.case = TRUE)) |>
        dplyr::filter(status == "standard")
      
      if (nrow(japan_eez) == 0) {
        stop("No records found for Japan's EEZ.")
      }
      
      gaz_geometry(japan_eez) |> 
        mr_tidy(source_name = "Japan EEZ")
    }
  ),
  
  # --- Taiwan ---
  tar_target(
    name = taiwan,
    command = {
      taiwan <- gaz_search("Taiwan")
      if (nrow(taiwan) > 0) {
        taiwan <- taiwan[taiwan$status != "deleted", ]
        if (nrow(taiwan) > 0) {
          gaz_geometry(taiwan) |>
            mr_tidy(source_name = "Taiwan")
        } else {
          warning("All found records for Taiwan are deleted.")
          NULL
        }
      } else {
        warning("No records found for Taiwan")
        NULL
      }
    }
  ),
  
  # --- Coral Reefs (WCMC Polygons) ---
  tar_target(
    name = coral_reefs,
    command = {
      coral_reefs_all <- st_read("data/WCMC008_CoralReef2021_Py_v4_1.shp", quiet = TRUE)
      coral_reefs_all <- st_make_valid(coral_reefs_all)
      coral_reefs_all <- st_transform(coral_reefs_all, st_crs(bounding_box))
      
      coral_reefs_clip <- st_intersection(coral_reefs_all, st_as_sfc(bounding_box))
      
      coral_reefs_clip$mrgid <- NA
      coral_reefs_clip$name <- "Coral Reef"
      coral_reefs_clip$latitude <- NA
      coral_reefs_clip$longitude <- NA
      coral_reefs_clip$source <- "UNEP-WCMC"
      
      coral_reefs_clip <- coral_reefs_clip |>
        select(mrgid, name, latitude, longitude, source, geometry)
      
      # Less simplification here, or even none, we will handle after union
      coral_reefs_clip
    }
  ),
  
  # --- Aggregate Layers ---
  tar_target(
    name = study_area,
    command = {
      study_area <- aggregate_areas(list(japan_eez, ryukyu_archipelago, taiwan, izu_islands, ogasawara_islands, coral_reefs))
      study_area
    }
  ),
  
  # --- Clip to Bounding Box ---
  tar_target(
    name = study_area_clip,
    command = {
      st_intersection(study_area, st_as_sfc(bounding_box))
    }
  ),
  
  # --- Save Study Area as GeoPackage ---
  tar_target(
    name = save_study_area_gpkg,
    command = {
      st_write(study_area_clip, "data/output.gpkg", driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      "data/output.gpkg"
    },
    format = "file"
  ),
  
  # --- Save Study Area as Shapefile ---
  tar_target(
    name = save_study_area_shp,
    command = {
      st_write(study_area_clip, "data/output.shp", driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
      "data/output.shp"
    },
    format = "file"
  ),
  
  # --- Create and Save Map ---
  tar_target(
    name = study_area_map,
    command = {
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