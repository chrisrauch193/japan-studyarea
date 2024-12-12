# Japan, Kuroshio Current, and Ryukyu Archipelago Study Area

This repository defines a study area around Japan, including the Ryukyu Archipelago, the broader region influenced by the Kuroshio Current, Taiwan, the Ogasawara Islands, the Izu Islands, and coral reefs. This is for modeling anemonefish and their hosts.

## Requirements

-   R (>= 4.0)
-   `targets` package
-   `mregions2` package
-   `sf` package
-   `dplyr` package
-   `ggplot2` package
-   `terra` package
-   `rmarkdown` package
-   `archive` package

## Usage

1. Clone this repository:

    ```bash
    git clone https://github.com/yourusername/japan-studyarea.git
    cd japan-studyarea
    ```

2. Install required R packages:

    ```R
    install.packages(c("targets", "tarchetypes", "mregions2", "sf", "dplyr", "ggplot2", "terra", "rmarkdown", "archive"))
    ```

3. Download coral reef data:

    *   Go to [https://data.unep-wcmc.org/datasets/1](https://data.unep-wcmc.org/datasets/1)
    *   Manually download "WCMC008_CoralReefs2021_v4_1.zip"
    *   Place the downloaded file in the  `data/`  directory.

4. Run the pipeline:

    ```R
    targets::tar_make()
    ```

## Outputs

-   `data/output.gpkg`: A GeoPackage file containing the study area polygon, which integrates the EEZs of Japan and Taiwan, the Ryukyu and Izu Islands, the Ogasawara Islands, and coral reefs.
-   `output/study_area.png`: A map visualizing the study area.
-   `report.html`  (if  `tar_render(report, "report.Rmd")`  is included in  `_targets.R`): An HTML report summarizing the study area definition.

## Notes

-   The  `mregions2`  package is used to access the Marine Regions Gazetteer.
-   The bounding box is defined as longitude 115°E to 155°E and latitude 0°N to 45°N.
-   The code checks if the Ogasawara Islands and Izu Islands are within the bounding box.
-   Coral reef data is sourced from UNEP-WCMC (using the polygon shapefile).