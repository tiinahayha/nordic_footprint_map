library(tidyverse)
library(magrittr)
library(sf)    # Install from GitHub: library(devtools); devtools::install_github("rstats-db/DBI"); devtools::install_github("r-spatial/sf")
library(maps)
library(maptools)
library(rgeos)
library(ggthemes)

# Read country conconrdance table
country_list <- readr::read_csv("./input/country_concordance.csv") 

# Read footprint data 
data <- readr::read_csv("./input/data.csv") %>% 
  dplyr::filter(!region %in% c("Finland", "Denmark", "Sweden", "Norway"))

# Get polygon world map
world_map = sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

# Join world_map with region names and footprint data 
footprint_map <- world_map %>% 
  dplyr::left_join(country_list, by = c("ID" = "ID")) %>% 
  dplyr::left_join(data, by = c("Region" = "region")) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::filter(!is.na(Region)) %>% 
  dplyr::summarise(water_nordic = mean(water_nordic, na.rm = TRUE),
                   land_nordic = mean(land_nordic, na.rm = TRUE))

# Calculate regions centroids 
footprint_map_centroid <- footprint_map %>% 
  sf::st_centroid(of_largest_polygon = TRUE) %>% 
  dplyr::filter(Region != "Nordic")

# define colors
colors <- list()
colors[[1]] <- c("#bae4bc", "#56c5b8", "#0096c8", "#0868ac", "#00507d", "#000a32")
colors[[2]] <- c("#e4deba", "#c5a456", "#c86500", "#ac3a08", "#7d2600", "#320a00")
colors[[3]] <- c("#e4bade", "#c456c5", "#c800b6", "#ac08a5", "#6f007d", "#270032")
colors[[4]] <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
colors[[5]] <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b")

gp_map <- ggplot2::ggplot(footprint_map) +
  ggplot2::geom_sf(aes(fill = water_nordic)) +
  ggthemes::theme_map() +
  ggplot2::scale_fill_gradientn(colors = colors[[5]]) + 
  ggplot2::geom_sf(data = footprint_map_centroid, aes(size = land_nordic)) +
  ggplot2::theme(legend.position = "none", panel.grid.major = element_line(color = "white")) 
  
gp_map
 
ggplot2::ggsave(paste0("global_footprint_map.pdf"), plot = gp_map, path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 600)

ggplot2::ggsave(paste0("global_footprint_map.eps"), plot = gp_map, path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 600)

ggplot2::ggsave(paste0("global_footprint_map.png"), plot = gp_map, path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 600)

# Get zoom to EU 
eu_bbox <- world_map %>% 
  dplyr::filter(ID %in% c("Finland", "Spain", "Ukraine", "Ireland")) %>% 
  sf::st_bbox()

eu_bbox[1] <- eu_bbox[1] + 0
eu_bbox[2] <- eu_bbox[2] + -3
eu_bbox[3] <- eu_bbox[3] + 2
eu_bbox[4] <- eu_bbox[4] + 0

gp_map_eu <- footprint_map %>% 
  sf::st_crop(eu_bbox) %>% 
  dplyr::select(water_nordic) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = water_nordic)) +
  ggthemes::theme_map() +
  ggplot2::scale_fill_gradientn(colors = colors[[5]]) + 
  ggplot2::geom_sf(data = sf::st_crop(footprint_map_centroid, eu_bbox), aes(size = land_nordic)) +
  ggplot2::theme(legend.position = "none", panel.grid.major = element_line(color = "white")) 

gp_map_eu

ggplot2::ggsave(paste0("global_footprint_map_eu.pdf"), plot = gp_map_eu, path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 600)

ggplot2::ggsave(paste0("global_footprint_map_eu.eps"), plot = gp_map_eu, path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 600)

ggplot2::ggsave(paste0("global_footprint_map_eu.png"), plot = gp_map_eu, path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 600)




