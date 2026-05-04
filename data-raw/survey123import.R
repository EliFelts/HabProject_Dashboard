############################
# Initial setup steps that #
# apply to everything else #
# in this process          #
############################

# load libraries

library(tidyverse)
library(httr)
library(jsonlite)
library(reticulate)
library(readxl)
library(sf)
library(stringdist)

# conda_create("agol", packages = "python=3.10")
#
# conda_install("agol", packages = "arcgis", pip = TRUE)


# use_condaenv("agol", required = TRUE)
#
# py_config()
#
# # import Python modules
#
# gis <- import("arcgis.gis")
#
# pandas <- import("pandas")
#
# gis_conn <- gis$GIS(
#   Sys.getenv("AGOL_URL"),
#   Sys.getenv("AGOL_USER"),
#   Sys.getenv("AGOL_PASS")
# )



# path to downloaded zip

zip_path <- "data-raw/Initial_Project_Entry.zip"

# folder where you want it extracted

out_dir <- "data-raw"

unzip(zip_path, exdir = out_dir)

# find the .gdb folder

gdb_path <- list.dirs(out_dir, recursive = TRUE, full.names = TRUE) %>%
  stringr::str_subset("\\.gdb$")

gdb_path

# grab the main point layer

projects_raw <- sf::st_read(gdb_path, layer = "Form_1") |>
  dplyr::select(-c(award_amount:project_status))

# read in spatial reference layers that get used
# to join in additional spatial data based on
# the project waypoint

idaho_huc8.sf <- read_rds("data-raw/huc8")

idaho_counties.sf <- read_rds("data-raw/idaho_counties") %>%
  st_transform(crs = st_crs(idaho_huc8.sf))

idaho_regions.sf <- st_read("data-raw/idfg_regions.gpkg") %>%
  st_transform(crs = st_crs(idaho_huc8.sf))

idaho_fmp.sf <- read_rds("data-raw/fmp_drainages.rds") %>%
  st_transform(crs = st_crs(idaho_huc8.sf))

idfg_streams.sf <- st_read("data-raw/Hydrography_Public_3451083698128016512.geojson") %>%
  st_transform(crs = st_crs(idaho_huc8.sf)) %>%
  st_zm() %>%
  st_join(idaho_huc8.sf)

# load helper function

source("R/match_project_stream.R")

# next step is to join in some spatial data

projects.spatialjoin1 <- projects_raw %>%
  # st_as_sf(
  #   coords=c("primary_point_long",
  #            "primary_point_lat"),
  #   crs=st_crs(idaho_huc8.sf),
  #   remove=F)%>%
  st_join(idaho_huc8.sf) %>%
  st_join(idaho_counties.sf) %>%
  st_join(idaho_regions.sf) %>%
  st_join(idaho_fmp.sf) %>%
  mutate(
    guidance_docs = str_replace_all(guidance_docs, "_", " "),
    stream_name = trimws(stream_name)
  ) %>%
  mutate(across(
    .cols = c(
      guidance_docs, partner_agency, project_category,
      idfg_staff, primary_species, secondary_species, life_stage,
      habitat_types, public_benefits, monitoring_methods
    ),
    .fns = ~ str_replace_all(.x, "_", " ")
  )) %>%
  select(project_name,
    globalid = globalid.x, idfg_trackingnumber,
    managing_org, partner_agency, guidance_docs,
    project_startdate, project_category, idfg_staff,
    land_ownership, work_window_start, work_window_end,
    project_description, primary_species, secondary_species,
    life_stage, habitat_types, stream_name, latitude = primary_point_lat,
    longitude = primary_point_long, monitoring_methods, public_benefits,
    public_visibility, public_suitability, internal_notes, huc8,
    huc6, idfg_region = region_name, fmp_drainage, county = NAME
  )

# run the function to join in flowlines base on
# stream name and coordinates

stream_join <- projects.spatialjoin1 %>%
  split(seq_len(nrow(.))) %>%
  map_dfr(match_project_stream, flowlines = idfg_streams.sf) %>%
  mutate(string_dist = coalesce(string_dist, 0)) %>%
  group_by(project_id) %>%
  slice(which.min(string_dist))
