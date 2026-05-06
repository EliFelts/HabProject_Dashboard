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

zip_path <- "main_fgdb/Initial_Project_Entry.zip"

# folder where you want it extracted

out_dir <- "main_fgdb"

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
      habitat_types, public_benefits, monitoring_methods,
      ecological_outcomes
    ),
    .fns = ~ str_replace_all(.x, "_", " ")
  )) %>%
  select(project_name,
    globalid = globalid.x, idfg_trackingnumber,
    managing_org, partner_agency, guidance_docs,
    project_startdate, project_category, idfg_staff,
    land_ownership, work_window_start, work_window_end,
    project_description, primary_species, secondary_species,
    life_stage, habitat_types, stream_name, primary_point_lat,
    primary_point_long, monitoring_methods, public_message,
    public_benefits,
    public_visibility, public_suitability, internal_notes, huc8,
    huc6, idfg_region = region_name, fmp_drainage, county = NAME,
    ecological_outcomes, wildlife_species
  )

# run the function to join in flowlines base on
# stream name and coordinates

stream_join <- projects.spatialjoin1 %>%
  split(seq_len(nrow(.))) %>%
  map_dfr(match_project_stream, flowlines = idfg_streams.sf) %>%
  mutate(string_dist = coalesce(string_dist, 0)) %>%
  group_by(project_id) %>%
  slice(which.min(string_dist))

staff_current <- read_excel("data-raw/position_list_current.xlsx")

projects.df <- projects.spatialjoin1 %>%
  left_join(stream_join, by = c("globalid" = "project_id", "huc8")) %>%
  left_join(staff_current, by = c("idfg_staff" = "old_survey")) %>%
  select(-c(idfg_staff, position)) %>%
  rename(idfg_staff = current) %>%
  mutate(managing_org = str_replace_all(managing_org, "_", " ")) #|>
# mutate(
#   floodplain_reconnect = NA_real_,
#   award_amount = NA_real_,
#   funding_source = NA_character_,
#   barriers_removed = NA_real_,
#   stream_miles_reconnected = NA_real_,
#   stream_miles_restored = NA_real_,
#   enhancement_structures = NA_real_,
#   bda_count = NA_real_,
#   riparian_area = NA_real_,
#   streambank_linearfeet = NA_real_,
#   flow_restored_miles = NA_real_,
#   wetland_area = NA_real_,
#   project_state = "Pre-Implementation",
#   project_status = "Active"
# ) |>
# select(globalid, project_name, idfg_trackingnumber, managing_org, partner_agency,
#   guidance_docs, project_startdate, project_category, land_ownership,
#   work_window_start, work_window_end, project_description, primary_species,
#   secondary_species, life_stage, habitat_types, wildlife_species,
#   ecological_outcomes, stream_name, primary_point_lat,
#   primary_point_long, monitoring_methods, public_message,
#   public_benefits,
#   public_visibility, public_suitability, award_amount,
#   funding_source, idfg_staff_name,
#   llid = LLID, idfg_region,
#   fmp_drainage, huc6, huc8, county, barriers_removed,
#   stream_miles_reconnected, stream_miles_restored,
#   floodplain_reconnect, enhancement_structures,
#   bda_count, riparian_area, streambank_linearfeet,
#   flow_restored_miles, wetland_area,
#   project_state, project_status
# )


# need dummy project with isolate project type
# to be able to get individual selections
# within the EB platform; this is only needed
# if trying to use in EB, so first export the
# existing version for use in Shiny app

dummy_projects.sf <- tibble(project_category = c(
  "Beaver Dam Analogues", "Channel Restoration",
  "Erosion Control and Sediment Management",
  "Fish-Friendly Water Intake Screens",
  "Floodplain Reconnection",
  "Flow and Irrigation Management",
  "Riparian Vegetation Restoration",
  "Side Channel Reconnection",
  "Streambank Stabilization",
  "Water Temperature Control",
  "Watershed Assessment",
  "Wetland Restoration"
)) %>%
  mutate(
    pt_type = "dummy",
    latitude = 46.64244,
    longitude = -116.10712,
    project_status = "Complete"
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = st_crs(projects.df),
    remove = F
  )


projects.df <- projects.df %>%
  mutate(
    pt_type = "real",
    project_status = "In Progress"
  ) %>%
  bind_rows(dummy_projects.sf)


# now bring in data from habitat project updates survey

# path to downloaded zip

update_zip_path <- "update_fgdb/Annual_Update.zip"

# folder where you want it extracted

update_out_dir <- "update_fgdb"

unzip(update_zip_path, exdir = update_out_dir)

# find the .gdb folder

update_gdb_path <- list.dirs(update_out_dir, recursive = TRUE, full.names = TRUE) %>%
  stringr::str_subset("\\.gdb$")

update_gdb_path

# grab the main point layer

update_main.df <- sf::st_read(update_gdb_path, layer = "Form_1") |>
  mutate(project_name = str_replace_all(project_id, "_", " ")) |>
  mutate(project_name = case_when(
    project_name == "Bi Po'i Naokwaide Creek Aquatic Organism Passage" ~ "Bia Po'i Naokwaide Creek Aquatic Organism Passage",
    TRUE ~ project_name
  ))

# funding table

funding.df <- sf::st_read(update_gdb_path, layer = "funding_action_group") |>
  inner_join(update_main.df, by = c("parentrowid" = "uniquerowid")) |>
  mutate(action_type = "funding") |>
  select(
    uniquerowid,
    project_name, reporting_year, action_type, funding_source, funding_details,
    funding_date, funding_amount
  )

# funding match

match.df <- st_read(update_gdb_path, layer = "match_group") |>
  inner_join(funding.df, by = c("parentrowid" = "uniquerowid")) |>
  mutate(action_type = "match") |>
  select(
    project_name, reporting_year, funding_source,
    match_source, match_amount
  )

# planning

planning.df <- st_read(update_gdb_path, layer = "planning_action_group") |>
  inner_join(update_main.df, by = c("parentrowid" = "uniquerowid")) |>
  mutate(action_type = "planning") |>
  select(project_name, reporting_year, action_type,
    planning_action = planning_action_annual,
    planning_date = planning_action_date
  )

# requirements

requirements.df <- st_read(update_gdb_path, layer = "requirement_action_group") |>
  inner_join(update_main.df, by = c("parentrowid" = "uniquerowid")) |>
  mutate(action_type = "requirements") |>
  select(project_name, reporting_year, action_type,
    requirement_action = requirement_action_annual,
    requirement_details, design_phase,
    requirement_status = requirement_status_annual,
    completion_date = requirement_application_date
  )

# implementations

implementations.df <- st_read(update_gdb_path, layer = "implementation_action_group") |>
  inner_join(update_main.df, by = c("parentrowid" = "uniquerowid")) |>
  mutate(
    action_type = "implementation",
    bda_logical = if_else(implementation_action_annual == "beaver_dam_analogue",
      T, F
    ),
    enhancement_structure_count = ifelse(bda_logical == T,
      1, enhancement_structure_count
    )
  ) |>
  select(project_name, reporting_year, action_type,
    implementation_type = implementation_action_annual,
    implementation_date = implementation_action_date,
    implementation_lat = implementation_action_lat,
    implementation_long = implementation_action_long,
    barrier_action_type:enhancement_structure_count
  )

delays.df <- st_read(update_gdb_path, layer = "delay_action_group") |>
  mutate(action_type = "delay") |>
  inner_join(update_main.df, by = c("parentrowid" = "uniquerowid")) |>
  select(
    project_name, reporting_year, action_type,
    delay_date, delay_status, delay_details
  )


# summarize funding by project

project_funding_summary <- funding.df |>
  mutate(funding_source = str_replace_all(funding_source, "_", " ")) |>
  group_by(project_name) |>
  summarize(
    award_amount = sum(funding_amount, na.rm = T),
    funding_source = paste(unique(funding_source), collapse = "; "),
    .groups = "drop"
  )

project_match_summary <- match.df |>
  mutate(funding_source = str_replace_all(funding_source, "_", " ")) |>
  group_by(project_name) |>
  summarize(
    match_amount = sum(match_amount, na.rm = T),
    match_source = paste(unique(funding_source), collapse = "; "),
    .groups = "drop"
  )

# summarize relevant metrics

project_metrics.summary <- implementations.df %>%
  mutate(across(floodplain_reconnect_acres:wetland_affected_metric, as.numeric)) %>%
  group_by(project_name) %>%
  summarize(
    barriers_removed = sum(implementation_type == "barrier_removal"),
    stream_miles_reconnected = sum(barrier_removal_miles, na.rm = T),
    stream_miles_restored = sum(stream_restoration_miles, na.rm = T),
    floodplain_reconnect = sum(floodplain_reconnect_acres, na.rm = T),
    enhancement_structures = sum(enhancement_structure_count, na.rm = T),
    bda_count = sum(implementation_type == "beaver_dam_analogue"),
    riparian_area = sum(riparian_affected_metric, na.rm = T),
    streambank_linearfeet = sum(streambank_affected_metric, na.rm = T),
    flow_restored_miles = sum(flowmanagement_metric, na.rm = T),
    wetland_acres = sum(wetland_affected_metric, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(across(barriers_removed:wetland_acres, ~ coalesce(.x, 0)))

# determine status based on this information


project_status.df <- funding.df %>%
  select(project_name, action_type, action_date = funding_date) %>%
  bind_rows(planning.df %>% select(project_name, action_type, action_date = planning_date)) %>%
  bind_rows(requirements.df %>% select(project_name, action_type, action_date = completion_date)) %>%
  bind_rows(implementations.df %>% select(project_name, action_type, action_date = implementation_date)) %>%
  bind_rows(delays.df %>% select(project_name, action_type, action_date = delay_date)) %>%
  group_by(project_name) %>%
  slice(which.max(action_date)) %>%
  mutate(project_state = case_when(
    action_type %in% c("planning", "requirements", "funding") ~ "Pre-Implementation",
    action_type %in% c("implementation", "post_monitoring") ~ "Implementation Complete",
    action_type %in% c("delay") ~ "Delayed"
  )) %>%
  ungroup() |>
  select(project_name, project_state)

# projects output for EB



projects.df_export <- projects.df |>
  left_join(project_status.df, by = "project_name") |>
  left_join(project_metrics.summary, by = "project_name") |>
  mutate(across(barriers_removed:wetland_acres, ~ coalesce(.x, 0))) |>
  left_join(project_funding_summary, by = "project_name") |>
  left_join(project_match_summary, by = "project_name")



st_write(projects.df_export,
  "data-raw/projects_agolupdate2.gpkg",
  delete_dsn = TRUE
)

# now work on getting photos out of the
# file geodatabase downloads

initial_attachments <- st_read(gdb_path,
  layer = "service_f2cf955572f04b0c9a5accac74a8022f_form_1__ATTACH",
  as_tibble = TRUE
)


# write all these photos out

pwalk(
  list(initial_attachments$data, initial_attachments$att_name),
  \(data, name) {
    writeBin(
      data,
      file.path("www/project_photos", name)
    )
  }
)
