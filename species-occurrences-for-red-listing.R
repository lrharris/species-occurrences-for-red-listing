###############################################################################
# SUPPLEMENTARY MATERIAL TO: Harris et al. Sandy beach ecosystem and species  #
# red listing highlight priorities for beach conservation and restoration.    #
# doi: .... <to be added when published>                                      #
# Please cite this paper if you use the code below                            #
###############################################################################

library(rgbif)
library(rinat)
library(naniar)
library(here)
library(data.table)
library(sf)
library(redlistr)
library(tidyverse)

# Get started ------------------------------------------------------------------ 
# This script assumes that the field sampling data are stored in a folder called
# data, and all the outputs get written to a folder called outputs. All the data 
# downloaded from GBIF also get stored in the data folder. If you implement the
# spatial filter in R, then the shapefile of the shore should also be stored in 
# the data folder.

# Review species data in iNat --------------------------------------------------
sp <- "..." #insert species name

inat_data <- get_inat_obs(taxon_name = sp, maxresults=10000)        #all
inat_data_a <- get_inat_obs(taxon_name = sp, annotation = c(17,18), #alive
                            maxresults=10000)
inat_data_d <- get_inat_obs(taxon_name = sp, annotation = c(17,19), #dead
                            maxresults=10000)
inat_data1 <- inat_data %>% filter(!(id %in% inat_data_a$id))   #filter alive
inat_data2 <- inat_data1 %>% filter(!(id %in% inat_data_d$id))  #filter dead

# open the url of any records that are remaining to make sure all have been
# verified and properly coded as alive or dead. When this is complete for all
# species, continue with the rest of the script.
# ------------------------------------------------------------------------------

## Set up GBIF login credentials ----
user = "..." # your username
pwd = "..."  # your password
email = "..." # your email address

## Read in shapefiles ----
shore <- st_read(here(
  "data/yourfile.shp")) %>%
  st_cast(., "MULTIPOLYGON")


## Set up the species list and find the best species name match in GBIF ----
species <- c("Acanthoscelis ruficornis",
             "Africorchestia quadrispinosa",
             "Bullia digitalis",
             "Bullia mozambicensis",
             "Bullia natalensis",
             "Bullia rhodostoma",
             "Capeorchestia capensis",
             "Donax serra",
             "Emerita austroafricana",
             "Excirolana latipes",
             "Excirolana natalensis",
             "Griffithsius latipes",
             "Latona madagascariensis",
             "Latona sordida",
             "Ocypode madagascariensis",
             "Ocypode ryderi",
             "Pachyphaleria capensis",
             "Tylos capensis",
             "Tylos granulatus")

species_gbif_list <- list()

for (i in 1:length(species)){
  sp_name = name_suggest(q = species[i], rank = "species")
  species_gbif_list[[i]] <- sp_name$data
}
species_gbif_list

## Set up metadata table for gbif metadata, EOO and AOO ----
meta_table <- tibble(download_key = character(), 
                     species = character(),
                     doi = character(),
                     citation = character(),
                     eoo_km2 = numeric(),
                     aoo_km2 = numeric())

# And off we go ----------------------------------------------------------------

for (i in 1:length(species_gbif_list)){
  species_name <- species_gbif_list[[i]]$canonicalName[1]
  
  
# Download GBIF data -----------------------------------------------------------
  ## Set up the download ----
  sp_download = occ_download(
    pred_in("taxonKey", species_gbif_list[[i]]$key[1]),
    pred("hasCoordinate", TRUE), # Select only records with coordinates
    format = "SIMPLE_CSV",
    user = user, pwd = pwd, email = email)
  
  ## Get the download key and request the data ----
  sp_download
  d_key=sp_download[[1]]
  
  ## Wait for the dataset to be compiled ----
  still_running <- TRUE
  status_ping <- 3
  while (still_running) {
    meta <- occ_download_meta(key = d_key)
    status <- meta$status
    still_running <- status %in% c("PREPARING", "RUNNING")
    Sys.sleep(status_ping) # sleep between pings
  }
  
  ## Save the download metadata
  meta_table[i,1] <- d_key
  meta_table[i,2] <- species_name
  meta_table[i,3] <- occ_download_meta(key = d_key)$doi
  meta_table[i,4] <- paste0("GBIF.org (", Sys.Date(), 
                            ") GBIF Occurrence Download https://doi.org/", 
                            occ_download_meta(key = d_key)$doi)
  
  ## Download the dataset ----
  sp_download = occ_download_get(key = d_key, 
                                 path = here("data"), 
                                 overwrite = TRUE) %>%
    occ_download_import(sp_download, na.strings = c("", NA))
  unzip(zipfile=here("data", paste0(d_key, ".zip")), exdir="data")
  
  ## Read the data into R and remove data from iNaturalist ----
  gbif_data = fread(here("data", paste0(d_key, ".csv")), 
                    data.table = FALSE, fill = F, encoding ="UTF-8", quote="") %>% 
    mutate(latitude = decimalLatitude, longitude=decimalLongitude,
           source=paste0("GBIF.org 2024-11-22; https://gbif.org/occurrence/", gbifID),
           quality_grade="research") %>% #GBIF citation date = date of download
    dplyr::rename(recordedby=recordedBy, basisofrec=basisOfRecord) %>% 
    filter(!(institutionCode=="iNaturalist")) %>% 
    dplyr::select(basisofrec, catalogNumber, latitude, longitude, year, 
                  scientificName, source, recordedby, quality_grade) 
  
  
# Download iNaturalist data ----------------------------------------------------
  try(
    inat_data <- get_inat_obs(taxon_name = species_name,
                              annotation = c(17,18),  #see note below
                              #quality = "research",  #see note below
                              maxresults=10000)  %>% 
      replace_with_na(., replace = list(user_name="")) %>% 
      mutate(recordedby=coalesce(user_name, user_login),
             observed_on=as.POSIXlt(observed_on, format="%Y-%m-%d"),
             observed_on=format(observed_on, format="%Y")) %>% 
      dplyr::rename(scientificName=scientific_name, year=observed_on,
                    catalogNumber=id) %>%
      mutate(basisofrec="HUMAN_OBSERVATION",
             source=paste0("iNaturalist community 2024; ", url)) %>% 
      dplyr::select(basisofrec,catalogNumber, latitude, longitude, year, 
                    scientificName, source, recordedby, institutionCode,
                    quality_grade) %>% 
      filter(!(is.na(latitude))))
  
  if(exists("inat_data")=="FALSE") {
    inat_data<-as_tibble(matrix(nrow = 0, ncol = length(names(gbif_data))), 
                         .name_repair = ~ names(gbif_data))
  }
  
  #note: To download all research grade data instead of downloading records of alive individuals e.g., for O ceratophthalmus, exclude the line:  annotation = c(17,18),  and uncomment the line below it:  quality = "research",
  
# Read in field sampling data --------------------------------------------------
# this assumes your data are saved in a data folder with a name like: 
# "Acanthoscelis ruficornis_distribution-records.csv"
# and has the following columns (with example values):
  # Collector (e.g., Harris 2012)
  # Identification (e.g., Acanthoscelis ruficornis)
  # Origin (e.g., Site Name)
  # Latitude (in decimal degrees)
  # Longitude (in decimal degrees)
  # year (year of observation)
  
  other_data <- read_csv(here("data", 
                              paste0(species_name, "_distribution-records.csv")),
                         show_col_types = FALSE) %>% 
    rename_with(tolower) %>% 
    mutate(basisofrec="HUMAN_OBSERVATION",
           catalogNumber="NA", 
           source=collector,
           quality_grade = "research") %>%
    dplyr::rename(scientificName=identification, recordedby=collector) %>% 
    select(-origin)
  
  
# Combine datasets -------------------------------------------------------------
# Edit values as you need to, see the following for mapping standards and names 
# of the field and their meaning:
# https://www.iucnredlist.org/resources/mappingstandards
  all_data <- rbind(gbif_data, inat_data, other_data) %>% 
    mutate(spatialref="WGS84",
           yrcompiled=2024,
           presence=1,
           compiler="Linda R. Harris",
           citation="Linda R. Harris", #citation for the dataset
           seasonal=1,
           data_sens=0,
           origin=1) %>% 
    mutate(quality_grade=factor(quality_grade)) %>%
    rename(sci_name=scientificName, catalog_no=catalogNumber, event_year=year) %>% 
    select(sci_name, presence, origin, seasonal, compiler, yrcompiled, citation,
           latitude, longitude, spatialref, data_sens, event_year, source, 
           basisofrec, catalog_no, recordedby, quality_grade)

  # to have a record of the data before the spatial filtering, save a copy now  
write_csv(all_data, here("outputs/unfiltered_data", 
                           paste0(species_name, "_unfiltered.csv")))
  
  
# Filter data based on presence in the shore polygon ---------------------------
#note: skip this section for O ceratophthalmus - the distribution is larger than the shore polygon created for the WIO. This step was performed in ArcGIS 10.6 because it was substantially faster than in R.
  spdf <- st_as_sf(x = all_data,
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +datum=WGS84") %>% 
    st_transform(., st_crs(shore))
  
  sp_dat_filter <-  spdf %>%
    mutate(include = as.numeric(st_intersects(spdf, shore))) %>%
    filter(include>0) %>%
    mutate(id=seq(1:nrow(.)))
  
  
# Write output -----------------------------------------------------------------
  st_write(sp_dat_filter, paste0(species_name, "_temp.csv"), 
           layer_options = "GEOMETRY=AS_XY")
  dat <- read_csv(paste0(species_name, "_temp.csv"),show_col_types = FALSE) %>% 
    dplyr::rename(latitude=Y, longitude=X) %>% 
    select(-include) #-quality_grade, -id
  write_csv(dat, here("outputs/final_data", paste0(species_name, "_distribution.csv")))
  file.remove(paste0(species_name, "_temp.csv"))
  
  
# Calculate EOO and AOO (just an initial estimate, actual calculations done in geocat) ---
  sp_projected <- sp_dat_filter %>% 
    st_transform(., 9822) %>% 
    as(., "Spatial")
  
  EOO <- makeEOO(sp_projected)
  AOO <- makeAOOGrid(sp_projected, grid.size = 2000)
  
  meta_table[i,5] <- getAreaEOO(EOO)
  meta_table[i,6] <- length(AOO)*4
  
  rm(gbif_data, inat_data, other_data)
}
# Save the metadata file -------------------------------------------------------
write_csv(meta_table, 
          here(paste0("outputs/metadata_table_", Sys.Date(), ".csv")))

################################################################################

# O ceratophthalmus - as above, but filter iNaturalist data by observations that
# are research grade, and don't perform the spatial filter

species <- "Ocypode ceratophthalmus"

species_gbif_list <- list()

for (i in 1:length(species)){
  sp_name = name_suggest(q = species[i], rank = "species")
  species_gbif_list[[i]] <- sp_name$data
}
species_gbif_list

## Set up metadata table for gbif metadata, EOO and AOO ----
meta_table <- tibble(download_key = character(), 
                     species = character(),
                     doi = character(),
                     citation = character(),
                     eoo_km2 = numeric(),
                     aoo_km2 = numeric())

# And off we go ----------------------------------------------------------------
for (i in 1:length(species_gbif_list)){
  species_name <- species_gbif_list[[i]]$canonicalName[1]
  
  
  # Download GBIF data -----------------------------------------------------------
  ## Set up the download ----
  sp_download = occ_download(
    pred_in("taxonKey", species_gbif_list[[i]]$key[1]),
    pred("hasCoordinate", TRUE), # Select only records with coordinates
    format = "SIMPLE_CSV",
    user = user, pwd = pwd, email = email)
  
  ## Get the download key and request the data ----
  sp_download
  d_key=sp_download[[1]]
  #occ_download_meta(key = d_key)
  meta_table[i,1] <- d_key
  meta_table[i,2] <- species_name
  meta_table[i,3] <- occ_download_meta(key = d_key)$doi
  meta_table[i,4] <- paste0("GBIF.org (", Sys.Date(), 
                            ") GBIF Occurrence Download https://doi.org/", 
                            occ_download_meta(key = d_key)$doi)
  
  ## Wait for the dataset to be compiled ----
  still_running <- TRUE
  status_ping <- 3
  while (still_running) {
    meta <- occ_download_meta(key = d_key)
    status <- meta$status
    still_running <- status %in% c("PREPARING", "RUNNING")
    Sys.sleep(status_ping) # sleep between pings
  }
  
  ## Download the dataset ----
  sp_download = occ_download_get(key = d_key, 
                                 path = here("data"), 
                                 overwrite = TRUE) %>%
    occ_download_import(sp_download, na.strings = c("", NA))
  unzip(here("data", paste0(d_key, ".zip")), exdir="data")
  
  ## Read the data into R ----
  gbif_data = fread(here("data", paste0(d_key, ".csv")), 
                    data.table = FALSE, fill = F, encoding ="UTF-8", quote="") %>% 
    mutate(latitude = decimalLatitude, longitude=decimalLongitude,
           source=paste0("https://gbif.org/occurrence/", gbifID),
           quality_grade="research") %>%
    dplyr::rename(recordedby=recordedBy, basisofrec=basisOfRecord) %>% 
    dplyr::select(basisofrec, catalogNumber, latitude, longitude, year, 
                  scientificName, source, recordedby, institutionCode, 
                  quality_grade) %>% 
    filter(!(institutionCode=="iNaturalist"))
  
  
  # Download iNaturalist data ----------------------------------------------------
  try(
    inat_data <- get_inat_obs(taxon_name = species_name, 
                              #annotation = c(17,18), 
                              quality = "research",
                              maxresults=10000)  %>% 
      replace_with_na(., replace = list(user_name="")) %>% 
      mutate(recordedby=coalesce(user_name, user_login),
             observed_on=as.POSIXlt(observed_on, format="%Y-%m-%d"),
             observed_on=format(observed_on, format="%Y")) %>% 
      dplyr::rename(scientificName=scientific_name, year=observed_on,
                    catalogNumber=id) %>%
      mutate(basisofrec="HUMAN_OBSERVATION",
             institutionCode="iNaturalist", source=url) %>% 
      dplyr::select(basisofrec,catalogNumber, latitude, longitude, year, 
                    scientificName, source, recordedby, institutionCode,
                    quality_grade) %>% 
      filter(!(is.na(latitude))))
  
  if(exists("inat_data")=="FALSE") {
    inat_data<-as_tibble(matrix(nrow = 0, ncol = length(names(gbif_data))), 
                         .name_repair = ~ names(gbif_data))
  }
  
  
  # Read in field sampling data --------------------------------------------------
  other_data <- read_csv(here("data", 
                              paste0(species_name, "_distribution-records.csv")),
                         show_col_types = FALSE) %>% 
    rename_with(tolower) %>% 
    mutate(basisofrec="HUMAN_OBSERVATION",
           catalogNumber="NA", 
           source=collector,
           institutionCode=collector,
           quality_grade = "research") %>%
    dplyr::rename(scientificName=identification, recordedby=collector) %>% 
    select(-origin)
  
  
  # Combine datasets -------------------------------------------------------------
  all_data <- rbind(gbif_data, inat_data, other_data) %>% 
    mutate(spatialref="WGS84",
           yrcompiled=2024,
           presence=1,
           compiler="Linda R. Harris",
           seasonal=1,
           data_sens=0,
           origin=1) %>% 
    mutate(quality_grade=factor(quality_grade)) %>%
    rename(sci_name=scientificName, citation=institutionCode, 
           catalog_no=catalogNumber, event_year=year) %>% 
    select(sci_name, presence, origin, seasonal, compiler, yrcompiled, citation,
           latitude, longitude, spatialref, data_sens, event_year, source, 
           basisofrec, catalog_no, recordedby, quality_grade)
  
  write_excel_csv(all_data, here("outputs/unfiltered_data", 
                                 paste0(species_name, "_unfiltered.csv")))
  
  
    # Calculate EOO and AOO
  sp_projected <- all_data %>% 
    st_transform(., 9822) %>% 
    as(., "Spatial")
  
  EOO <- makeEOO(sp_projected)
  AOO <- makeAOOGrid(sp_projected, grid.size = 2000)
  
  meta_table[i,5] <- getAreaEOO(EOO)
  meta_table[i,6] <- length(AOO)*4
  
  rm(gbif_data, inat_data, other_data)
}

# Save the metadata file -------------------------------------------------------
write_csv(meta_table, 
          here(paste0("outputs/metadata_table_", Sys.Date(), ".csv")))


# Combine files for IUCN submission --------------------------------------------    
files <- list.files(path = "./outputs/for IUCNRL", pattern="[a-z].csv") 
rm(iucn, iucn_dat)
iucn<-tibble()
for (i in files){  
  iucn_dat <- read_csv(paste("./outputs/for IUCNRL/", i, sep=""))
  iucn <- rbind(iucn, iucn_dat) 
}
write_excel_csv(iucn, "./outputs/for IUCNRL/compiled_beach_invertebrate_distributions.csv")

### ------------------------------ END SCRIPT ------------------------------ ###