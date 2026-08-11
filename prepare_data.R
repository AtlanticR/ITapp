if(!require("spocc")) install.packages("spocc")
if(!require("robis")) install.packages("robis")
if(!require("devtools")) install.packages("devtools")
if(!require("esri2sf")) devtools::install_github("yonghah/esri2sf")
if(!require("arcpullr")) devtools::install_github("pfrater/arcpullr")
if(!require("arcgislayers")) install.packages("arcgislayers")
if(!require("sf")) install.packages("sf")
if(!require("raster")) install.packages("raster")
if(!require("fasterize")) install.packages("fasterize")
if(!require("gdistance")) install.packages("gdistance")
if(!require("data.table")) install.packages("data.table")
if(!require("tidyverse")) install.packages("tidyverse")

print("Warning: Initial installation will take multiple hours!")

#### global variables ####

print("Loading data")
proj <- "+proj=longlat +datum=WGS84"
# equidist1 <- "+proj=eqdc +lon_0=-58.50 +lat_0=48.00 +lat_1=44.00 +lat_2=52.00 +x_0=1000000 +y_0=1000000 +datum=WGS84 +units=m +no_defs"
# equidist2<- "+proj=eqdc +lon_0=-66.00 +lat_0=53.00 +lat_1=47.00 +lat_2=59.00 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs"
equidist <- paste(
  "+proj=eqdc",
  "+lat_1=45",
  "+lat_2=58",
  "+lat_0=51.5",
  "+lon_0=-66",
  "+x_0=2000000",
  "+y_0=2000000",
  "+datum=WGS84",
  "+units=m",
  "+no_defs"
)
sf_use_s2(FALSE)

# NS<-st_read("spatialdata/NS_Aqua_lease_2026/geo_export_95bfaf74-c1b7-4c65-a258-d1cb7076637e.shp")%>%
#   mutate(Lease_Identifier=license_le)
#saveRDS(NS, "spatialdata/NS.rds")

# NL<- st_read("spatialdata/NL_lease/Fisheries_and_Aquaculture_-_Licensed_Fish_Processors_and_Aquaculture_Sites.shp")%>%
#   mutate(Lease_Identifier=LICENSES)%>%
#   st_transform(proj)
# 
# 
# NL$Lease_Identifier<-make.unique(as.character(NL$Lease_Identifier)) #NL leases are not uniquely coded. Therefore, I needed ti force uniqueness. 
# 
# saveRDS(NL, "spatialdata/NL.rds")

# NB_shell<-st_read("spatialdata/NB_lease/Shellfish.geojson")%>%
#   mutate(Lease_Identifier=SITE_NUMBER)%>%
#   select(-"CULTIVATION_METHOD")
# NB_finfish<-st_read("spatialdata/NB_lease/Finfish.geojson")%>%
#   mutate(Lease_Identifier=SITE_NUMBER)
# NB<-rbind(NB_shell, NB_finfish)
# saveRDS(NB, "spatialdata/NB.rds")

# QC<-st_read("spatialdata/QCAquaculture/QCAquaculture20260512.shp")%>%
#   mutate(Lease_Identifier=DISPLAY_FR)%>%
#   select(Lease_Identifier, geometry)%>%
#   st_transform(proj)
# saveRDS(QC, "spatialdata/QC.rds")

NS <- readRDS("spatialdata/NS.rds")
NB<- readRDS("spatialdata/NB.rds")
PEI <- readRDS("spatialdata/PEI.rds")
NL<- readRDS ("spatialdata/NL.rds")
QC<-readRDS("spatialdata/QC.rds")

species <- read.csv("commonnames.csv")

# Load and clean up incidental data ---------------------------------------

searcharea <- c(NS$geometry,NB$geometry,PEI$geometry, NL$geometry, QC$geometry) %>% 
  st_combine() %>% 
  st_convex_hull() %>% 
  st_sfc()%>%
  st_transform(equidist) %>% 
  st_buffer(100000) %>% 
  st_transform(proj)

####Read in and amalgamate NL data
NL_csv<-c("recentdata/NL AIS Open Data Golden star Tunicate 2006 to 2025.csv",
          "recentdata/NL AIS Open Data Vase Tunicate 2006 to 2025.csv",
          "recentdata/NL AIS Open Data Violet Tunicate 2006 to 2025.csv")

# 2. Read all files into a named list (retaining file names for tracking)
NL_tunicates_list <- map(set_names(NL_csv), \(file) {
  read_csv(file, col_types = cols(.default = "c")) # Force columns to text to prevent merge crashes
})

# 3. Combine the list into one master dataframe
tunicates_df <- list_rbind(NL_tunicates_list, names_to = "source_file") %>%
  # Fix column classes that shouldn't be characters
  mutate(
    decimalLongitude = as.numeric(decimalLongitude),
    decimalLatitude = as.numeric(decimalLatitude),
    coordinateUncertaintyInMeters = as.integer(coordinateUncertaintyInMeters)
  )

# 4. (Optional) Convert the combined dataframe into a spatial 'sf' object
tunicates_sf <- tunicates_df %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>% # Drop missing coordinates
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)%>%
  filter(validated == "yes")%>%
  mutate(Species=case_when(vernacularNameEN == "golden star tunicate" ~"Botryllus_schlosseri",
                           vernacularNameEN =="vase tunicate"~"Ciona_intestinalis",
                           vernacularNameEN =="violet tunicate"~"Botrylloides_violaceus"),
         Presence=case_when(occurrenceStatus=="detected"~1,
                            occurrenceStatus=="not detected"~0),
         Year = as.integer(str_extract(eventDate2, "^\\d{4}")))%>%
  rename("StnLocation"="locality")%>%
  select (Year, Species, geometry, Presence, StnLocation, samplingProtocol)%>%
  filter(!is.na(Species)) %>% # Drop any unmapped species to prevent an 'NA' column
  
  # 4. Pivot from Long to Wide format
  pivot_wider(
    id_cols = c(StnLocation, Year, geometry, samplingProtocol),
    names_from = Species,
    values_from = Presence,
    values_fn = max,        # Prevents c(1,1) by taking the maximum presence value
    values_fill = 0         # Fills missing species records at a station with 0
  )

# 1. Extract the coordinates and reverse their matrix columns
flipped_coords <- st_coordinates(tunicates_sf)[, c("Y", "X")]

# 2. Convert the reversed coordinates back into clean POINT geometries
corrected_geometry <- st_sfc(
  lapply(1:nrow(flipped_coords), function(i) st_point(flipped_coords[i, ])),
  crs = 4326
)

# 3. Replace the broken inverted geometry column with the corrected one
st_geometry(tunicates_sf) <- corrected_geometry

#4. Remove StnLocation errors such as spaces 

tunicates_sf$StnLocation <- iconv(tunicates_sf$StnLocation, from = "latin1", to = "UTF-8")
tunicates_sf$StnLocation <- gsub("’", "'", tunicates_sf$StnLocation)

#Now split NL data into incidental and eDNA datasets

NL_incidental<-tunicates_sf%>%
  filter(samplingProtocol != "eDNA")%>%
  select(-samplingProtocol)

#Important note: eDNA alone did not detect any tunicates and therefore, there is no need to include this in the eDNA section below. 
NL_eDNA<-tunicates_sf%>%
  filter(samplingProtocol == "eDNA")%>%
  select(-samplingProtocol)

####incidental reports confirmed by DFO Science

incidental_occ <- occ(query=species$Scientific_Name,
                      from=c("gbif","inat"),
                      geometry = st_bbox(searcharea),
                      has_coords = TRUE,
                      limit=10000) %>%
  occ2df() %>%
  mutate(StnLocation=paste0("within 1.1km of lon ",round(as.numeric(longitude),2)," lat ",round(as.numeric(latitude),2))) %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326)%>%
  st_transform(crs=proj) %>%
  mutate(link=case_when(prov=="gbif" ~ paste0("https://www.gbif.org/occurrence/",key),
                        prov=="inat" ~ paste0("https://www.inaturalist.org/observations/",key)
  ))

incidental_occ<-incidental_occ%>%
  filter(!grepl("BOLD", name))%>% #remove any columns form the BOLD database
  mutate(Species=case_when(name %in% c("Argopecten irradians amplicostatus (Dall, 1898)", "Aequipecten irradians (Lamarck, 1819)","Argopecten irradians (Lamarck, 1819)", "Argopecten irradians", "Argopecten irradians irradians", "Aequipecten irradians sablensis A.H.Clarke, 1965" ,"Argopecten irradians sablensis (A.H.Clarke, 1965)") ~ "Argopecten_irradians",
                           name %in% c("Ascidiella aspersa (Müller, 1776)", "Ascidiella aspersa (M?ller, 1776)","Ascidiella aspersa" ) ~ "Ascidiella_aspersa",
                           name %in% c("BOLD:AAA7687","BOLD:ACL8382","Carcinus maenas (Linnaeus, 1758)", "Carcinus maenas") ~ "Carcinus_maenas",
                           name %in% c("Botrylloides violaceus Oka, 1927", "Botrylloides violaceus") ~ "Botrylloides_violaceus",
                           name %in% c("Botryllus schlosseri (Pallas, 1766)", "Botryllus schlosseri") ~ "Botryllus_schlosseri",
                           name %in% c("Caprella mutica Schurin, 1935","BOLD:AAE7686", "Caprella mutica") ~ "Caprella_mutica",
                           name %in% c("Carcinus maenas (Linnaeus, 1758)", "Carcinus maenas")~"Carcinus maenas",
                           name %in% c("Ciona intestinalis (Linnaeus, 1767)", "Ascidia intestinalis Linnaeus, 1767", "Ciona intestinalis tenella (Stimpson, 1852)","Ciona tenella (Stimpson, 1852)", "Ciona intestinalis" ) ~ "Ciona_intestinalis",
                           name %in% c("Codium fragile fragile", "Codium fragile (Suringar) Hariot","Codium fragile subsp. fragile","Codium fragile subsp. tomentosoides (Goor) P.C.Silva","Codium fragile tomentosoides", "Codium fragile", "Codium fragile (Suringar) Har." ,"Codium fragile var. fragile") ~ "Codium_fragile",
                           name %in% c("Diadumene lineata (Verrill, 1869)", "Diadumene lineata")~"Diadumene_lineata",
                           name %in% c("Didemnum vexillum Kott, 2002", "Didemnum vexillum") ~ "Didemnum_vexillum",
                           name %in% c("Leptoclinum gelatinosum Milne Edwards, 1841" , "Diplosoma listerianum (Milne Edwards, 1841)", "Diplosoma listerianum") ~ "Diplosoma_listerianum",
                           name %in% c("Fucus serratus L.", "Fucus serratus")~"Fucus_serratus",
                           name %in% c("Hemigrapsus sanguineus (De Haan, 1835)","Hemigrapsus sanguineus (de Haan, 1835)", "Hemigrapsus sanguineus") ~ "Hemigrapsus_sanguineus",
                           name %in% c("Juxtacribrilina mutabilis (Ito, Onishi & Dick, 2015)","Cribrilina mutabilis Ito, Onishi & Dick, 2015", "Juxtacribrilina mutabilis")~"Juxtacribrilina_mutabilis",
                           name %in% c("Membranipora membranacea (Linnaeus, 1767)", "Flustra membranacea Linnaeus, 1767", "Membranipora membranacea") ~ "Membranipora_membranacea",
                           name %in% c("Oncorhynchus mykiss (Walbaum, 1792)", "Salmo iridea Gibbons, 1855", "Salmo gairdnerii Richardson, 1836", "Oncorhynchus mykiss", "Oncorhynchus mykiss irideus (Gibbons, 1855)" ) ~ "Oncorhynchus_mykiss",
                           name %in% c("Ostrea edulis (Linnaeus, 1767)","Ostrea edulis Linnaeus, 1758","Ostrea edulis") ~ "Ostrea_edulis",
                           name %in% c("Sargassum muticum (Yendo) Fensholt", "Sargassum muticum")~"Sargassum_muticum",
                           name %in% c("Styela clava Herdman, 1881", "Styela clava") ~ "Styela_clava",
                           name %in% c("Tricellaria inopinata d'Hondt & Occhipinti Ambrogi, 1985", "Tricellaria inopinata")~"Tricellaria_inopinata",
                           TRUE ~ name),
         Year=as.numeric(substr(date,1,4)))

if(!all(sort(unique(incidental_occ$Species)) %in% sort(species$R_Name))){
  sp <- sort(unique(incidental_occ$Species))[!sort(unique(incidental_occ$Species)) %in% sort(species$R_Name)]
  warning(paste0(sp," is not found in a recognized species name, rename in `incidental_occ` which is in `prepare_data.R`"))
}


asian_shore_crab_2020 <- rbind(arcpullr::get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/226"),
                               arcpullr::get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/227") )%>%
  dplyr::rename(geometry=geoms, StnLocation=stn_location)%>% 
  st_transform(proj) %>% 
  dplyr::filter(cover_index>=1)%>%
  dplyr::select(-OBJECTID,-latitude,-longitude, -province, -stn_num, -cover_index)%>%
  rename("Species"="species_name",
         "Year"="year")%>%
  mutate(Presence="TRUE",
         prov="DFO Science Maritimes Region contact Claudio.DiBacco@dfo-mpo.gc.ca",
         Species=str_replace_all(Species, " ", "_"))
  
gulf_tunicate_incidental_2020 <- readxl::read_excel("recentdata/Gulf AIS data_biof_monit_incidental_AISNCP MAR_April 2021.xlsx",sheet=2,col_types =  "text") %>%
  mutate('Longitude (W)'=case_when(`Latitude (N)`=="*waiting for coordinate"~-61.91,   #fixing bad data entry
                                   `Longitude (W)`>0~as.numeric(`Longitude (W)`)*-1,
                                   TRUE~as.numeric(`Longitude (W)`)),
         'Latitude (N)'=case_when(`Latitude (N)`=="*waiting for coordinate"~"45.88",
                                  TRUE~`Latitude (N)`),
         'Latitude (N)'=as.numeric(`Latitude (N)`),
         Year=as.numeric(Year))%>% 
  filter(!is.na(`Longitude (W)`)) %>%
  st_as_sf(coords=c('Longitude (W)','Latitude (N)'),crs=4326)%>% 
  dplyr::rename(StnLocation=Location,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')

gulf_tunicate_incidental_2021 <- readxl::read_excel("recentdata/Copy of P-A Table_2021 data_March2022.xlsx",sheet=2,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Location,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                #"Caprella_mutica"="C mutica",
                #"Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')

gulf_tunicate_incidental_2023<-readxl::read_excel("recentdata/Gulf_incidental_new detections_2023.xlsx",col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Location,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')

gulf_tunicate_incidental_2024<-readxl::read_excel("recentdata/Gulf 2024 AIS Data_Feb2025.xlsx",sheet=2,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=`Location Name`,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Membranipora_membranacea"="M membranacea",
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile",
                "Juxtacribrilina_mutabilis"="J mutabilis") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')

gulf_tunicate_incidental_2025<-readxl::read_excel("recentdata/Gulf 2025 AIS Data_Science Gulf_Feb2026.xlsx",sheet=2,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=`Location Name`,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Membranipora_membranacea"="M membranacea",
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile",
                "Juxtacribrilina_mutabilis"="J mutabilis",
                "Fucus_serratus"="F serratus",
                "Caprella_mutica"="C mutica") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')

mar_incidental <- read.csv("recentdata/Incidental_AIS_Reports_MAR.csv")%>%
  filter(!Lat == 'NA')%>%
  sf::st_as_sf(coords=c('Lon','Lat'),crs=4326) %>% 
  filter(Picture_Confirmation==1) %>% 
  mutate(Presence=as.logical(Picture_Confirmation),
         prov = paste("Maritimes Incidental Data:", "Contact Sarah.Kingsbury@dfo-mpo.gc.ca")
         ) %>% 
  dplyr::select(Species,StnLocation,Year,prov)%>%
  filter(Species %in% species$R_Name) #only keep species of relevance to I&T transfers

NS_RAS_2025<-readxl::read_xlsx ("recentdata/NSRAS2025_nonindigenousspecies_summary_EditedJune2026.xlsx", sheet=2)%>%
  rename("StnLocation"="Station name")%>%
  sf::st_as_sf(coords=c("DecimalLONG","DecimalLAT"),crs=4326)%>%
  mutate(Year=2025,
         Presence="TRUE",
         prov="Huntsman Marine Science Centre")%>%
  select(-Group, -Date, -`Identified by`, -`Notes on confirmation of prescence`, -`Station code`)%>%
  mutate(Species=str_replace_all(Species, " ", "_"))

incidental_sites <- rbind(
  # incidental_occ %>%
  #   dplyr::select(StnLocation),
  # asian_shore_crab_2020 %>% 
  #   dplyr::select(StnLocation),
  gulf_tunicate_incidental_2020 %>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2021%>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2023%>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2024%>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2025%>% 
    dplyr::select(StnLocation),
  mar_incidental%>%
    dplyr::select(StnLocation),
  NS_RAS_2025%>%
    dplyr::select(StnLocation),
  NL_incidental%>%
    dplyr::select(StnLocation)
)%>%
  na.omit()%>%
  dplyr::group_by(StnLocation) %>% 
  dplyr::summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  sf::st_transform(equidist) %>% 
  dplyr::filter(geometry%>% 
                  st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
                  lengths()>0) %>% 
  sf::st_transform(proj)

#Note: incidental reports from iNat and GBiF will be moved to a public reports tab because these are unverified reports
incidental <-  dplyr::bind_rows(
  # incidental_occ %>%
  #   mutate(across(.fns = as.character))%>%
  #   as.data.table(),
  # asian_shore_crab_2020 %>%
  #   dplyr::mutate(across(.fns = as.character))%>%
  #   as.data.table(),
  gulf_tunicate_incidental_2020 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2021 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2023 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2024 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2025 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  mar_incidental%>%
    dplyr::mutate(across(.fns=as.character))%>%
    as.data.table(),
  NS_RAS_2025%>%
    dplyr::mutate(across(.fns=as.character))%>%
    as.data.table(),
  NL_incidental%>%
    dplyr::mutate(across(.fns=as.character))%>%
    as.data.table()
) %>% 
  unique() %>%
  dplyr::select(Species,StnLocation,Year,prov) %>% 
  dplyr::right_join(incidental_sites,by = "StnLocation") %>% 
  st_sf()%>%
  na.omit()

leaflet::leaflet(incidental_sites) %>% leaflet::addTiles() %>% leaflet::addMarkers()

saveRDS(incidental_sites,"outputdata/incidental_sites.rds")
saveRDS(incidental,"outputdata/incidental.rds")

#Public incidental reports

publicdata_sites <- incidental_occ %>%
  dplyr::select(StnLocation)%>%
  dplyr::group_by(StnLocation) %>% 
  dplyr::summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  sf::st_transform(equidist) %>% 
  dplyr::filter(geometry%>% 
                  st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
                  lengths()>0) %>% 
  sf::st_transform(proj)

publicdata<- incidental_occ %>%
  dplyr::mutate(across(.fns = as.character))%>%
  as.data.table()%>%
  unique() %>%
  dplyr::select(Species,StnLocation,Year,prov) %>% 
  dplyr::right_join(publicdata_sites,by = "StnLocation") %>% 
  st_sf()%>%
  na.omit()

saveRDS(publicdata_sites,"outputdata/publicdata_sites.rds")
saveRDS(publicdata,"outputdata/publicdata.rds")

leaflet::leaflet(publicdata_sites) %>% leaflet::addTiles() %>% leaflet::addMarkers()

#Genomics Data

#Attempting layer pull into R from Claudio's eDNA result list
library(arcgislayers)

base_url <- "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/"

# Explicitly target the data-holding year layers underneath ID 362
leaf_ids <- c(363, 364, 365, 367, 368, 369, 371, 372, 373, 375, 376, 377, 379, 380, 381, 383, 384, 385, 387, 388, 389, 391, 392, 393, 395, 396, 397, 399, 400, 401) 

# Construct URLs, open connections, and pull spatial tables directly
edna_sf <- lapply(leaf_ids, function(id) {
  layer_url <- paste0(base_url, id)
  message("Connecting directly to target layer ID: ", id)
  
  opened_layer <- arc_open(layer_url)
  arc_select(opened_layer)
})

eDNA_df<-rbindlist(edna_sf, fill=TRUE)%>%
  filter(cover_index>0)%>%
  rename("StnLocation"= "stn_location",
         "Year"="year",
         "Species"="species_name")%>%
  select(-OBJECTID, -province, -stn_num, -latitude, -longitude, -cover_index)%>%
  mutate(Species=str_replace_all(Species, " ", "_"),
         Presence=TRUE)%>%
  st_as_sf()
  

eDNA_sites<-eDNA_df%>% 
  dplyr::select(StnLocation, geometry)%>%
  na.omit()%>%
  st_as_sf()%>%
  dplyr::group_by(StnLocation) %>% 
  dplyr::summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  sf::st_transform(equidist) %>% 
  dplyr::filter(geometry%>% 
                  st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
                  lengths()>0) %>% 
  st_transform(proj)


eDNA<-eDNA_df%>%
    as.data.frame()%>%
  pivot_wider(
    id_cols = c(StnLocation, Year, geometry),
    names_from = Species, 
    values_from = Presence,
    # Safety feature: If a duplicate still exists, just fill with "1" instead of c("1", "1")
    values_fn = list(Presence = function(x) "1"), 
    # Optional: Fill stations where the species was NOT found with "0" instead of NA
    values_fill = "0" 
  )%>%
  st_sf() %>% 
  mutate(StnLocation=gsub("[ \t]+$","",StnLocation))

saveRDS(eDNA_sites, "outputdata/eDNA_sites.rds")
saveRDS(eDNA, "outputdata/eDNA.rds")

# Load and clean up monitoring data ---------------------------------------

# Maritimes Tunicates
#Biofouling data for MAR is available here: https://open.canada.ca/data/en/dataset/8d87f574-0661-40a0-822f-e9eabc35780d
#Please note that due to the file size, some of the data will be accessed in the first pull and the rest accessed in teh second pull. Data includes 2006-2023
maritimes_tunicate_monitor_1 <- rbind(
  (lapply(4:17,function(x)
     {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for A. aspersa
     })%>%
     bind_rows()),
  (lapply(19:38,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for B. violaceus
  })%>%
    bind_rows()),
  (lapply(40:59,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for B. schlosseri
  })%>%
    bind_rows()),
  (lapply(61:74,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for C. mutica
  })%>%
    bind_rows()),
  (lapply(76:95,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for S. clava
  })%>%
    bind_rows())
  )%>%
  dplyr::rename(geometry=geoms,Year = year, StnLocation=stn_location)%>% 
  st_transform(proj) %>% 
  filter(cover_index=="1")%>%
  dplyr::select(-OBJECTID,-latitude,-longitude,-cover_index,-province,-stn_num)

maritimes_tunicate_monitor_2 <- rbind(
  (lapply(97:110,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for C. intestinalis
  })%>%
    bind_rows()),
  (lapply(112:125,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for D. vexillum
  })%>%
    bind_rows()),
  (lapply(127:128,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for D. listerianum
  })%>%
    bind_rows()),
  (lapply(130:143,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for M. membranacea
  })%>%
    bind_rows()),
  (lapply(145:163,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for M. membranacea
  })%>%
    bind_rows())
)%>%
  dplyr::rename(geometry=geoms,Year = year, StnLocation=stn_location)%>% 
  st_transform(proj) %>% 
  filter(cover_index=="1")%>%
  dplyr::select(-OBJECTID,-latitude,-longitude,-province,-stn_num, -cover_index)

maritimes_tunicate_monitor <- rbind(maritimes_tunicate_monitor_1,
                                    maritimes_tunicate_monitor_2) %>%
  mutate(Species = gsub(" ", "_", species_name)) %>%
  dplyr::select(-species_name) %>%
  mutate(Presence = "1") %>% # Simpler way to set character "1"
  # 1. CRITICAL STEP: Remove duplicates before pivoting
  # This stops c("1", "1") from forming by flattening duplicates
  distinct(StnLocation, Year, Species, geometry, .keep_all = TRUE) %>%
  
  as.data.frame() %>%
  
  # 2. Pivot data wide
  pivot_wider(
    id_cols = c(StnLocation, Year, geometry),
    names_from = Species, 
    values_from = Presence,
    # Safety feature: If a duplicate still exists, just fill with "1" instead of c("1", "1")
    values_fn = list(Presence = function(x) "1"), 
    # Optional: Fill stations where the species was NOT found with "0" instead of NA
    values_fill = "0" 
  ) %>%
  
  st_as_sf() %>%
  st_transform(proj)

# Gulf Tunicates
# gulf_tunicate_monitor <- esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_Gulf_Region_Aquatic_Invasive_Species_Data/MapServer/0")
# the above Gulf data is included in the xlsx file below


gulf_tunicate_monitor_2020 <- readxl::read_excel("recentdata/Gulf AIS data_biof_monit_incidental_AISNCP MAR_April 2021.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile")%>%
  dplyr::select(-Province)%>%
  #mutate(Year=as.character(Year))%>%
  #as.data.frame()%>%
  st_transform(proj)
  # mutate(Juxtacribrilina_mutabilis=as.character("0"),
  #        Year=as.character(Year))%>%
  # st_transform(proj)

gulf_tunicate_monitor_2021 <- readxl::read_excel("recentdata/Copy of P-A Table_2021 data_March2022.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile")%>%
  dplyr::select(-Province)%>%
  #mutate(Year=as.character(Year))%>%
  #as.data.frame()
  # mutate(Juxtacribrilina_mutabilis=as.character("0"),
  #        Year=as.character(Year))%>%
   st_transform(proj)

gulf_tunicate_monitor_2022 <- readxl::read_excel("recentdata/Copy of 2022 P-A Data_AIS monitoring_Gulf Region_Jan2023.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station_Name)%>%
  mutate(Year=2022)%>%
  dplyr::select(-Date_In, -Date_Out, -Province) %>%
   #mutate(Year=as.character(Year))%>%
  #as.data.frame()
  # mutate(Juxtacribrilina_mutabilis=as.character("0"),
  #        Year=as.character(Year))%>%
   st_transform(proj)

#missing data for 2023 from Gulf

gulf_tunicate_montior_2024<-readxl::read_excel("recentdata/Gulf 2024 AIS Data_Feb2025.xlsx", sheet=1, col_types = 'text') %>% 
  filter(!Latitude=="NA")%>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=`Location/Station Name`,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile",
                "Juxtacribrilina_mutabilis"="J mutabilis"
                )%>%
   dplyr::select(-Province, -Comments)%>%
  #as.data.frame()
  st_transform(proj)
  
gulf_tunicate_montior_2025<-readxl::read_excel("recentdata/Gulf 2025 AIS Data_Science Gulf_Feb2026.xlsx", sheet=1, col_types = 'text') %>% 
  filter(!Latitude=="NA")%>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation="Station Name",
                "Botryllus_schlosseri"="B. schlosseri",
                "Botrylloides_violaceus"="B. violaceus",
                "Ciona_intestinalis"="C. intestinalis",
                "Styela_clava"="S. clava",
                "Caprella_mutica"="C. mutica",
                "Membranipora_membranacea"="M. membranacea", 
                "Carcinus_maenas"="C. maenas",
                "Codium_fragile"="C. fragile",
                "Juxtacribrilina_mutabilis"="J. mutabilis",
                "Fucus_serratus"="F. serratus",
                "Diadumene_lineata"="D. lineata",
  )%>%
  dplyr::select(-Province, -Comments)%>%
  #as.data.frame()
  st_transform(proj)

gulf_tunicate_monitor<-bind_rows(gulf_tunicate_monitor_2020,
                             gulf_tunicate_monitor_2021,
                             gulf_tunicate_monitor_2022)%>%
  st_transform(proj)

gulf_tunicate_monitor<-rbind(gulf_tunicate_monitor%>%mutate(Juxtacribrilina_mutabilis=0, Fucus_serratus=0, Diadumene_lineata=0),
                             gulf_tunicate_montior_2024%>%mutate(Fucus_serratus=0, Diadumene_lineata=0),
                             gulf_tunicate_montior_2025)%>%
  st_as_sf()%>%
  st_transform(proj)


###QUebec Monitoring
QC_monitoring<-read_csv("recentdata/quebec_itapp_collector_data.csv")%>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326) %>%
  dplyr::rename(Year = year, StnLocation=station_name)%>% 
  st_transform(proj) %>% 
  filter(cover_index>0)%>%
  mutate(Presence=TRUE)%>%
  dplyr::select(-province, -ecoregion, -station_name_text, -cover_index)%>%
  as.data.frame() %>%
  
  # 2. Pivot data wide
  pivot_wider(
    id_cols = c(StnLocation, Year, geometry),
    names_from = species_name, 
    values_from = Presence,
    # Safety feature: If a duplicate still exists, just fill with "1" instead of c("1", "1")
    values_fn = list(Presence = function(x) "1"), 
    # Optional: Fill stations where the species was NOT found with "0" instead of NA
    values_fill = "0" 
  ) %>%
  
  st_as_sf() %>%
  st_transform(proj)

# Define a brand new, wide bounding box for Atlantic Canada (WGS84 degrees)
atlantic_bbox <- st_bbox(
  c(xmin = -80.0, xmax = -52.0, ymin = 43.0, ymax = 63.0), 
  crs = 4326
)
searcharea <- st_as_sfc(atlantic_bbox) # Converts the box into a usable polygon layer

monitoring_sites <- rbind(maritimes_tunicate_monitor%>% 
                            dplyr::select(StnLocation), 
                          gulf_tunicate_monitor %>% 
                            dplyr::select(StnLocation),
                          # tunicates_sf%>% 
                          #   dplyr::select(StnLocation),
                          QC_monitoring%>%
                            dplyr::select(StnLocation)
                          ) %>% 
  group_by(StnLocation) %>% 
  summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  #unique() %>% 
  mutate(StnLocation = iconv(as.character(StnLocation), from = "WINDOWS-1252", to = "UTF-8", sub = ""),
         StnLocation=gsub("[ \t]+$","",StnLocation)) %>% 
  st_transform(equidist) %>% 
  filter(geometry%>% 
           st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
           lengths()>0) %>% 
  st_transform(proj)

#leaflet::leaflet(monitoring_sites) %>% leaflet::addTiles() %>% leaflet::addMarkers()


# monitoring <- rbind(maritimes_tunicate_monitor%>%
#                       mutate(Carcinus_maenas=0,
#                              Codium_fragile=0,
#                              Fucus_serratus=0,
#                              Diadumene_lineata=0)%>%
#                           as.data.table(),
#                         gulf_tunicate_monitor %>% 
#                       mutate(Ascidiella_aspersa=0,
#                              Diplosoma_listerianum=0,
#                              Didemnum_vexillum=0,
#                              Tricellaria_inopinata=0)%>%
#                           as.data.table(),
#                     tunicates_sf%>%
#                       mutate(Carcinus_maenas=0,
#                              Codium_fragile=0,
#                              Fucus_serratus=0,
#                              Diadumene_lineata=0)%>%
#                       as.data.table()) %>% 
#   dplyr::select(-geometry) %>% 
#   unique()%>%
#   gather(key = "Species", value = "Presence",-StnLocation,-Year) %>%
#   group_by(Species,StnLocation,Year) %>% 
#   #unique()%>%
#   summarize(Presence = if_else(all(is.na(Presence)),
#                                FALSE,
#                                any(Presence>0,na.rm = TRUE))) %>%
#   ungroup() %>%
#   spread(key = "Species", value = "Presence") %>%
#   right_join(monitoring_sites,by = "StnLocation") %>%
#   st_sf() %>% 
#   mutate(StnLocation=gsub("[ \t]+$","",StnLocation))

monitoring <- bind_rows(
  # Force Year and all species presence data to numeric types across all sets
  maritimes_tunicate_monitor %>% st_drop_geometry() %>% mutate(Year = as.integer(Year), across(!StnLocation & !Year, as.numeric)),
  gulf_tunicate_monitor      %>% st_drop_geometry() %>% mutate(Year = as.integer(Year), across(!StnLocation & !Year, as.numeric)),
 # tunicates_sf               %>% st_drop_geometry() %>% mutate(Year = as.integer(Year), across(!StnLocation & !Year, as.numeric)),
  QC_monitoring %>% st_drop_geometry()%>%mutate(Year = as.integer(Year), across(!StnLocation & !Year, as.numeric))
) %>% 
  # 1. Deduplicate the raw data layout early
  unique() %>%
  
  # 2. Reshape from wide to long format to collapse duplicates cleanly
  pivot_longer(
    cols = !c(StnLocation, Year), 
    names_to = "Species", 
    values_to = "Presence"
  ) %>%
  
  # 3. Handle duplicates: Aggregate presence status for identical combinations
  group_by(Species, StnLocation, Year) %>% 
  summarize(
    Presence = if_else(
      all(is.na(Presence)),
      FALSE,
      any(Presence > 0, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  
  # 4. Reshape back to wide format with clean TRUE/FALSE logical headers
  pivot_wider(
    names_from = Species, 
    values_from = Presence, 
    values_fill = FALSE
  ) %>%
  
  # 5. Connect back to your master spatial grid layout
  right_join(monitoring_sites, by = "StnLocation") %>%
  st_sf() %>% 
  mutate(StnLocation = iconv(as.character(StnLocation), to = "UTF-8", sub = "")) %>%
  mutate(StnLocation = gsub("[ \t]+$", "", StnLocation))

#leaflet::leaflet(monitoring) %>% leaflet::addTiles() %>% leaflet::addMarkers()


saveRDS(monitoring_sites,"outputdata/monitoring_sites.rds")
saveRDS(monitoring,"outputdata/monitoring.rds")


if(!file.exists("spatialdata/gshhg-shp-2.3.7.zip")){
  print("Downloading Coastline")
  curl::curl_download(url="http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-shp-2.3.7.zip",
                      destfile = "spatialdata/gshhg-shp-2.3.7.zip")
  utils::unzip("spatialdata/gshhg-shp-2.3.7.zip",exdir="spatialdata")
}


# GSHHS is from http://www.soest.hawaii.edu/pwessel/gshhs/index.html
maritimes <- st_read("spatialdata/GSHHS_shp/f/GSHHS_f_L1.shp") %>% 
  filter(st_is_valid(geometry)) %>%
  st_transform(equidist) %>% 
  st_crop(st_bbox(st_transform(searcharea,equidist))) %>% 
  st_union() %>% 
  st_cast('POLYGON') %>% 
  st_sf() 

source("functions.R")

#### set up transition matrix ####
library(raster)
library(fasterize)
library(sp)

print("Setting up transition matrix")
#searchbox<-extent(st_bbox(monitoring_sites %>% st_transform(equidist)))

# r <- raster(
#   #maritimes,
#             # xmn=628730, #issue with extent and x and y values for min/max. 
#             # xmx=1374056 , #Maritimes polygon is cropped to the box extent above, therefore, no need to repeat the operation here.
#             # ymn=603784.7,
#             # ymx=1311254,
#             ext=searchbox,
#             #ext=extent(st_bbox(searcharea %>% st_transform(equidist))),
#             res = 1000)

searchbox <- publicdata_sites %>%
  st_transform(equidist) %>%
  st_bbox() %>%
  st_as_sfc() %>%          # convert bbox to polygon
  st_buffer(10000)

r <- raster(
  as(searchbox, "Spatial"),
  res = 1000
)

r <- fasterize(maritimes, r)
r@data@values[r@data@values==1] <- 1
r@data@values[is.na(r@data@values)] <- 10000
plot(r)
tr <- transition(r, mean, directions = 16, symm=TRUE)
saveRDS(tr,"outputdata/transition.rds")


####calculate incidental distance for each province ####

print("Calculating in water distances for NS")
ns_incidental_dist <- do.call(rbind,(lapply(NS$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(incidental_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
                                      
row.names(ns_incidental_dist) <- NS$Lease_Identifier
colnames(ns_incidental_dist) <- incidental_sites$StnLocation
saveRDS(ns_incidental_dist,"outputdata/ns_incidental_dist.rds")


print("Calculating in water distances for NB")
nb_incidental_dist <- do.call(rbind,(lapply(NB$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(incidental_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(nb_incidental_dist) <- NB$Lease_Identifier
colnames(nb_incidental_dist) <- incidental_sites$StnLocation
saveRDS(nb_incidental_dist,"outputdata/nb_incidental_dist.rds")



print("Calculating in water distances for PEI")
pei_incidental_dist <- do.call(rbind,(lapply(PEI$geometry %>%
                                               st_transform(equidist),
                                             function(x) inwaterdistance(incidental_sites %>%
                                                                           st_transform(equidist),
                                                                         x,
                                                                         tr))))
row.names(pei_incidental_dist) <- PEI$Lease_Identifier
colnames(pei_incidental_dist) <- incidental_sites$StnLocation
saveRDS(pei_incidental_dist,"outputdata/pei_incidental_dist.rds")

print("Calculating in water distances for NL")
NL_incidental_dist <- do.call(rbind,(lapply(NL$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(incidental_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(NL_incidental_dist) <- NL$Lease_Identifier
colnames(NL_incidental_dist) <- incidental_sites$StnLocation
saveRDS(NL_incidental_dist,"outputdata/NL_incidental_dist.rds")

QC_incidental_dist <- do.call(rbind,(lapply(QC$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(incidental_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(QC_incidental_dist) <- QC$Lease_Identifier
colnames(QC_incidental_dist) <- incidental_sites$StnLocation
saveRDS(QC_incidental_dist,"outputdata/QC_incidental_dist.rds")


###Calculate monitoring site distances for each prov####
# searchbox <- monitoring_sites %>%
#   st_transform(equidist) %>%
#   st_bbox() %>%
#   st_as_sfc() %>%          # convert bbox to polygon
#   st_buffer(10000)
# 
# r <- raster(
#   as(searchbox, "Spatial"),
#   res = 1000
# )
# 
# r <- fasterize(maritimes, r)
# r@data@values[r@data@values==1] <- 1
# r@data@values[is.na(r@data@values)] <- 10000
# plot(r)
# tr <- transition(r, mean, directions = 16, symm=TRUE)
# saveRDS(tr,"outputdata/transition.rds")
leaflet::leaflet(monitoring_sites) %>% leaflet::addTiles() %>% leaflet::addMarkers()

ns_monitoring_dist <- do.call(rbind,(lapply(NS$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(monitoring_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(ns_monitoring_dist) <- NS$Lease_Identifier
colnames(ns_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(ns_monitoring_dist,"outputdata/ns_monitoring_dist.rds")

nb_monitoring_dist <- do.call(rbind,(lapply(NB$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(monitoring_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(nb_monitoring_dist) <- NB$Lease_Identifier
colnames(nb_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(nb_monitoring_dist,"outputdata/nb_monitoring_dist.rds")

# PEI <- sf::st_make_valid(PEI)
# PEI <- PEI %>% 
#   filter(st_is(geometry, c("POLYGON"))) #remove single linestring lease from dataset
# saveRDS(PEI, "spatialdata/PEI.rds")

pei_monitoring_dist <- do.call(rbind,(lapply(PEI$geometry %>%
                                               st_transform(equidist),
                                             function(x) inwaterdistance(monitoring_sites %>%
                                                                           st_transform(equidist),
                                                                         x,
                                                                         tr))))
row.names(pei_monitoring_dist) <- PEI$Lease_Identifier
colnames(pei_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(pei_monitoring_dist,"outputdata/pei_monitoring_dist.rds")

NL_monitoring_dist <- do.call(rbind,(lapply(NL$geometry %>%
                                               st_transform(equidist),
                                             function(x) inwaterdistance(monitoring_sites %>%
                                                                           st_transform(equidist),
                                                                         x,
                                                                         tr))))


row.names(NL_monitoring_dist) <- NL$Lease_Identifier
colnames(NL_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(NL_monitoring_dist,"outputdata/NL_monitoring_dist.rds")

QC_monitoring_dist <- do.call(rbind,(lapply(QC$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(monitoring_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(QC_monitoring_dist) <- QC$Lease_Identifier
colnames(QC_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(QC_monitoring_dist,"outputdata/QC_monitoring_dist.rds")
