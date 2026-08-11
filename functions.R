#### inwaterdistance ####
inwaterdistance <- function(origin, destination, transition){
  shortestPath(transition, st_coordinates(suppressWarnings(st_centroid(destination))), st_coordinates(origin), output="SpatialLines") %>% 
    st_as_sf() %>% 
    st_length() %>% 
    as.numeric()
}



#### nearestsites ####
nearestsites <- function(lease,prov,sites,n,distmat){
  # if("MF-0491"==lease$Lease_Identifier){browser()}
  #browser()
  if(length(lease$Lease_Identifier)==0){
    stop("Invalid lease identifier (lease error)")
  }
  if(!lease$Lease_Identifier %in% prov$Lease_Identifier){
    stop("Invalid lease identifier (prov error)")
  }
  if(!lease$Lease_Identifier %in% row.names(distmat)){
    #browser()
    stop("Invalid lease identifier (distmat error)")
  }
  #browser()
  # print("calculating nearest sites")
  # distances <- data.table(StnLocation=colnames(distmat),
  #                         distance=distmat[row.names(distmat)==as.character(lease$Lease_Identifier),]) 
  distances <- data.table(
    StnLocation = as.character(colnames(distmat)), 
    distance = as.numeric(distmat[match(as.character(lease$Lease_Identifier), row.names(distmat)), ])
  )
  # if(ncol(distances)!=2){
  #   browser()
  # }
  
  if(!"Species" %in% names(sites)) {
    # sites for monitoring
    sites %>%
      left_join(distances, by = "StnLocation") %>% 
      top_n(-n,distance) %>% 
      arrange(distance) %>% 
      mutate(StnLocation = paste0(StnLocation," (",round(distance/1000)," km)"))
  } else {
    # sites for incidentals
    sites %>%
      left_join(distances, by = "StnLocation") %>% 
      top_n(-n,distance) %>% 
      arrange(distance) %>% 
      mutate(StnLocation = paste0(Species, " ", StnLocation," (",round(distance/1000)," km)"))
  }
  
}

species_palette <- c(
  Didemnum_vexillum          = "#2C7FB8",  # blue
  Botryllus_schlosseri       = "#41B6C4",  # teal
  Botrylloides_violaceus     = "#6A51A3",  # purple
  Diplosoma_listerianum      = "#9E9AC8",  # lavender
  Ciona_intestinalis         = "#1B9E77",  # green-teal
  Styela_clava               = "#66C2A4",  # pale teal
  Ascidiella_aspersa         = "#238B45",  # green
  Caprella_mutica            = "#A1D99B",  # light green
  
  Carcinus_maenas            = "#B35806",  # brown-orange
  Codium_fragile             = "#FDB863",  # pale orange
  Membranipora_membranacea   = "#E08214",  # orange
  Hemigrapsus_sanguineus     = "#D7301F",  # red
  Oncorhynchus_mykiss        = "#EF6548",  # coral
  Argopecten_irradians       = "#FB9A99",  # light red
  
  Ostrea_edulis              = "#8C510A",  # dark brown
  Juxtacribrilina_mutabilis  = "#DFC27D",  # sand
  Sargassum_muticum          = "#7F3B08",  # deep brown
  Tricellaria_inopinata      = "#542788",  # deep purple
  Fucus_serratus             = "#01665E",  # dark teal
  Diadumene_lineata          = "#80CDC1"   # pale blue-green
)

#### basemap ####
basemap <- function(leases, incidentals, monitoring, monitoringsp,...){
  
 #browser()
  
  sp <- monitoringsp[monitoringsp %in% names(monitoring)]

  palette_for_minicharts <- species_palette[sp]
  
  leaflet(leases,...) %>%
    addTiles() %>%
    addPolygons(popup = paste("Lease:",leases$Lease_Identifier),group = "Leases") %>%
    addCircleMarkers(data=incidentals, 
                     lng=st_coordinates(incidentals$geometry)[,1],
                     lat=st_coordinates(incidentals$geometry)[,2],
                     label = ~as.character(incidentals$Species),
                     group = incidentals$Species,
                     fillColor = 'black',
                     color = 'darkgrey',
                     fillOpacity = 0.7)%>%
    addMinicharts(st_coordinates(monitoring$geometry)[,1],
                  st_coordinates(monitoring$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(monitoring)[,sp],
                  colorPalette = unname(palette_for_minicharts),
                  legend = TRUE,
                  legendPosition = 'topright') %>%
    addLayersControl(overlayGroups = c("Leases",incidentals$Species),
                     options = layersControlOptions(collapsed = FALSE))
}

#### basemap eDNA ####
basemap_eDNA <- function(leases, eDNA, eDNAsp,...){
 
  # browser()
  
  sp_eDNA <- eDNAsp[eDNAsp %in% names(eDNA)]
  
  palette_for_minicharts_eDNA <- species_palette[sp_eDNA]
  
  leaflet(leases,...) %>%
    addTiles() %>%
    addPolygons(popup = paste("Lease:",leases$Lease_Identifier),group = "Leases") %>%
    addMinicharts(st_coordinates(eDNA$geometry)[,1],
                  st_coordinates(eDNA$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(eDNA)[,sp_eDNA],
                  colorPalette = unname(palette_for_minicharts_eDNA),
                  legend = TRUE,
                  legendPosition = 'topright') %>%
    addLayersControl(overlayGroups = c("Leases"),
                     options = layersControlOptions(collapsed = FALSE))
  
}

#### basemap Public Species Reports ####
basemap_pReport<- function(leases, publicdata, publicdatasp,...){
  
 # browser()
  
  leaflet(leases,...) %>%
    addTiles() %>%
    addPolygons(popup = paste("Lease:",leases$Lease_Identifier),group = "Leases") %>%
    addCircleMarkers(data=publicdata, 
                     lng=st_coordinates(publicdata$geometry)[,1],
                     lat=st_coordinates(publicdata$geometry)[,2],
                     label = ~as.character(publicdata$Species),
                     group = publicdata$Species,
                     fillColor = 'black',
                     color = 'darkgrey',
                     fillOpacity = 0.7)%>%
  addLayersControl(overlayGroups = c("Leases",publicdata$Species),
                   options = layersControlOptions(collapsed = FALSE))
}
  


create_response <- function(summitigation,species){
  if("Site" %in% names(summitigation)){
    #browser()
    summitigation$Site
  } else if("Risk Assessment" %in% names(summitigation)){
    mitigation <- read.csv("mitigation.csv")
    if("High risk" %in% summitigation$`Risk Assessment`){
      paste0("The risk to AIS/FFHPP is high with medium certainty because there are aquatic invasive species (",
             paste(unique(summitigation$Common_Name[summitigation$`Risk Assessment`=="High risk"]),collapse = ", "),
             ") present at the origin site that are not found at the destination site")
    }else {
      paste0("The risk to AIS/FFHPP is considered low with high certainty, with mitigation, because all aquatic invasive species (",
             paste(unique(summitigation$Common_Name),collapse = ", "),
             ") present at the origin site are also present at the destination site. To reduce the risk of further spreading aquatic invasive species, the following mitigation treatment(s) are recommended: SELECT MITIGATION OPTION FROM TABEL 2")
      # paste0("The risk to AIS/FFHPP is considered low with high certainty, with mitigation, because all aquatic invasive species,"
      #        (paste(unique(summitigation$Common_Name),collapse = ", ")),
      #        "present at the origin site are also present at the destination site. To reduce the risk of further spreading aquatic invasive species, the following mitigation treatment(s) are recommended: SELECT MITIGATION OPTION FROM TABEL 2",
      #        #tolower(paste(unique(summitigation$Treatment_proposed),collapse = "; or, "))
      #        )
      
    }
  } else {
    "Error: Could not generate a response (called from create_response())"
  }
}

