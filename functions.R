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
  
  # print("calculating nearest sites")
  distances <- data.table(StnLocation=colnames(distmat),
                          distance=distmat[row.names(distmat)==as.character(lease$Lease_Identifier),]) 
  
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

#### basemap ####
basemap <- function(leases, incidentals, monitoring, monitoringsp,...){
  #browser()

  # IncidentalIcons <- iconList(
  #   "Argopecten_irradians" = makeIcon(
  #     iconUrl = "mussel.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Ascidiella_aspersa" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Botrylloides_violaceus" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Botryllus_schlosseri" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Caprella_mutica" = makeIcon(
  #     iconUrl = "skeletonshrimp.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Carcinus_maenas" = makeIcon(
  #     iconUrl = "GreenCrab.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Ciona_intestinalis" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Codium_fragile" = makeIcon(
  #     iconUrl = "algae.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Didemnum_vexillum" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Diplosoma_listerianum" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Hemigrapsus_sanguineus" = makeIcon(
  #     iconUrl = "blackCrab.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Membranipora_membranacea" = makeIcon(
  #     iconUrl = "algae.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Oncorhynchus_mykiss" = makeIcon(
  #     iconUrl = "trout.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Ostrea_edulis" = makeIcon(
  #     iconUrl = "mussel.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Styela_clava" = makeIcon(
  #     iconUrl = "blackCrab.png",
  #     iconWidth = 30,
  #     iconHeight = 15)
    # "Procambarus_clarkii"=makeIcon(
    #   iconUrl="crayfish.png",
    #   iconWidth = 30,
    #   iconHeight = 15),
    # "Esox_niger"=makeIcon(
    #   iconUrl="pike.png",
    #   iconWidth = 30,
    #   iconHeight = 15),
    # "Micopterus_dolomieu"=makeIcon(
    #   iconUrl="trout.png",
    #   iconWidth = 30,
    #   iconHeight = 15),
    # "Cipangopaludina_chinensis"=makeIcon(
    #   iconUrl="snail.png",
    #   iconWidth = 30,
    #   iconHeight = 15),
    # "Carassius_auratus"=makeIcon(
    #   iconUrl="goldfish.png",
    #   iconWidth = 30,
    #   iconHeight = 15)
  #)
  
  # if(!all(incidentals$Species %in% names(IncidentalIcons))){
  #   warning(paste0("basemap() in functions.R does not have a logo associated with: ",
  #                  unique(incidentals$Species[!incidentals$Species %in% names(IncidentalIcons)]),
  #                  collapse="\n"))
 # }


  
 #browser()
  sp <- monitoringsp[monitoringsp %in% names(monitoring)]

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
    # addMarkers(data=incidentals$geometry,
    #            icon = IncidentalIcons[as.numeric(factor(incidentals$Species,levels=sort(monitoringsp)))],
    #            group = incidentals$Species
    #            #popup = incidentals$link
    #            ) %>%
    addMinicharts(st_coordinates(monitoring$geometry)[,1],
                  st_coordinates(monitoring$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(monitoring)[,sp],
                  #layerId= monitoring,
                  #colorPalette = d3.schemeCategory10,
                  legend = TRUE,
                  legendPosition = 'topright') %>%
    addLayersControl(overlayGroups = c("Leases",incidentals$Species),
                     options = layersControlOptions(collapsed = FALSE))
}

#### basemap eDNA ####
basemap_eDNA <- function(leases, metabarcoding, metabarcodingsp,...){
  #browser()
  sp_eDNA <- metabarcodingsp[metabarcodingsp %in% names(metabarcoding)]
  
  leaflet(leases,...) %>%
    addTiles() %>%
    addPolygons(popup = paste("Lease:",leases$Lease_Identifier),group = "Leases") %>%
    addMinicharts(st_coordinates(metabarcoding$geometry)[,1],
                  st_coordinates(metabarcoding$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(metabarcoding)[,sp_eDNA],
                  legend = TRUE,
                  legendPosition = 'topright') %>%
    addLayersControl(overlayGroups = c("Leases"),
                     options = layersControlOptions(collapsed = FALSE))
  
}

#### basemap Public Species Reports ####
basemap_pReport<- function(leases, publicdata, publicdatasp,...){
  
 # browser()
  # publicIcons <- iconList(
  #   "Argopecten_irradians" = makeIcon(
  #     iconUrl = "mussel.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Ascidiella_aspersa" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Botrylloides_violaceus" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Botryllus_schlosseri" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Caprella_mutica" = makeIcon(
  #     iconUrl = "skeletonshrimp.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Carcinus_maenas" = makeIcon(
  #     iconUrl = "GreenCrab.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Ciona_intestinalis" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Codium_fragile" = makeIcon(
  #     iconUrl = "algae.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Didemnum_vexillum" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Diplosoma_listerianum" = makeIcon(
  #     iconUrl = "tunicate.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Hemigrapsus_sanguineus" = makeIcon(
  #     iconUrl = "blackCrab.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Membranipora_membranacea" = makeIcon(
  #     iconUrl = "algae.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Oncorhynchus_mykiss" = makeIcon(
  #     iconUrl = "trout.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Ostrea_edulis" = makeIcon(
  #     iconUrl = "mussel.png",
  #     iconWidth = 30,
  #     iconHeight = 15),
  #   "Styela_clava" = makeIcon(
  #     iconUrl = "blackCrab.png",
  #     iconWidth = 30,
  #     iconHeight = 15)
  # )
  # 
  # if(!all(publicdata$Species %in% names(publicIcons))){
  #   warning(paste0("basemap() in functions.R does not have a logo associated with: ",
  #                  unique(publicdata$Species[!publicdata$Species %in% names(publicIcons)]),
  #                  collapse="\n"))
  # }
  
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
  # addMarkers(data=publicdata$geometry,
  #            icon = publicIcons[as.numeric(factor(publicdata$Species,levels=sort(publicdatasp)))],
  #            group = publicdata$Species) %>%
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

