#if(!require("shiny")) install.packages("shiny")
if(!require("sf")) install.packages("sf")
if(!require("leaflet")) install.packages("leaflet")
if(!require("leaflet.minicharts")) install.packages("leaflet.minicharts")
if(!require("data.table")) install.packages("data.table")
if(!require("tidyverse")) install.packages("tidyverse")

#### global variables ####

proj <- "+proj=longlat +datum=WGS84"


### download provincial lease data or just open the saved version
if(!file.exists("spatialdata/NS.rds")){
  if(!require("devtools")) install.packages("devtools")
  if(!require("esri2sf")) devtools::install_github("yonghah/esri2sf")
  NS <- esri2sf::esri2sf('https://services.arcgis.com/nQHSMRVltyfsxeFe/ArcGIS/rest/services/Marine_Lease_Boundary_Database_Shellfish_View/FeatureServer/0') %>% 
    filter(grepl("Issued",SiteStatus)|grepl("Propose",SiteStatus)|grepl("Approved Option",SiteStatus)) %>%
    # filter(SiteStatus=="Issued") %>% 
    mutate(Lease_Identifier=License_Lease_Num) %>% 
    st_transform(proj) %>% 
    rename(geometry=geoms)
  
  NB <- bind_rows(esri2sf::esri2sf('https://gis-erd-der.gnb.ca/arcgis/rest/services/MASMPS/MASMPS_service/MapServer/0') %>% rename(Lease_Identifier = MSNO),
                  esri2sf::esri2sf('https://gis-erd-der.gnb.ca/arcgis/rest/services/MASMPS/MASMPS_service/MapServer/1') %>% rename(Lease_Identifier = LPNO),
                  esri2sf::esri2sf('https://gis-erd-der.gnb.ca/arcgis/rest/services/MASMPS/MASMPS_service/MapServer/2') %>% rename(Lease_Identifier = MSNO)) %>% 
    st_transform(proj) %>% 
    rename(geometry=geoms)

  raw <- jsonlite::read_json(
    "https://www.arcgis.com/sharing/rest/content/items/16aa8830c7084a8a92ce066b525978b4/data",
    simplifyVector = FALSE
  )
  
  features <- c(raw$operationalLayers[[1]]$featureCollection$layers[[1]]$featureSet$features,
                raw$operationalLayers[[2]]$featureCollection$layers[[1]]$featureSet$features,
                raw$operationalLayers[[3]]$featureCollection$layers[[1]]$featureSet$features)
  
  PEI <- lapply(features, "[[", "attributes") %>% 
    lapply(as_tibble) %>% 
    bind_rows() %>% 
    mutate(geometry = esri2sf:::esri2sfPolygon(features)) %>% 
    st_as_sf() %>% 
    st_set_crs(features[[1]]$geometry$spatialReference$latestWkid)%>% 
    mutate(Lease_Identifier=Lease) %>% 
    st_transform(proj)
  
  saveRDS(NS,"spatialdata/NS.rds")
  saveRDS(NB,"spatialdata/NB.rds")
  saveRDS(PEI,"spatialdata/PEI.rds")
  
  
}
NS <- readRDS("spatialdata/NS.rds")
NB <- readRDS("spatialdata/NB.rds")
PEI <- readRDS("spatialdata/PEI.rds")


AIS <- read.csv("commonnames.csv")
AIS_meta<-read.csv("commonnames.csv")

sp_treatments <- read.csv("treatment.csv")%>% 
  complete(Scientific_Name,Product_treated, R_Name) %>% 
  left_join(read.csv("commonnames.csv",stringsAsFactors = FALSE),by = "R_Name")%>%
  tidyr::drop_na()

sp_mitigation <- read.csv("mitigation.csv",stringsAsFactors = FALSE)

product_sp <- as.list(c(unique(sp_treatments$Product_treated),
                        unique(sp_mitigation$Common_Name)))

if(!file.exists("outputdata/pei_monitoring_dist.rds")){
  source("prepare_data.R")
}

publicdata_sites <- readRDS("outputdata/publicdata_sites.rds")
publicdata <- readRDS("outputdata/publicdata.rds")
incidental_sites <- readRDS("outputdata/incidental_sites.rds")
incidental <- readRDS("outputdata/incidental.rds")
monitoring_sites <- readRDS("outputdata/monitoring_sites.rds")
monitoring <- readRDS("outputdata/monitoring.rds")
metabarcoding_sites<-readRDS("outputdata/metabarcoding_sites.rds")
metabarcoding<-readRDS("outputdata/metabarcoding.rds")
ns_incidental_dist <- readRDS("outputdata/ns_incidental_dist.rds")
ns_monitoring_dist <- readRDS("outputdata/ns_monitoring_dist.rds")
ns_metabarcoding_dist<-readRDS("outputdata/ns_metabarcoding_dist.rds")
nb_incidental_dist <- readRDS("outputdata/nb_incidental_dist.rds")
nb_monitoring_dist <- readRDS("outputdata/nb_monitoring_dist.rds")
nb_metabarcoding_dist<-readRDS("outputdata/nb_metabarcoding_dist.rds")
pei_incidental_dist <- readRDS("outputdata/pei_incidental_dist.rds")
pei_monitoring_dist <- readRDS("outputdata/pei_monitoring_dist.rds")
pei_metabarcoding_dist<-readRDS("outputdata/pei_metabarcoding_dist.rds")


greenCrabIcon <- makeIcon(
  iconUrl = "GreenCrab.png",
  iconWidth = 50,
  iconHeight = 37
)

# html_legend <- paste0("<img src='",getwd(),"/",greenCrabIcon$iconUrl,"'>  Green Crab")

source("functions.R")

#### User Interface ####
ui <- navbarPage(
  title = "Introductions and Transfers Tool",
  
  
  tabPanel("Origin",
           sidebarLayout(
             
             sidebarPanel(
               selectInput(inputId = "origprov",
                           label = "Choose a Province:",
                           choices = c("NS", "NB", "PEI")),
               selectInput(inputId = "origlease",
                           label = "Choose a lease:",
                           choices = NS$Lease_Identifier),
               numericInput(inputId = "origmonitoringnum",
                         label = "Number of Biofouling Monitoring Sites",
                         value = 3),
               numericInput(inputId = "origincidentalnum",
                         label = "Number of Incidental Observation Sites",
                         value = 5),
               checkboxGroupInput(inputId = "origmonitoringsite",
                                  label = "Biofouling Monitoring Sites to Include",
                                  choices = "temp"),
               checkboxGroupInput(inputId = "origincidentalsite",
                                  label = "Incidental Observation Sites to Include",
                                  choices = "temp")
             ),
             
             mainPanel(
               leafletOutput("leafletorig",height=800)
             )
             
           )
  ),
  
  
  tabPanel("Destination",
           sidebarLayout(
             
             sidebarPanel(
               selectInput(inputId = "destprov",
                           label = "Choose a Province:",
                           choices = c("NS", "NB", "PEI")),
               selectInput(inputId = "destlease",
                           label = "Choose a lease:",
                           choices = NS$Lease_Identifier),
               numericInput(inputId = "destmonitoringnum",
                         label = "Number of Biofouling Monitoring Sites",
                         value = 3),
               numericInput(inputId = "destincidentalnum",
                         label = "Number of Incidental Observation Sites",
                         value = 3),
               checkboxGroupInput(inputId = "destmonitoringsite",
                                  label = "Biofouling Monitoring Sites to Include",
                                  choices = "temp"),
               checkboxGroupInput(inputId = "destincidentalsite",
                                  label = "Incidental Observation Sites to Include",
                                  choices = "temp")
             ),
             
             mainPanel(
               leafletOutput("leafletdest",height=800)
             )
             
           )
  ),
  
  
  
  tabPanel("Summary",
           fluidRow(
             column(4,
                    radioButtons(
                      inputId = "product",
                      label = "Applicable Product Description(s):",
                      choices = product_sp,
                      selected = character(0)
                      )),
             column(4,
                    checkboxGroupInput(
                      inputId = "consideration",
                      label = "Other Considerations",
                      choices = c("Land-based origin",
                                  "Land-based destination")
                    )
             )
           ),
           textAreaInput("response","Suggested Response (Use in combination with Table 2)","No suggested response generated (yet)",width = "100%",resize = "vertical"),
           h3("Supportive Tables"),
           tableOutput("species"),
           tableOutput("mitigation"),
           tableOutput("history")
  ),
  
  
  tabPanel("Plate Monitoring Program Map",
           leafletOutput("leafletmap",height=800)
  ),
  tabPanel("eDNA Map",
           leafletOutput("leafletmap_eDNA",height=800)
  ),
  tabPanel("Public Reports Map",
           leafletOutput("leafletmap_pReport",height=800)
  ),
  
  
  tabPanel("Settings",
           numericInput(inputId = "monitoringyear",
                        label = "Ignore biofouling monitoring records older than:",
                        value = 1900),
           numericInput(inputId = "monitoringstrikes",
                        label = "Number of non-detection records required to reverse a detection (i.e. when is an AIS considered 'failed to establish'):",
                        value = 3),
           numericInput(inputId = "incidentalyear",
                        label = "Ignore incidental observation records older than:",
                        value = 1900),
           numericInput(inputId = "publicyear",
                        label = "Ignore citizen science observation records older than:",
                        value = 1900),
           numericInput(inputId = "metabarcodeyear",
                        label = "Ignore eDNA observation records older than:",
                        value = 2020)
  ),
  
  tabPanel("Read Me",
           includeMarkdown("README.md"))
  
)




#### Server Logic ####
server <- function(input, output, session) {
  
  # reactive functions that filter data
  monitoring_filtered <- reactive({
    #browser()
    monitoring %>% 
      dplyr::filter(Year>=input$monitoringyear) %>% 
      as.data.table() %>% 
      dplyr::select(-geometry) %>% 
      gather(key = "Species", value = "Presence",-StnLocation,-Year) %>%
      group_by(Species,StnLocation) %>% 
      summarize(
        History=case_when(!any(Presence)~paste("Not detected in:",paste(Year,collapse = ", ")),
        length(Year)>input$monitoringstrikes&all(!tail(Presence,input$monitoringstrikes)) ~ paste0("Likely failed to establish, detected in: ",
                                                                                                   paste(Year[Presence],collapse = ", "),
                                                                                                   ", and not detected in: ",
                                                                                                   paste(Year[!Presence],collapse = ", ")),
                          any(!Presence)~paste0("Detected in: ",
                                                paste(Year[Presence],collapse = ", "),
                                                ", and not detected in: ",
                                                paste(Year[!Presence],collapse = ", ")),
                          TRUE~paste("Detected in",paste(Year,collapse = ","))),
        Presence = if_else(all(is.na(Presence)),
                                   FALSE,
                                   any(Presence>0,na.rm = TRUE))
        ) %>% 
      ungroup() %>% 
      mutate(Presence=as.character(Presence)) %>%
      tidyr::pivot_longer(cols = c(Presence,History)) %>%
      tidyr::pivot_wider(id_cols = c(StnLocation,name), names_from = Species, values_from = value) %>%
      inner_join(monitoring_sites,by = "StnLocation")
  })
  
  #filtering metabarcoding data data
  metabarcoding_filtered <- reactive({
    #browser()
    metabarcoding %>% 
      filter(Year>=input$metabarcodeyear) %>% 
      as.data.table() %>% 
      dplyr::select(-geometry) %>%
      gather(key = "Species", value = "Presence",-StnLocation,-Year) %>%
      group_by(Species,StnLocation) %>% 
      summarize(
        History=case_when(!any(Presence)~paste("Not detected in:",paste(Year,collapse = ", ")),
                          any(!Presence)~paste0("Detected in: ",
                                                paste(Year[Presence],collapse = ", "),
                                                ", and not detected in: ",
                                                paste(Year[!Presence],collapse = ", ")),
                          TRUE~paste("Detected in",paste(Year,collapse = ","))),
        Presence = if_else(all(is.na(Presence)),
                           FALSE,
                           any(Presence>0,na.rm = TRUE))
      ) %>% 
      ungroup() %>% 
      mutate(Presence=as.character(Presence)) %>%
      tidyr::pivot_longer(cols = c(Presence,History)) %>%
      tidyr::pivot_wider(id_cols = c(StnLocation,name), names_from = Species, values_from = value) %>%
      inner_join(metabarcoding_sites,by = "StnLocation")
  })
  
#filtering incidental reports by by DFO or made to DFO's email inboxs that had photo evidence
  
  incidental_filtered <- reactive({
    # browser()
    incidental %>% 
      filter(Year>=input$incidentalyear) %>% 
      as.data.table() %>% 
      dplyr::select(-geometry) %>% 
      group_by(Species,StnLocation) %>% 
      summarize(Presence = TRUE,
                # link=if_else(all(is.na(link)),
                #              "NA",
                #              paste0('<a href = "',unique(link),'"> ',Species,' </a>',collapse=" ")),
                prov = paste(unique(prov))) %>% 
      ungroup() %>% 
      # mutate(link=if_else(link=="NA",
      #                     prov,
      #                     link)) %>% 
      left_join(incidental_sites,by = "StnLocation")
    
    
  })
  
#Public report map filtering
  publicdata_filtered <- reactive({
     #browser()
    publicdata %>% 
      filter(Year>=input$publicyear) %>% 
      as.data.table() %>% 
      dplyr::select(-geometry) %>% 
      group_by(Species,StnLocation) %>% 
      summarize(Presence = TRUE,
                prov = paste(unique(prov))) %>% 
      ungroup() %>% 
      left_join(publicdata_sites,by = "StnLocation")
  })
  
  # functions for updating UI
  
  update_orig_sites <- function(lease,prov){
    #browser()

    if(input$origmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                        prov,
                                        monitoring_filtered() %>% filter(name=="History") %>% 
                                        # monitoring_filtered() %>% filter(name=="Presence") %>% 
                                          dplyr::select(-name), 
                                        #%>% mutate(across(2:(ncol(.)-1),as.logical)),
                                        as.numeric(input$origmonitoringnum),
                                        monitoring_dist_orig())
      
      updateCheckboxGroupInput(session,
                               "origmonitoringsite",
                               choices = nearestmonitoring$StnLocation,
                               selected = nearestmonitoring$StnLocation[1])
      
    }
    
    if(input$origincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                        prov,
                                        incidental_filtered(),
                                        as.numeric(input$origincidentalnum),
                                        incidental_dist_orig())
      
      updateCheckboxGroupInput(session,
                               "origincidentalsite",
                               choices = nearestincidental$StnLocation)
    }
    
    
  }
  
  update_dest_sites <- function(lease,prov){
    #browser()
    
    if(input$destmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                        prov,
                                        monitoring_filtered() %>% filter(name=="History") %>% 
                                        # monitoring_filtered() %>% filter(name=="Presence") %>% 
                                          dplyr::select(-name), 
                                        #%>% mutate(across(2:(ncol(.)-1),as.logical)),
                                        as.numeric(input$destmonitoringnum),
                                        monitoring_dist_dest())
      
      updateCheckboxGroupInput(session,
                               "destmonitoringsite",
                               choices = nearestmonitoring$StnLocation,
                               selected = nearestmonitoring$StnLocation[1])
      
    }
    
    if(input$destincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                        prov,
                                        incidental_filtered(),
                                        as.numeric(input$destincidentalnum),
                                        incidental_dist_dest())
      
      updateCheckboxGroupInput(session,
                               "destincidentalsite",
                               choices = nearestincidental$StnLocation)
    }
    
  }
  
  
  # Observe events to trigger updates
  
  # Origin - choose a province 
  observeEvent(input$origprov, {
    #get provincial leases
    prov <- origprovInput()
    
    # update lease options
    freezeReactiveValue(input,"map")
    updateSelectInput(session,
                      "origlease",
                      choices = prov$Lease_Identifier,
                      selected = prov$Lease_Identifier[1])
    
    # get correct lease
    lease <- origprovInput() %>%
      filter(Lease_Identifier==prov$Lease_Identifier[1])

    update_orig_sites(lease,prov)
  })
  
  # Origin - Choose a lease
  observeEvent(input$origlease, {
     update_orig_sites(lease=origprovInput() %>%
                        filter(Lease_Identifier==input$origlease),
                      prov=origprovInput())
  })
  
  # Origin - # of monitoring sites
  observeEvent(input$origmonitoringnum, {
    update_orig_sites(lease=origprovInput() %>%
                        filter(Lease_Identifier==input$origlease),
                      prov=origprovInput())
  })
  
  # Origin - # of incidental sites
  observeEvent(input$origincidentalnum, {
    update_orig_sites(lease=origprovInput() %>%
                        filter(Lease_Identifier==input$origlease),
                      prov=origprovInput())
  })
  
  # Destination - choose a province 
  observeEvent(input$destprov, {
    #get provincial leases
    prov <- destprovInput()
    
    # update lease options
    updateSelectInput(session,
                      "destlease",
                      choices = prov$Lease_Identifier,
                      selected = prov$Lease_Identifier[1])
    
    # get correct lease
    lease <- destprovInput() %>%
      filter(Lease_Identifier==prov$Lease_Identifier[1])
    
    update_dest_sites(lease,prov)
  })
  
  # Destination - Choose a lease
  observeEvent(input$destlease, {
    update_dest_sites(lease=destprovInput() %>%
                        filter(Lease_Identifier==input$destlease),
                      prov=destprovInput())
  })
  
  # Destination - # of monitoring sites
  observeEvent(input$destmonitoringnum, {
    update_dest_sites(lease=destprovInput() %>%
                        filter(Lease_Identifier==input$destlease),
                      prov=destprovInput())
  })
  
  # Destination - # of incidental sites
  observeEvent(input$destincidentalnum, {
    update_dest_sites(lease=destprovInput() %>%
                        filter(Lease_Identifier==input$destlease),
                      prov=destprovInput())
  })
 
  
  # reactive switches that get the correct leases and distance matrices
  origprovInput <- reactive({
    switch(input$origprov,
           "NS" = NS,
           "NB" = NB,
           "PEI" = PEI)
  })
  
  destprovInput <- reactive({
    switch(input$destprov,
           "NS" = NS,
           "NB" = NB,
           "PEI" = PEI)
  })
  
  monitoring_dist_orig <- reactive({
    switch(input$origprov,
           "NS" = ns_monitoring_dist,
           "NB" = nb_monitoring_dist,
           "PEI" = pei_monitoring_dist)
  })
  
  incidental_dist_orig <- reactive({
    switch(input$origprov,
           "NS" = ns_incidental_dist,
           "NB" = nb_incidental_dist,
           "PEI" = pei_incidental_dist)
  })
  

  monitoring_dist_dest <- reactive({
    switch(input$destprov,
           "NS" = ns_monitoring_dist,
           "NB" = nb_monitoring_dist,
           "PEI" = pei_monitoring_dist)
  })
  
  incidental_dist_dest <- reactive({
    switch(input$destprov,
           "NS" = ns_incidental_dist,
           "NB" = nb_incidental_dist,
           "PEI" = pei_incidental_dist)
  })
  
  #### Leaflet maps ####
  
  # full interactive map
  output$leafletmap <- renderLeaflet({
   # browser()
    
    all_leases <- rbind(dplyr::select(NS,Lease_Identifier),
                        dplyr::select(NB,Lease_Identifier),
                        dplyr::select(PEI,Lease_Identifier))
    basemap(leases=all_leases,
            incidentals=incidental_filtered(),
            monitoring=monitoring_filtered() %>% filter(name=="Presence") %>% 
              dplyr::select(-name) %>% 
              mutate(across(2:(ncol(.)-1),as.logical)),
            monitoringsp=AIS$R_Name)
  })
  
  #eDNA map
  output$leafletmap_eDNA <- renderLeaflet({
    # browser()
    
    all_leases <- rbind(dplyr::select(NS,Lease_Identifier),
                        dplyr::select(NB,Lease_Identifier),
                        dplyr::select(PEI,Lease_Identifier))
    basemap_eDNA(leases=all_leases,
                 metabarcoding=metabarcoding_filtered() %>% filter(name=="Presence") %>% 
                   dplyr::select(-name) %>% 
                   mutate(across(2:(ncol(.)-1),as.logical)),
            metabarcodingsp=AIS$R_Name)
  })
  
  #Public Reports map
  output$leafletmap_pReport<- renderLeaflet({
     #browser()
    
    all_leases <- rbind(dplyr::select(NS,Lease_Identifier),
                        dplyr::select(NB,Lease_Identifier),
                        dplyr::select(PEI,Lease_Identifier))
    basemap_pReport(leases=all_leases,
                 publicdata=publicdata_filtered(),
                 publicdatasp=AIS$R_Name)
  })


  # map for origin tab
  output$leafletorig <- renderLeaflet({
     #browser()
    print("making map")
    
    prov <- origprovInput()
   
    
    if(!input$origlease %in% prov$Lease_Identifier){
      lease <- prov %>%
        filter(Lease_Identifier==prov$Lease_Identifier[1])
    } else {
      lease <- origprovInput() %>%
        filter(Lease_Identifier==input$origlease)
    }
    

    if(input$origmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                       prov,
                                       monitoring_filtered() %>% filter(name=="Presence") %>% 
                                         dplyr::select(-name) %>% mutate(across(2:(ncol(.)-1),as.logical)) %>% mutate(across(2:(ncol(.)-1),as.logical)),
                                       as.numeric(input$origmonitoringnum),
                                       monitoring_dist_orig())
    }

    if(input$origincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                       prov,
                                       incidental_filtered(),
                                       as.numeric(input$origincidentalnum),
                                       incidental_dist_orig())
    }

    if(input$origincidentalnum!="" & input$origmonitoringnum!=""){
      basemap(leases=lease,
              incidentals=nearestincidental,
              monitoring=nearestmonitoring,
              monitoringsp=AIS$R_Name)
    }
    
   
  })

  # map for destination tab
  output$leafletdest <- renderLeaflet({

    prov <- destprovInput()
    
    if(!input$destlease %in% prov$Lease_Identifier){
      lease <- prov %>%
        filter(Lease_Identifier==prov$Lease_Identifier[1])
    } else {
      lease <- destprovInput() %>%
        filter(Lease_Identifier==input$destlease)
    }
    
    
    if(input$destmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                        prov,
                                        monitoring_filtered() %>% filter(name=="Presence") %>% 
                                          dplyr::select(-name) %>% mutate(across(2:(ncol(.)-1),as.logical)),
                                        as.numeric(input$destmonitoringnum),
                                        monitoring_dist_dest())
    }
    
    if(input$destincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                        prov,
                                        incidental_filtered(),
                                        as.numeric(input$destincidentalnum),
                                        incidental_dist_dest())
    }
    
    if(input$destincidentalnum!="" & input$destmonitoringnum!=""){
      basemap(leases=lease,
              incidentals=nearestincidental,
              monitoring=nearestmonitoring,
              monitoringsp=AIS$R_Name)
    }

 
  })
  
  
  #### summary ####
  
  summaryValues <- reactive({
    print("Generating Summary")
    #browser()
    
    # prevent summary/suggested responses if inadequate sites selected
    if(is.null(input$origmonitoringsite)&
       is.null(input$origincidentalsite)&
       is.null(input$destmonitoringsite)&
       is.null(input$destincidentalsite)
       ){
      summary <- data.frame(
        Site = "No biofouling monitoring sites or incidental observation sites were selected on the Origin or Destination tab"
      )
    } else if(is.null(input$origmonitoringsite)&
              is.null(input$origincidentalsite)
              ){
      summary <- data.frame(
        Site = "No biofouling monitoring sites or incidental observation sites were selected on the Origin tab"
      )
    }else if(is.null(input$destmonitoringsite)&
             is.null(input$destincidentalsite)
             ){
      summary <- data.frame(
        Site = "No biofouling monitoring sites or incidental observation sites were selected on the Destination tab"
      )
    } else {
      # get monitoring data for origin
      if(!is.null(input$origmonitoringsite)){
        origin <- monitoring_filtered() %>% filter(name=="Presence") %>% 
          dplyr::select(-name) %>% mutate(across(2:(ncol(.)-1),as.logical)) %>% 
          filter(gsub("\\s*\\([^\\)]+\\)","",StnLocation) %in% gsub("\\s*\\([^\\)]+\\)","",input$origmonitoringsite)) %>% 
          data.frame() %>% 
          gather(key="Species",value="Presence",-StnLocation) %>% 
          #mutate(Species=gsub("\\."," ",Species)) %>%
          mutate(Species=gsub("\\.","_",Species)) %>% 
          filter(Species %in% AIS$R_Name) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      }else {
        origin <- data.frame("Species"=character(0),"Presence"=logical(0))
      }
      
      # get monitoring data for destination
      if(!is.null(input$destmonitoringsite)){  
        destination <- monitoring_filtered() %>% filter(name=="Presence") %>% 
          dplyr::select(-name) %>% mutate(across(2:(ncol(.)-1),as.logical)) %>% 
          filter(gsub("\\s*\\([^\\)]+\\)","",StnLocation) %in% gsub("\\s*\\([^\\)]+\\)","",input$destmonitoringsite)) %>% 
          data.frame() %>% 
          gather(key="Species",value="Presence",-StnLocation) %>% 
          #mutate(Species=gsub("\\."," ",Species)) %>% 
          mutate(Species=gsub("\\.","_",Species)) %>% 
          filter(Species %in% AIS$R_Name) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      } else {
        destination <- data.frame("Species"=character(0),"Presence"=logical(0))
      }
      
      
      # get incidental data for origin
      if(!is.null(input$origincidentalsite)){
        origin <- origin %>%
          bind_rows(incidental_filtered() %>% 
                      filter(gsub("\\s*\\([^\\)]+\\)","",paste(Species,StnLocation)) %in% gsub("\\s*\\([^\\)]+\\)","",input$origincidentalsite)) %>% 
                      data.frame() %>% 
                      #mutate(Species=gsub("\\."," ",Species)) %>% 
                      mutate(Species=gsub("\\.","_",Species)) %>% 
                      filter(Species %in% AIS$R_Name) %>% 
                      group_by(Species) %>% 
                      summarize(Presence = any(as.logical(Presence)))) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
        
      }
      
      
      # get incidental data for destination
      if(!is.null(input$destincidentalsite)){
        destination <- destination %>%
          bind_rows(incidental_filtered() %>% 
                      filter(gsub("\\s*\\([^\\)]+\\)","",paste(Species,StnLocation)) %in% gsub("\\s*\\([^\\)]+\\)","",input$destincidentalsite)) %>% 
                      data.frame() %>% 
                      #mutate(Species=gsub("\\."," ",Species)) %>% erased _ between species names
                      mutate(Species=gsub("\\.","_",Species)) %>% 
                      filter(Species %in% AIS$R_Name) %>% 
                      group_by(Species) %>% 
                      summarize(Presence = any(as.logical(Presence)))) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      }
      
      

      summary <- full_join(origin,destination,by="Species",suffix=c(" (Origin)"," (Destination)")) %>% 
        replace_na(list(Species=NA,"Presence (Origin)"=FALSE,"Presence (Destination)"=FALSE)) %>% 
        mutate(Species = str_replace(Species,"_"," "))
    }

    summary
  })
  
  output$species <- renderTable({
    summaryspecies <- summaryValues() 
    if(ncol(summaryspecies)>1){
      summaryspecies <- summaryspecies %>% 
        mutate(`Risk Assessment` = if_else(`Presence (Origin)`,
                                    if_else(`Presence (Destination)`,"Low risk with mitigation","High risk"),
                                    "")
               )
    }
    summaryspecies
  },
  caption.placement="top",
  caption="Table 1: Species detections and consequential risk assessment due to presence/absence")
  
  output$mitigation <- renderTable({
    summarymitigation <- summaryValues()
     #browser()
     
    if(ncol(summarymitigation)>1){
      if(length(input$product)>0){
        summarymitigation <- summarymitigation %>% 
          mutate(Species=gsub(" ", "_", Species))%>%
          mutate(`Risk Assessment` = if_else(`Presence (Origin)`,
                                             if_else(`Presence (Destination)`,"Low risk with mitigation","High risk"),
                                             "")) %>%
          filter(`Risk Assessment`!="") %>%
          left_join(sp_treatments,by=c("Species"="R_Name")) %>%
          dplyr::select(-`Presence (Origin)`, -`Presence (Destination)`
          #Species, Common_Name, Product_treated,`Risk Assessment`,Treatment_proposed
          ) %>%
          dplyr::filter(Product_treated %in% input$product|is.na(Product_treated))
        
        # get common names from AIS if not available in sp_treatments
        if(any(is.na(summarymitigation$Common_Name))){
          summarymitigation <- summarymitigation %>% 
            dplyr::select(-Common_Name) %>% 
            left_join(AIS,by=c("Species"="R_Name"))
        }
        
      } else {
        summarymitigation <- data.frame(
          Site = "An applicable product description must be selected"
        )
      }
    }
    
    if(nrow(summarymitigation)==0){
      if(input$product %in% sp_mitigation$Common_Name){
        summarymitigation <- data.frame(
          Site = sp_mitigation %>% 
            filter(Common_Name==input$product) %>% 
            dplyr::select(Mitigation) %>% 
            as.character()
        )
      } else{
        summarymitigation <- data.frame(
          Site = "No mitigation treatments exist for this combination of species and product"
        )
      }
      
    }
    
    if(!is.null(input$consideration)){
      if(all(input$consideration==c("Land-based origin","Land-based destination"))){
        summarymitigation <- data.frame(
          Site = "The risk for AIS/FFHPP is considered low with high certainty because the origin and destination sites are land-based. However, precautions should be taken to ensure no escapes or accidental introductions occur. It is recommended that all equipment used to capture, rear, hold, or transport the species  (and associated water used in transfer) should be cleaned, drained, dried, and decontaminated prior to use elsewhere. Approval of introduction & transfer requests do not preclude individuals from their responsibilities to ensure no introductions of non-indigenous species (including species that are common to Nova Scotia but from a different genetic strain) to areas they are not indigenous to as states under the Aquatic Invasive Species Regulations s. 10. ")
      } else if(input$consideration=="Land-based origin"){
        if(!input$product %in% sp_mitigation$Common_Name){
          summarymitigation <- data.frame(
            Site = "The risk for AIS/FFHPP is considered low with high certainty because the origin site is land-based")
        }
        
      } else if(input$consideration=="Land-based destination"){
        summarymitigation <- data.frame(
          Site = "The risk for AIS/FFHPP is considered low with high certainty because the destination site is land-based. However, precautions should be taken to ensure no escapes or accidental introductions occur. It is recommended that all equipment used to capture, rear, hold, or transport the species  (and associated water used in transfer) should be cleaned, drained, dried, and decontaminated prior to use elsewhere. Approval of introduction & transfer requests do not preclude individuals from their responsibilities to ensure no introductions of non-indigenous species (including species that are common to Nova Scotia but from a different genetic strain) to areas they are not indigenous to as states under the Aquatic Invasive Species Regulations s. 10. ")
      } else {
        summarymitigation <- data.frame(
          Site = "This (combination of) consideration(s) is not resolved in this app")
      }
    }
    
    updateTextInput(inputId = "response",value = create_response(summitigation = summarymitigation, species = input$product))
    return(summarymitigation %>% 
             setNames(gsub("_"," ",names(.)))) 
    
  },
  caption.placement="top",
  caption="Table 2: Applicable 'fellow-traveller' mitigation strategies. Responses based on Masse-Beaulne et al. (2025). AIS mitigrations were selected based on high to moderate certainty of effectiveness, except for green crab that had limit options.")
  
  output$history <- renderTable({
    # browser()
    origin <- monitoring_filtered() %>%
      filter(name=="History"&gsub("\\s*\\([^\\)]+\\)","",StnLocation) %in% gsub("\\s*\\([^\\)]+\\)","",input$origmonitoringsite)) %>%
      data.frame() %>%
      dplyr::select(-StnLocation) %>% 
      gather(key="Species",value="History") %>%
     # mutate(Species=gsub("\\."," ",Species)) %>%
      mutate(Species=gsub("\\.","_",Species)) %>%
      filter(Species %in% AIS$R_Name)
    destination <- monitoring_filtered() %>%
      filter(name=="History"&gsub("\\s*\\([^\\)]+\\)","",StnLocation) %in% gsub("\\s*\\([^\\)]+\\)","",input$destmonitoringsite)) %>%
      data.frame()%>%
      dplyr::select(-StnLocation) %>% 
      gather(key="Species",value="History") %>%
     # mutate(Species=gsub("\\."," ",Species)) %>%
      mutate(Species=gsub("\\.","_",Species)) %>%
      filter(Species %in% AIS$R_Name)
    full_join(origin,destination,by="Species", suffix=c(" (Origin)"," (Destination)"))
  },
  caption.placement="top",
  caption="Table 3: Invasion history")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

