

#load required libraries
library(shiny)
library(leaflet)
library(lwgeom)
library(sf)
library(tidyverse)
library(data.table)

#Load data from Prepared RDS files
streams=readRDS("data/streams.rds")
wat=readRDS("data/wat.rds") %>%
  filter(!GNIS_NM_1 %like% 'Lake')
lakes=readRDS("data/lakes.rds") 
##

# User Interface two maps with action button
ui <- fluidPage(

    # Application title
    titlePanel("Freshwater Atlas Assessment Watersheds"),

    # Map that is of the whole Ok Basin
   
        sidebarPanel(
            leafletOutput(outputId = "map1") ,
            actionButton( inputId = "clearSelection"
                          , icon = icon( name = "eraser")
                          , label = "Clear Selected Watershed")
      
        )
        ,

        # Map that reactively generates based on watershed selection
        mainPanel(
        leafletOutput(outputId = "map2")
       
        
        )

)


# Server
server <- function(input, output) {

    output$map1 <- renderLeaflet({
       
        leaflet()%>%
            addPolygons( data=wat,weight=4,color="#266687",layerId = wat$WTRSHD_FID) %>%  addTiles() 
    })
    
    #$Click Event
    observeEvent(input$map1_shape_click, {
        click <- input$map1_shape_click 
       wat.sel= wat %>% filter(WTRSHD_FID == click$id)
       
      #if watershed is clicked below code continues
        if( is.null( click$id ) ){
         
           req( click$id )
           
       } else if( !click$id %in% wat.sel$id ){
           
           # call the leaflet proxy
           leaflet::leafletProxy( mapId = "map1" ) %>%
              #highlight selected watershed in map1
               addPolylines( data = wat.sel
                             , layerId = wat.sel$id 
                             , color = "#6cb5bc"
                             , weight = 5
                             , opacity = 1
               ) 
       } 
       output$map2 <- renderLeaflet({
        
         # getting in HTML for title
           
           tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 24px;
  }
"))
           
           title <- tags$div(
               tag.map.title, HTML(ifelse(!is.na(wat.sel$GNIS_NM_1),wat.sel$GNIS_NM_1,"Unnamed Watershed"))
           )  
           
           
           #add stream
           cols <- colorFactor("Blues", domain = streams$STREAM_ORDER,reverse = TRUE)
           sel.st= st_intersection(streams, wat.sel)
           ##get rid of stream segments length 0 m
           sel.st$len=as.numeric(st_length(sel.st))
           sel.st= sel.st %>% filter(len !=  0)
           
           
           ##function to fix geometry collection
           convert_co=function(x,type_target){
             sel.st=x
             gec=sel.st %>% filter(geom == "GEOMETRYCOLLECTION") %>% 
               st_collection_extract(type_target)
             sel.st=rbind(sel.st %>% filter(geom != "GEOMETRYCOLLECTION"),gec)
             return(sel.st)
             
           }
      
           
           ## get rid of geometry collection issue- if it exists for streams
          sel.st= sel.st %>%  mutate(geom=st_geometry_type(sel.st))
          if(length(grep("GEOMETRYCOLLECTION",sel.st$geom))>0){
            sel.st=convert_co(sel.st,"LINESTRING")
         }
        
          #add Lakes in watershed
           lk.sel= st_intersection(lakes, wat.sel)
           lk.sel= lk.sel %>%  mutate(geom=st_geometry_type(lk.sel)) %>% filter(geom != "POINT")
           if(length(grep("GEOMETRYCOLLECTION",lk.sel$geom))>0){
             lk.sel=convert_co(lk.sel,"POLYGON")
           }
           ## compute area to conditionally label lakes based on size  
           lk.sel=lk.sel %>% mutate(shp_area=as.numeric(st_area(lk.sel))) %>% mutate(lake_label=ifelse(shp_area>80000,GNIS_NAME_1,NA))
         
           # generate leaflet map
           leaflet(wat.sel)%>%
               addPolygons( data=wat.sel,weight=4,color="#266687",fillOpacity = 1,fillColor = "#a9a9a9") %>% 
             addPolylines(data=sel.st, color=~cols(STREAM_ORDER),weight=~STREAM_ORDER, group="streams", label= ~GNIS_NAME) %>%
             addPolygons( data=lk.sel,weight=0,fillColor="#3CA7DE", fill=TRUE,label= ~lake_label, fillOpacity = 1,
                          labelOptions = labelOptions(noHide = T,textOnly = TRUE))  %>% 
               addControl(title, position = "bottomleft", className="map-title") %>%
             addLegend(data=sel.st,  position = "bottomright", pal = cols, values = ~STREAM_ORDER,  title = "Stream Order", opacity = 1, group="streams" )
           
           #create table of streams in watershed- should this go here
     
          
       }) 
       ##end Click event
      
   
     
       #When action button is clicked
       observeEvent( input$clearSelection, {
           
           # recreate big map
           output$map1 <- leaflet::renderLeaflet({
               
             
               click$id <- NULL
               
               leaflet()%>%
                   addPolygons( data=wat,weight=4,color="#266687",layerId = wat$WTRSHD_FID) %>%  addTiles() 
              
              
               
           }) 
           
        
      
           
       }) # end of when Action Button is clicked
       
})
    
 
    
    
 
   
}
# Run the application 
shinyApp(ui = ui, server = server)

