library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(glue)
library(RMariaDB)
library(dplyr)
library(shinyjs)
library(readxl)
library(dplyr)
library(leaflet)
library(leaflet.providers)
library(colortools)



base <- read_excel("base_coord_sence.xlsx")

base 





header <- function(){
    
    dashboardHeader(title = div( id="titulo_app",tags$img(src="logo_ctes.png", height = 45, width = 150)))
    
}

sidebar <- function(){
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("mapa")
            
        )
 
        
    )
    
}

body <- function(){
    
    dashboardBody(
        
        useShinyjs(),
        
        
        
        tags$head(tags$style(HTML('
                          /* logo */
                          .skin-blue .main-header .logo {
                          background-color: rgb(255,255,255); color:        rgb(0,144,197);
                          font-weight: bold;font-size: 24px;text-align: center;
                          height: 60px;
                          }

                          /* logo when hovered */

                          .skin-blue .main-header .logo:hover {
                          background-color: rgb(255,255,255);
                          }


                          /* navbar (rest of the header) */
                          .skin-blue .main-header .navbar {
                          background-color: rgb(255,255,255);
                          max-height: 60px;

                          }

                          /* main sidebar */
                          .skin-blue .main-sidebar {
                          background-color: #012842;;
                          }

                          # /* main body */
                          # .skin-blue .main-body {
                          # background-color: rgb(255,255,255);
                          # }

                          /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: steelblue;
                          color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                          }

                          /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: #012842;
                          color: rgb(255,255,255);font-weight: bold;
                          }

                          /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: rgb(232,245,251);color: rgb(0,144,197);font-weight: bold;
                          }

                          /* toggle button color  */
                          .skin-blue .main-header .navbar .sidebar-toggle{
                          background-color: rgb(255,255,255);color:#012842;
                          height: 60px;
                          }
                          
                          .left-side, .main-sidebar {padding-top: 60px;}

                          /* toggle button when hovered  */
                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: rgb(0,144,197);color:rgb(255,255,255);
                          }

#                           ')))
     
       ,
         tabItem(tabName = "mapa",
                
                
                leafletOutput("mapa")
                
                
                )
           
    )
    
}

ui <- dashboardPage(
    header = header(),
    sidebar = sidebar(),
    body = body()
    
)

server <- function(input,output,session){

    
    pal <- colorFactor(
        # palette = c("black","grey","blue","darkblue","orange","darkorange","yellow","red","darkred","green","darkgreen","lightblue"),
        palette = wheel("steelblue", num = 14), 
        levels = base$Tipo %>% unique(),
        domain = base$Tipo
    )
        
    output$mapa <- renderLeaflet({
        

        
        leaflet(base) %>%   
       #    addProviderTiles("Jawg.Streets") %>%
           # addTiles(
           #     options = tileOptions(accessToken='fPW0ziIhPzyRRFHaFYIENuKiXD2HJLFIyVS9xc7ztf5boNJSJK19Lrxmn63bJTkP',subdomains= 'abcd'),
           #     urlTemplate = 'https://{s}.tile.jawg.io/jawg-streets/{z}/{x}/{y}{r}.png?access-token=${fPW0ziIhPzyRRFHaFYIENuKiXD2HJLFIyVS9xc7ztf5boNJSJK19Lrxmn63bJTkP}',
           #   #  options = list(accessToken = 'fPW0ziIhPzyRRFHaFYIENuKiXD2HJLFIyVS9xc7ztf5boNJSJK19Lrxmn63bJTkP'),
           #     attribution = '<a href="http://jawg.io" title="Tiles Courtesy of Jawg Maps" target="_blank">&copy; <b>Jawg</b>Maps</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
           # 
           # ) %>%
            
            addTiles(
                
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                
            ) %>% 

            
            
            # addProviderTiles("Jawg.Streets", 
            #                  options = providerTileOptions(accessToken = 'fPW0ziIhPzyRRFHaFYIENuKiXD2HJLFIyVS9xc7ztf5boNJSJK19Lrxmn63bJTkP')) %>% 
            
            addCircleMarkers(
                lng = ~lng, 
                lat = ~lat, 
                layerId = ~id,
                popup = ~`Nombre Institución`,  
                fillColor = pal(base$Tipo),
                fillOpacity = 1,
                color = pal(base$Tipo)
            )  %>%
            addLegend("bottomleft",pal = pal, values = base$Tipo, opacity = 1,title = "Tipo")
        
        
        
    })
    
}

shinyApp(ui,server)

