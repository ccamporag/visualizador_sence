library(shiny)
library(shinydashboard)
#library(DBI)
#library(pool)
#library(glue)
#library(RMariaDB)
library(dplyr)
library(shinyjs)
library(readxl)
library(dplyr)
library(leaflet)
library(leaflet.providers)
library(colortools)
library(shinyWidgets)
#install.packages("dashboardthemes")
library(dashboardthemes)
library(shinythemes)
library(shinymanager)

setwd("D:/OneDrive/NuevaCarpeta/InData/sence/visualizador_sence/sence")
base <- read_excel("base_coord_sence.xlsx")

base 

col1 <- "#B22231"
col2 <- "#D48C93"
col3 <- "#595959"
col4 <- "#AFABAB"
col5 <- "#ECECEC"


### creating custom theme object
customTheme <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "black"
    ,primaryFontColor = col3
    ,infoFontColor = col1
    ,successFontColor = col1
    ,warningFontColor = col2
    ,dangerFontColor = col3
    ,bodyBackColor = "white"
    
    ### header
    ,logoBackColor = col5
    
    ,headerButtonBackColor = col5
    ,headerButtonIconColor = col1
    ,headerButtonBackColorHover = col4
    ,headerButtonIconColorHover = col1
    
    ,headerBackColor = col5
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    ,sidebarBackColor = col5
    #     cssGradientThreeColors(
    #     direction = "down"
    #     ,colorStart = "rgb(20,97,117)"
    #     ,colorMiddle = "rgb(56,161,187)"
    #     ,colorEnd = "rgb(3,22,56)"
    #     ,colorStartPos = 0
    #     ,colorMiddlePos = 50
    #     ,colorEndPos = 100
    # )
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = col3
    
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
    
    ,sidebarTabTextColor = col3
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = col5
    ,sidebarTabBorderWidth = 1
    
    ,sidebarTabBackColorSelected = col3
    #     cssGradientThreeColors(
    #     direction = "right"
    #     ,colorStart = "rgba(44,222,235,1)"
    #     ,colorMiddle = "rgba(44,222,235,1)"
    #     ,colorEnd = "rgba(0,255,213,1)"
    #     ,colorStartPos = 0
    #     ,colorMiddlePos = 30
    #     ,colorEndPos = 100
    # )
    ,sidebarTabTextColorSelected = col1
    ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
    
    ,sidebarTabBackColorHover = col4
    #     cssGradientThreeColors(
    #     direction = "right"
    #     ,colorStart = "rgba(44,222,235,1)"
    #     ,colorMiddle = "rgba(44,222,235,1)"
    #     ,colorEnd = "rgba(0,255,213,1)"
    #     ,colorStartPos = 0
    #     ,colorMiddlePos = 30
    #     ,colorEndPos = 100
    # )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = col1
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(210,214,220)"
    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"
    
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgba(44,222,235,1)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)


credentials <- data.frame(
    user = c("ccampora", "cyañez","esanchez","admin"), # mandatory
    password = c("indata2020", "indata2020","indata2020","indata2020"), # mandatory
    start = c("2019-04-15"), # optinal (all others)
    expire = c(NA, "3019-12-31"),
    admin = c(FALSE, TRUE),
    comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
)



header <- function(){
    
    dashboardHeader(title = span("SENCE", style = "color:#B22231; font-size: 20px"))
    
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
        
        
        
        
            customTheme



# 
#         shinyDashboardThemes(
#             theme = customTheme
#         )


        
#         tags$head(tags$style(HTML('
#                           /* logo */
#                           .skin-blue .main-header .logo {
#                           background-color: rgb(255,255,255); color:        rgb(0,144,197);
#                           font-weight: bold;font-size: 24px;text-align: center;
#                           height: 60px;
#                           }
# 
#                           /* logo when hovered */
# 
#                           .skin-blue .main-header .logo:hover {
#                           background-color: rgb(255,255,255);
#                           }
# 
# 
#                           /* navbar (rest of the header) */
#                           .skin-blue .main-header .navbar {
#                           background-color: rgb(255,255,255);
#                           max-height: 60px;
# 
#                           }
# 
#                           /* main sidebar */
#                           .skin-blue .main-sidebar {
#                           background-color: #012842;;
#                           }
# 
#                           /* main body */
#                           .skin-blue .main-body {
#                           background-color: rgb(255,255,255);
#                           backgroud-color: red;
#                           }
# 
#                           /* active selected tab in the sidebarmenu */
#                           .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
#                           background-color: steelblue;
#                           color: rgb(255,255,255);font-weight: bold;font-size: 18px;
#                           }
# 
#                           /* other links in the sidebarmenu */
#                           .skin-blue .main-sidebar .sidebar .sidebar-menu a{
#                           background-color: #012842;
#                           color: rgb(255,255,255);font-weight: bold;
#                           }
# 
#                           /* other links in the sidebarmenu when hovered */
#                           .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
#                           background-color: rgb(232,245,251);color: rgb(0,144,197);font-weight: bold;
#                           }
# 
#                           /* toggle button color  */
#                           .skin-blue .main-header .navbar .sidebar-toggle{
#                           background-color: rgb(255,255,255);color:#012842;
#                           height: 60px;
#                           }
#                           
#                           .left-side, .main-sidebar {padding-top: 60px;}
# 
#                           /* toggle button when hovered  */
#                           .skin-blue .main-header .navbar .sidebar-toggle:hover{
#                           background-color: rgb(0,144,197);color:rgb(255,255,255);
#                           }
# 
# #                           ')))
#      
       ,
         tabItem(tabName = "mapa",
                 
                 fluidRow(
                     column(8,
                            
                            leafletOutput("mapa", height = '800px')  
                         
                     ),
                     column(4,
                            uiOutput("ficha"), tags$style(ttpe="text/css", "#ficha {font-size: 15px; line-height: 1.5;}")
                     )
                     
                     
                     )
                
                
                
                )
           
    )
    
}

ui <- dashboardPage(title = "In-Data",
    header = header(),
    sidebar = sidebar(),
    body = body()
    
)


ui <- navbarPage(windowTitle = "Proyecto Sence", title =  div( id="titulo_app",tags$img(src="logo_indata_png.png", height = 30, width = 90)),theme = shinytheme("flatly"),
                  
                  tabPanel(title = "Ficha",
                           
                           fluidPage(
                               
                               fluidRow(
                                   column(8,
                                          
                                          leafletOutput("mapa", height = '800px')  
                                          
                                   ),
                                   column(4,
                                          uiOutput("ficha"), tags$style(ttpe="text/css", "#ficha {font-size: 15px; line-height: 1.5;}")
                                   )
                                   
                                   
                               )
                               
                           )
                           
                           )
                  

                  
                  )


ui <- secure_app(ui,
                 theme = shinythemes::shinytheme("flatly"),
                 background = "#012842"
                 
                 # "linear-gradient(rgba(0, 0, 255, 0.5),
                 #       rgba(255, 255, 0, 0.5)),
                 #       url('https://www.r-project.org/logo/Rlogo.png');"
                 
                 ,      
                 tags_top =
                     tags$div(
                         tags$h4("Bienvenido", style = "align:center"),
                         tags$img(
                             src = "logo_indata_png.png", width = 130, height = 55
                         )
                     )
                 #,
                 # add information on bottom ?
                 # tags_bottom = tags$div(
                 #     tags$p(
                 #         "Necesitas Acceso? envíanos un  ",
                 #         tags$a(
                 #             href = "mailto:edificacion@in-data.cl?Subject=Acceso Plataforma Baudata",
                 #             target="_top", "mensaje"
                 #         )
                 #     )
                 # ) 
                 
                 )

shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "",
    "Username:" = "Usuario:",
    "Password:" = "Contraseña:"
)



server <- function(input,output,session){
    
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    

    
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
            
            addTiles(group = "Mapbox",
                
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                
            ) %>%
            
            addTiles(group = "Satélite",
                     attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
                     urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
                        
                     ) %>% 
            
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            
            setView(lng = -70.6341 , lat = -33.4369, zoom = 4) %>%  
            addLayersControl(
                baseGroups = c("Mapbox", "Toner", "Toner Lite","Satélite"),
                
                options = layersControlOptions(collapsed = FALSE)
            )
            
            
            # addProviderTiles("Jawg.Streets", 
            #                  options = providerTileOptions(accessToken = 'fPW0ziIhPzyRRFHaFYIENuKiXD2HJLFIyVS9xc7ztf5boNJSJK19Lrxmn63bJTkP')) %>% 
            

        
        
        
    })
    
    
    
    
    observe({
        

        leafletProxy("mapa") %>%           
            clearMarkers() %>% 
            clearControls() %>%
           
            addCircleMarkers(
                lng = base$lng, 
                lat = base$lat, 
                layerId = base$id,
                popup = base$`Nombre Institución`,  
                fillColor = pal(base$Tipo),
                fillOpacity = 1,
                color = pal(base$Tipo)
            )  %>%
            addLegend("bottomleft",pal = pal, values = base$Tipo, opacity = 1,title = "Tipo")
            
    })
    
    
    observeEvent(input$mapa_marker_click, { 
    

        
        output$ficha <- renderUI({

            rv <- base %>% filter(id == input$mapa_marker_click$id)
           
            div(id="ficha",
                div(h2(toupper(rv$`Nombre Institución`))),
                div(rv$Tipo),
                div(h4("Descripción"),ifelse(is.na(rv$`Descripción Tipo`),"",rv$`Descripción Tipo`)),
                div(h4("Objetivo: "),rv$`Objetivo (población)`),
                div(h4("Características Servicios: "),rv$`Características Servicios`),
                
                
                
                div(h4("Servicios: "),paste0(ifelse(is.na(rv$`Servicio 1`),"",rv$`Servicio 1`),
                                             ifelse(is.na(rv$`Servicio 2`),"",paste0(" / ",rv$`Servicio 2`)),
                                             ifelse(is.na(rv$`Servicio 3`),"",paste0(" / ",rv$`Servicio 3`)))),
                div(h4("Perfil: "),rv$Perfil),
                div(h4("Pagina web: "), a(rv$`Página Web`, href=rv$`Página Web`, target="_blank") ),
                div(h4("Cobertura"), rv$Cobertura),
                div(h4("Dirección: "),paste0(rv$`Dirección`,", ",rv$Comuna)),
                div(h4("Teléfono: "),rv$Telefono),
                div(h4("email: "),rv$Mail)
                #,div(h4("Fuente: "),rv$`Fuente Info`)
                
                # ,
                # 
                # div(h3("Observaciones: "),rv$Observaciones),
                # div(h3("Apreciación: "),rv$`Apreciación`),
                # div(h3("id: ",rv$id))
                
                
                
                )
            
           
            
        })
        
        
    })
    

    
}

shinyApp(ui,server)

