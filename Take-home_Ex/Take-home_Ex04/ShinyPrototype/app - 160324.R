#
# https://isss608-vaa-suanern.shinyapps.io/ShinyExploratoryPrototype/
# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, tidyverse, dplyr, leaflet, leaflet.extras, plotly, highcharter, 
               ggthemes, fresh, sf, spdep, tmap, tm, ggforce, ggraph, igraph, wordcloud, tidytext, DT)

# read data files
final <- readRDS("data/final.rds")
mapping_rates <- readRDS("data/mapping_rates.rds")


###############################   
########### MAIN BODY - START
###############################

#==========================================================  
# main header ---
header <- dashboardHeader(title = "Decoding Chaos")


# main sidebar ---
sidebar <- dashboardSidebar(
  tags$style(HTML("
      .main-sidebar{
        width: 250px;
      }
      .main-header > .navbar {
        margin-left: 250px;      
      }
      
      
      .box.box-solid.box-info>.box-header {
      color:#000000;
      background:#F8F8F8
                    }
      .box.box-solid.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      }
      .box.box-info>.box-header {
      color:black; 
      background:#F8F8F8
      }
      .box.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      background: #F8F8F8;
      }
                  ")),
  
  sidebarMenu(
    width = 100,
    menuItem("Exploratory", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Clustering & Outlier Analysis", tabName = "Clustering", icon = icon("circle-nodes")),
    menuItem("Hot/ Cold Zone Analysis", tabName = "HotCold", icon = icon("magnifying-glass-chart")),
    menuItem("Confirmatory Analysis", tabName = "Confirmatory", icon = icon("clipboard-check")),
    menuItem("Visit ACLED data", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/data-export-tool/")))


# customise theme ---
mytheme <- create_theme(
  adminlte_color(
    red = "#9A3E41"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#F8F8F8",
    dark_hover_bg = "#9A3E41",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF", 
    info_box_bg = "#FFF"
  )
)

# =============================    
########### EXPLORATORY - START
# =============================


#==========================================================  
# ExploreOverviewrow1 ---
#==========================================================  
ExploreOverviewrow1 <-  fluidRow(
  box(title = "Armed Conflict Incidents in Myanmar (2010 to 2023)",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      box(width = 12,  # Automatically adjust to full width
                 align = "left",
                 withSpinner(leafletOutput("emap_eo1", height = "650px", width = "100%")),
          
                absolutePanel(id = "controls", class = "panel panel-default",
                        top = 180, right = 300, width = 250, fixed=TRUE,
                        draggable = TRUE, height = "auto",
                        box(title = "Desired Characteristics",
                            status = "info",
                            solidHeader = FALSE, 
                            collapsible = TRUE,
                            width = NULL,
                            selectizeInput(inputId = "EventSelect_eo1",
                                           label = "Select Event(s)",
                                           choices = unique(as.character(final$event_type)),
                                           selected = c("Battles", "Violence against civilians"),
                                           options = list(maxItems=3),
                                           multiple = TRUE
                            ),
                            hr(),
                            selectizeInput(inputId = "AdminSelect_eo1",
                                           label = "Select Administrative Region 1(s)",
                                           choices = unique(as.character(final$admin1)),
                                           selected = c("Bago-West", "Bago-East", "Shan-South", "Yangon", "Rakhine", "Kachin", "Sagaing", "Mandalay", "Magway"),
                                           multiple = TRUE
                            ),
                            hr(),
                            sliderInput(inputId = "YearSlider_eo1", 
                                        label = "Years:", 
                                        min = 2010, 
                                        max = 2023,
                                        value = c(2010, 2023),
                                        step = 1)
                        )
                ),
          absolutePanel(id = "controls", class = "panel panel-default",
                        top = 180, right = 40, width = 250, fixed=TRUE,
                        draggable = TRUE, height = "auto",
                        box(title = "About",
                            status = "danger",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = NULL,
                            align = "justify",
                            textOutput("abouttext") 
                        )
                )      
  ))
)



#==========================================================  
# ExploreIntensityrow1 ---
#==========================================================  
ExploreIntensityrow1 <-  fluidRow(
  box(title = "Spatial Intensity Map (Fatalities due to Armed Conflicts)",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      box(width = 12,  # Automatically adjust to full width
          align = "left",
          withSpinner(leafletOutput("emap_esi1", height = "650px", width = "100%")),
          
          absolutePanel(id = "controls", class = "panel panel-default",
                        top = 180, right = 300, width = 250, fixed=TRUE,
                        draggable = TRUE, height = "auto",
                        box(title = "Desired Characteristics",
                            status = "info",
                            solidHeader = FALSE, 
                            collapsible = TRUE,
                            width = NULL,
                            selectizeInput(inputId = "EventSelect_esi1",
                                           label = "Select Event(s)",
                                           choices = unique(as.character(final$event_type)),
                                           selected = c("Battles", "Violence against civilians"),
                                           multiple = TRUE
                            ),
                            hr(),
                            selectizeInput(inputId = "AdminSelect_esi1",
                                           label = "Select Administrative Region 1(s)",
                                           choices = unique(as.character(final$admin1)),
                                           selected = c("Bago-West", "Bago-East", "Shan-South", "Yangon", "Rakhine", "Kachin", "Sagaing", "Mandalay", "Magway"),
                                           multiple = TRUE
                            ),
                            hr(),
                            sliderInput(inputId = "YearSlider_esi1", 
                                        label = "Years:", 
                                        min = 2010, 
                                        max = 2023,
                                        value = c(2010, 2023),
                                        step = 1),
                            hr(),
                            sliderInput(inputId = "radiusSlider_esi1", 
                                        label = "Radius Intensity", 
                                        min = 5, 
                                        max = 20,
                                        value = 10,
                                        step = 1),
                            hr(),
                            sliderInput(inputId = "blurSlider_esi1", 
                                        label = "Blur Intensity", 
                                        min = 5, 
                                        max = 20,
                                        value = 10,
                                        step = 1),
                            hr(),
                            sliderInput(inputId = "maxSlider_esi1", 
                                        label = "Maximum Point Intensity", 
                                        min = 50, 
                                        max = 250,
                                        value = 100,
                                        step = 50)
                        )
          ),
          absolutePanel(id = "controls", class = "panel panel-default",
                        top = 180, right = 40, width = 250, fixed=TRUE,
                        draggable = TRUE, height = "auto",
                        box(title = "Chart Interpretation",
                            status = "danger",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = NULL,
                            align = "justify",
                            textOutput("intensitytext") 
                        )
          )      
      ))
)




#==========================================================  
# ExploreGeospatialrow1 ---
#==========================================================  
ExploreGeospatialrow1 <-  fluidRow(
  
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL, 
             helpText("Filter options are applicable for statistical distribution"),
             radioButtons(inputId = "InciFata_eg1",
                          label = "Display",
                          choices = c("No. of Incidents", "No. of Fatalities"),
                          selected = "No. of Incidents")
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("statisticaltext") 
         )
         
  ),
  column(4,
         box(title = "Statistical Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,  # Automatically adjust to full width
             align = "left",
             withSpinner(tmapOutput("emap_eg1", height = "700px", width = "100%"))
         )   
  ),
  column(4,
         box(title = "Spatial Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,
             align = "center",
             withSpinner(tmapOutput("emap_eg2", height = "700px", width = "100%"))
         )   
  ),
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL, 
             helpText("Filter options are applicable for spatial distribution"),
             selectInput(inputId = "YearSelect_eg2", 
                         label = "Year:", 
                         choices = unique(final$year)
             ),
             hr(),
             radioButtons(inputId = "InciFata_eg2",
                      label = "Display",
                      choices = c("Incident Rate", "Fatality Rate", "Political Violence Rate", "Violence against civilian Rate"),
                      selected = "Incident Rate"),
            hr(),
            selectInput(inputId = "ClassificationSelect_eg2", 
                     label = "Classification Type:", 
                     choices = c("equal", "kmeans", "pretty", "quantile"),
                     selected = "kmeans"
            ),
            hr(),
            selectInput(inputId = "ClassSelect_eg2", 
                     label = "Number of Classes:", 
                     choices = c(2, 4, 6),
                     selected = 6
            )),
            box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("spatialtext") 
         )
  )
  
)


#==========================================================  
# ExploreTrendrow1 ---
#==========================================================  
ExploreTrendrow1 <-  fluidRow(
  box(
    title = "Rising Armed Conflict Incidents in Myanmar (2010 to 2023)",
    status = "danger",
    solidHeader = TRUE, 
    collapsible = TRUE,
    width = 12,  # Use full width
    withSpinner(highchartOutput("line_et1", height = "250px"))
  )
)

#==========================================================  
# ExploreTrendrow2 ---
#==========================================================  
ExploreTrendrow2 <-  fluidRow(
  box(
    title = "Annual Calendar",
    status = "danger",
    solidHeader = TRUE, 
    collapsible = TRUE, 
    width = 12,  # Use full width
    column(8,
           box(width = NULL,
               withSpinner(plotlyOutput("calendar_et2", height = "400px", width = "100%")),
               absolutePanel(id = "controls", class = "panel panel-default",
                             top = 180, right = 300, width = 250, fixed=TRUE,
                             draggable = TRUE,
                             box(title = "Chart Interpretation",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = NULL,  # Automatically adjust to full width
                                 align = "justify",
                                 textOutput("trendtext") 
                             )
               )
           )
    ),
    column(4,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL, 
                         helpText("Filter options are applicable to calendar"),
          selectInput(inputId = "YearSelect_et2", 
                      label = "Year:", 
                      choices = unique(final$year), 
                      selected = 2023
                      ),
          hr(),
          radioButtons(inputId = "InciFata_et2",
                       label = "Display",
                       choices = c("No. of Incidents", "No. of Fatalities"),
                       selected = "No. of Fatalities")
           )
    )
  )

)


#==========================================================  
# ExploreDistributionrow1 ---
#==========================================================  
ExploreDistributionrow1<-  fluidRow(
  box(
    title = "Distribution of armed conflicts in Myanmar",
    status = "danger",
    solidHeader = TRUE, 
    width = 12,  # Use full width
    column(9,
           box(width = NULL,
               align = "center",
               withSpinner(plotlyOutput("box_ed1", height = "600px", width = "100%"))
           )
    ),
    column(3,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL, 
               sliderInput(inputId = "YearSlider_ed1", 
                           label = "X-axis (Years)", 
                           min = 2010, 
                           max = 2023,
                           value = c(2015, 2020),
                           step = 1
                           ),
               hr(),
               radioButtons(inputId = "yaxis_ed1",
                            label = "Y-axis",
                            choices = c("Event", "Administrative Region 1"),
                            selected = "Event"),
               hr(),
               radioButtons(inputId = "method_ed1",
                            label = "Frequency method (jitter)",
                            choices = c("Count distribution", "Density distribution"),
                            selected = "Density distribution")
           ),
           box(title = "Chart Interpretation",
               status = "danger",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = NULL,  # Automatically adjust to full width
               align = "justify",
               textOutput("distributiontext") 
           )
    )
  ) 
)

#==========================================================  
# ExploreNetworkrow1 ---
#==========================================================  
ExploreNetworkrow1 <-  fluidRow(
  box(
    title = "Network Relationship Among Actors in Myanmar",
    status = "danger",
    solidHeader = TRUE, 
    width = 12,  # Use full width
    column(9,
         box(width = NULL,
             align = "center",
             withSpinner(plotOutput("network_en1", height = "600px", width = "100%")) 
         )
         ),
    column(3,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL, 
               selectizeInput(inputId = "EventSelect_en1",
                            label = "Select Event(s)",
                            choices = unique(final$event_type),
                            multiple = TRUE,
                            selected = c("Battles", "Violence against civilians")
               ),
               hr(),
               selectizeInput(inputId = "AdminSelect_en1",
                            label = "Select Administrative Region 1(s)",
                            choices = unique(final$admin1),
                            multiple = TRUE,
                            selected = c("Bago-West", "Bago-East", "Shan-South", "Yangon", "Rakhine", "Kachin", "Sagaing", "Mandalay", "Magway")
               ),
              hr(),
              selectInput(inputId = "YearSelect_en1", 
                         label = "Year", 
                         choices = unique(final$year), 
                         # selected = 2023,
                        ),
              hr(),
              sliderInput(inputId = "FreqSlider_en1", 
                          label = "Frequency of actors' network", 
                          min = 10, 
                          max = 300,
                          value = 50,
                          step = 10)
                    ),
            box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,  # Automatically adjust to full width
             align = "justify",
             textOutput("networktext") 
         )
  )
  ) 
)



#==========================================================  
# ExploreSummaryrow1 ---
#========================================================== 
ExploreSummaryrow1 <-  fluidRow(
  column(4,
         box(
           title = "Word Cloud - Incident Summary",
           status = "danger",
           solidHeader = TRUE, 
           collapsible = TRUE,
           width = NULL,
           height = 800,
           box(title = "Desired Characteristics",
                      status = "info",
                      solidHeader = FALSE, 
                      width = NULL,
                      helpText("Filter options are applicable to word cloud"),
                      selectizeInput(inputId = "EventSelect_es1",
                                     label = "Select Event(s)",
                                     choices = unique(final$event_type),
                                     multiple = TRUE,
                                     selected = c("Battles", "Violence against civilians")
                      ),
                      hr(),
                      selectizeInput(inputId = "AdminSelect_es1",
                                     label = "Select Administrative Region 1(s)",
                                     choices = unique(final$admin1),
                                     multiple = TRUE,
                                     selected = c("Bago-West", "Bago-East", "Shan-South", "Yangon", "Rakhine", "Kachin", "Sagaing", "Mandalay", "Magway")
                                     ),
                      hr(),
                      selectInput(inputId = "YearSelect_es1", 
                                  label = "Year", 
                                  choices = unique(final$year), 
                                  # selected = 2023,
                      )
           ),
           withSpinner(plotOutput("cloud_es1", height = "300px", width = "100%"))
           )),
  
  column(8,
         box(
           title = "Data Table",
           status = "danger",
           width = NULL,
           solidHeader = TRUE, 
           collapsible = TRUE,
           withSpinner(DT::dataTableOutput(outputId = "ExploreSummaryTable")),
           style = "height:750px; overflow-y: scroll;overflow-x: scroll;" # Set scrollbars
         )
         
  )
  
)


#==========================================================  
# ExploreSubTabs
#==========================================================  
ExploreSubTabs <- tabsetPanel(
  tabPanel("Overview", 
           ExploreOverviewrow1
  ),
  tabPanel("Geospatial Intensity", 
           ExploreIntensityrow1
  ),
  tabPanel("Geospatial Exploration", 
           ExploreGeospatialrow1
  ),
  tabPanel("Trends", 
           ExploreTrendrow1,
           ExploreTrendrow2
  ),  
  tabPanel("Distributions", 
           ExploreDistributionrow1
  ),
  tabPanel("Network Relationships", 
           ExploreNetworkrow1
  ),
  tabPanel("Incident Summary", 
           ExploreSummaryrow1
  )
)



# =============================    
########### EXPLORATORY - END
# =============================

# main body ---
body <- dashboardBody(
  
  # use theme
  use_theme(mytheme),
  
  
  # =============================
  # tabItems - All Pages
  # =============================
  tabItems(
    # 1st tab content ---
    tabItem(tabName = "Exploratory",
            ExploreSubTabs
    ),
    # 2nd tab content ---
    tabItem(tabName = "Cluster and Outlier Analysis"
            #/// replace with fluidRow Names
    ),
    # 3rd tab content ---
    tabItem(tabName = "Hot and Cold Zone Analysis",
            #/// replace with fluidRow Names  
    ),
    # 4th tab content ---
    tabItem(tabName = "Confirmatory Analysis"
            #/// replace with fluidRow Names
    )
  )
)  



###############################   
########### MAIN BODY - END
###############################


# =============================    
########### UI - START
# =============================

# UI dashboard ---
ui <- dashboardPage(title = 'Armed Conflicts in Myanmar (2010 to 2023)', 
                    header, sidebar, body, skin='red')    

# =============================    
########### UI - END
# =============================


# =============================    
########### SERVER - START
# =============================
server <- function(input, output) {
  
  # =============================    
  # EXPLORATORY
  # =============================
  
  #==========================================================
  # Explore Map ---
  #==========================================================    
  final_explorespatial <- reactive({
    req(input$YearSlider_eo1)
    req(input$EventSelect_eo1)
    req(input$AdminSelect_eo1)
    filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
      filter(event_type %in% input$EventSelect_eo1) %>%
      filter(admin1 %in% input$AdminSelect_eo1)
    
  })
  
    # Render Point Map --- 
    output$emap_eo1 <- renderLeaflet({
      
      incident_pts <- final_explorespatial()
      
      incident_pts %>%
        filter(fatalities > 0)
      
      cof <- colorFactor(c("#ff7e8a", "#394938", "#ffa500", 
                           "#0092ff", "#741b47", "#60dcb5"), 
                         domain=c("Battles", "Explosions/ Remote violence", 
                                  "Protests", "Riots",
                                  "Strategic developments", 
                                  "Violence against civilians"))
      
      leaflet(incident_pts) %>% 
        addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
        setView(100, 20, zoom = 5) %>%
        addCircleMarkers(lat= ~latitude, lng = ~longitude, radius = ~(fatalities)^(1/2),
                         color = ~cof(event_type), weight = 1, fillOpacity = 0.1,
                         popup = paste( "<strong>", incident_pts$event_date, "</strong>", 
                                        "<br><strong>Country: </strong>",
                                        incident_pts$country, 
                                        "<br><strong>Sub-national Admin Region: </strong>",
                                        incident_pts$admin1, "/", 
                                        incident_pts$admin2, "/", 
                                        incident_pts$admin3, 
                                        "<br><strong>Event type: </strong>",
                                        incident_pts$event_type, 
                                        "<br><strong>Sub-event type: </strong>", 
                                        incident_pts$sub_event_type, 
                                        "<br><strong>Summary: </strong>", 
                                        incident_pts$notes,
                                        "<br><strong>Total Fatalities: </strong>",
                                        incident_pts$fatalities)) %>% 
        addLegend("bottomright", pal = cof, values = incident_pts$event_type, title = "Event Type")
    })
  
    output$abouttext <- renderText({ 
      "Armed conflicts due to political violence and coordinated attacks targeting innocent civilians, have been on the rise globally. 
      This threatens the public at both physical and psychological levels. 
      this visualisation dashboard helps: 
      (1) discover armed conflicts trends and 
      (2) conceptualise armed conflict spaces in Myanmar." 
    })

  #==========================================================
  # Explore Intensity Map ---
  #==========================================================    
    final_exploreintensity <- reactive({
      req(input$YearSlider_esi1)
      req(input$EventSelect_esi1)
      req(input$AdminSelect_esi1)
      req(input$radiusSlider_esi1) 
      req(input$blurSlider_esi1)
      req(input$maxSlider_esi1)
      filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
        filter(event_type %in% input$EventSelect_esi1) %>%
        filter(admin1 %in% input$AdminSelect_esi1)
      
    })
    
    # Render Spatial Intensity Map --- 
    output$emap_esi1 <- renderLeaflet({
      
      incident_pts <- final_exploreintensity()
      
      incident_pts %>%
        filter(fatalities > 0)
      
      # create armed conflict incidents hotspot intensity
      hotmap_inci <-  leaflet(incident_pts) %>% 
        addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
        setView(100, 20, zoom = 5) %>%
        addHeatmap(data=incident_pts, lng = ~longitude, lat = ~latitude, 
                   intensity = ~fatalities, max = input$maxSlider_esi1, radius = input$radiusSlider_esi1, blur = input$blurSlider_esi1) %>%
        addCircleMarkers(data=incident_pts, lat= ~latitude, lng = ~longitude,
                         fillOpacity = 0, weight = 0, 
                         popup = paste( "<strong>", incident_pts$event_date, "</strong>", 
                                        "<br><strong>Country: </strong>",
                                        incident_pts$country, 
                                        "<br><strong>Sub-national Admin Region: </strong>",
                                        incident_pts$admin1, "/", 
                                        incident_pts$admin2, "/", 
                                        incident_pts$admin3, 
                                        "<br><strong>Event type: </strong>",
                                        incident_pts$event_type, 
                                        "<br><strong>Sub-event type: </strong>", 
                                        incident_pts$sub_event_type, 
                                        "<br><strong>Summary: </strong>", 
                                        incident_pts$notes,
                                        "<br><strong>Total Fatalities: </strong>",
                                        incident_pts$fatalities),
                         labelOptions = labelOptions(noHide = TRUE))
  
    })
  
  output$intensitytext <- renderText({ 
    "Spatial intensity map shows the intensity of value (i.e. fatalities), 
    with warm colours (e.g. red/ orange/ yellow) indicating higher intensity values 
    while cool colours (e.g. blue/ green) indicating lower intensity values.
    The map intensity can be adjusted based on the settings of radius, blur and maximum point intensity parameters." 
  })
  
  #==========================================================
  # Explore Statistical Distribution Map ---
  #==========================================================   
  mapping_rates_explorestats <- reactive({
    req(input$InciFata_eg1)
  })
  
  # Render Statistical Distribution Map --- 
  output$emap_eg1 <- renderTmap({
    
    # create boxbreak function
    boxbreaks <- function(v,mult=1.5) {
      qv <- unname(quantile(v))
      iqr <- qv[4] - qv[2]
      upfence <- qv[4] + mult * iqr
      lofence <- qv[2] - mult * iqr
      # initialize break points vector
      bb <- vector(mode="numeric",length=7)
      # logic for lower and upper fences
      if (lofence < qv[1]) {  # no lower outliers
        bb[1] <- lofence
        bb[2] <- floor(qv[1])
      } else {
        bb[2] <- lofence
        bb[1] <- qv[1]
      }
      if (upfence > qv[5]) { # no upper outliers
        bb[7] <- upfence
        bb[6] <- ceiling(qv[5])
      } else {
        bb[6] <- upfence
        bb[7] <- qv[5]
      }
      bb[3:5] <- qv[2:4]
      return(bb)
    }
    
    # create get.var function
    get.var <- function(vname,df) {
      v <- df[vname] %>% st_set_geometry(NULL)
      v <- unname(v[,1])
      return(v)
    }
    
    # create boxmap function
    boxmap <- function(vnam, df, 
                       legtitle=NA,
                       mtitle= paste0("Box Map of ", vnam),
                       mult=1.5){
      var <- get.var(vnam,df)
      bb <- boxbreaks(var)
      tm_shape(df) +
        tm_polygons() +
        tm_shape(df) +
        tm_fill(vnam,title=legtitle,
                breaks=bb,
                palette="Reds",
                labels = c("lower outlier", 
                           "< 25%", 
                           "25% - 50%", 
                           "50% - 75%",
                           "> 75%", 
                           "upper outlier"))  +
        tm_layout(main.title = mtitle, 
                  title.position = c("left",
                                     "top"))
    }
    
    if(input$InciFata_eg1 == "No. of Incidents"){
      box_inci <- boxmap("total_inci", mapping_rates)
    }
    
    else{
      box_inci <- boxmap("total_fata", mapping_rates)
    }
    
  })
  
  ########################################
  output$statisticaltext <- renderText({ 
    "A boxmap is a spatial representation akin to boxplot and histogram, 
    which is useful to detect outliers and visualise distribution of variables across Myanmar's
    different geographical subnational administrative region 2." 
  })
  
  #==========================================================
  # Explore Spatial Distribution Map ---
  #==========================================================  
  mapping_rates_exploredist <- reactive({
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$ClassSelect_eg2)
    req(input$ClassificationSelect_eg2)

  })
  
  # Render Spatial Distribution Map --- 
  output$emap_eg2 <- renderTmap({

    if(input$InciFata_eg2 == "Incident Rate"){
      var <- paste("pct_inci_", input$YearSelect_eg2, sep = "")
    }
    
    else if(input$InciFata_eg2 == "Fatality Rate"){
      var <- paste("pct_fata_", input$YearSelect_eg2, sep = "")
    }
    
    else if(input$InciFata_eg2 == "Political Violence Rate"){
      var <- paste("pct_politicalrate_", input$YearSelect_eg2, sep = "")
    }
    
    else{
      var <- paste("pct_civilianrate_", input$YearSelect_eg2, sep = "")
    }  
    
    tmap_obj2 <- tm_shape(mapping_rates) +
      tm_fill(var, 
              n = input$ClassSelect_eg2,
              style = input$ClassificationSelect_eg2,
              palette = "Reds", 
              legend.hist = TRUE, 
              legend.is.portrait = TRUE, 
              legend.hist.z = 0.1) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_layout(main.title = "Distribution of Armed Conflict Incidents in Myanmar \nKmeans classification",
                title = "",
                main.title.size = 1,
                legend.height = 0.60,
                legend.width = 5.0,
                legend.outside = FALSE,
                legend.position = c("left", "bottom"))
    
    
  })

  ########################################  
  output$spatialtext <- renderText({ 
    "A choropleth map visualises spatial pattern or distribution across Myanmar's
    different geographical subnational administrative region 2, which can be further customised 
    by adjusting the desired data classification type and number of classes (or data ranges)." 
  })
  
  #==========================================================  
  # Explore Trends ---
  #==========================================================  
  calendar <- final %>%
    filter(fatalities > 0) %>%
    group_by(year, event_date, admin1) %>%
    mutate(
      wkday = wday(event_date),
      day = mday(event_date),
      month = factor(months(event_date), levels = rev(month.name)),
      week = isoweek(event_date),
      year_month = format(zoo::as.yearmon(event_date), "%y-%m")
    ) %>%
    ungroup()
  
  # Render Line Chart --- 
  output$line_et1 <- renderHighchart({
    
    year_fata <- calendar %>%
      filter(fatalities > 0) %>%
      group_by(year_month) %>%
      select(year, month, year_month, fatalities) %>%
      summarise(total_fata = sum(fatalities),
                total_inci = n()) %>%
      ungroup()
    
    hc_plot1 <-  highchart() %>% 
      hc_add_series(year_fata, hcaes(x = year_month, y = total_fata), type = "line", 
                    name = "Total Fatalities", color = "lightcoral") %>%
      hc_add_series(year_fata, hcaes(x = year_month, y = total_inci), type = "line", 
                    name = "Total Incidents", color = "black") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                 backgroundColor = "#FCFFC5",
                 borderWidth = 5,
                 pointFormat = "<b>20{point.year_month}</b> 
                                 <br> Fatalities: <b>{point.total_fata}</b>
                                 <br> Incidents: <b>{point.total_inci}</b>"
      ) %>%
      hc_title(text = "Armed Conflict Over The Years") %>% 
      hc_subtitle(text = "2010 to 2023") %>%
      hc_xAxis(title = list(text = "2010-2023"), labels = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Frequency"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "lightcoral", width = 1, dashStyle = "Dash",
                 value = mean(year_fata$total_fata),
                 label = list(text = paste("Average Monthly Fatalities:", round(mean(year_fata$total_fata))),
                              style = list(color = 'lightcoral', fontSize = 20))))) 
    hc_plot1
    
  })
  
  ########################################
  final_explorecalendar <- reactive({
    req(input$InciFata_et2)
    req(input$YearSelect_et2)
    filter(final, year %in% input$YearSelect_et2)
    
  })
  
  # Render calendar --- 
  output$calendar_et2 <- renderPlotly({
    
    calendar <- final_explorecalendar() 
    # selected years
    years <- input$YearSelect_et2
    
    calendar1 <- calendar %>%
      filter(fatalities > 0) %>%
      group_by(year, event_date, admin1) %>%
      mutate(
        wkday = wday(event_date),
        day = mday(event_date),
        month = factor(months(event_date), levels = rev(month.name)),
        week = isoweek(event_date)
      ) %>%
      ungroup()
    
    cal_conflict <- calendar1 %>%
      group_by(year, month, day) %>%
      filter(year == years) %>%
      summarise(total_fata = sum(fatalities),
                total_inci = n()) %>%
      ungroup()
    
    
    # tooltip
    tooltip_heat <- paste("<b>", cal_conflict$day, " ", cal_conflict$month, " ",
                          cal_conflict$year, "</b>", 
                          "\nFatalities : ", cal_conflict$total_fata,
                          "\nIncidents : ", cal_conflict$total_inci)
    
    if(input$InciFata_et2 == "No. of Incidents"){
      cal_var_fill <- cal_conflict$total_inci
      titletext <- paste("Armed Conflicts in Myanmar in", years)
      legendname <- "Incidents"
      }
    
    if(input$InciFata_et2 == "No. of Fatalities"){
      cal_var_fill <- cal_conflict$total_fata
      titletext <- paste("Fatalities due to Armed Conflicts in Myanmar in", years)
      legendname <- "Fatalities"
      }
    
    heat <- ggplot(cal_conflict, aes(x = day, y = month, fill = cal_var_fill)) +
      geom_tile(color = "white", size = 1, aes(text = tooltip_heat)) + 
      theme_tufte(base_family = "Helvetica") + 
      coord_equal() +
      scale_fill_gradient(name = legendname, low = "#fff2f4", 
                          high = "lightcoral") +
      labs(x = "Calendar Day", 
           y = "", 
           title = titletext,
           caption = "Data Source: ACLED (2023)") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6),
            legend.position = "top") +
      scale_x_continuous(breaks = seq(min(cal_conflict$day), max(cal_conflict$day), 
                                      by = 2),
                         labels = seq(min(cal_conflict$day), max(cal_conflict$day), 
                                      by = 2))
    heat_plotly <- ggplotly(heat, tooltip = "text")
    
    heat_plotly
  })
  
  
  ########################################
  output$trendtext <- renderText({ 
    "The line chart (top) shows armed conflict incidents and fatalities on a monthly basis, 
    while calendar heatmap (bottom) shows daily armed conflict incidents and fatalities based on selected year. 
    Calendar days of each month is arranged in rows. 
    The colour squares in the calendar heatmap represent the corresponding cell value (i.e. darker shade denotes higher frequency count)."
  })
  
  #==========================================================  
  # Explore Distributions ---
  #==========================================================  
  final_exploretime <- reactive({
    req(input$YearSlider_ed1)
    req(input$yaxis_ed1)
    req(input$method_ed1)
      filter(final, year >= input$YearSlider_ed1[1] & year <= input$YearSlider_ed1[2])
  })
  
  # Render Boxplot1 --- 
  
  output$box_ed1 <- renderPlotly({
    box_pts <- final_exploretime()
    
    if(input$method_ed1 == "Density distribution"){
      sinamethod <- "density"
    }
    else{
      sinamethod <- "counts"
    }
    
    box_pts %>%
      filter(fatalities >0)
    
    if(input$yaxis_ed1 == "Event"){
      box1 <- ggplot(box_pts, aes(x = forcats::fct_infreq(event_type), y = event_date, 
                                  color = factor(event_type), fill = factor(event_type))) +
        
        geom_boxplot(width = .2, color = "#000000", fill = NA, size = .5, 
                     outlier.shape = NA, position = position_nudge(.25)) +
        geom_sina(method = sinamethod, alpha = .05) +
        coord_flip()+
        theme_minimal() +
        theme(legend.position = "none", 
              plot.title.position = "plot") +
        labs(title = "", subtitle = "",
             y = paste("Year", input$YearSlider_ed1[1], "to", input$YearSlider_ed1[2]),
             x = "Event Type", 
             caption = "Data Source: ACLED (2023)")
      
      ggplotly(box1)
    }
    
    else{
      box2 <- ggplot(box_pts, aes(x = forcats::fct_infreq(admin1), y = event_date, 
                                  color = factor(admin1), fill = factor(admin1))) +
        
        geom_boxplot(width = .2, color = "#000000", fill = NA, size = .5, 
                     outlier.shape = NA, position = position_nudge(.25)) +
        geom_sina(method = sinamethod, alpha = .05) +
        coord_flip()+
        theme_minimal() +
        theme(legend.position = "none", 
              plot.title.position = "plot") +
        labs(title = "", subtitle = "",
             y = paste("Year", input$YearSlider_ed1[1], "to", input$YearSlider_ed1[2]),
             x = "Adminstrative Region 1", 
             caption = "Data Source: ACLED (2023)")
      
      ggplotly(box2)
    }
    
  })
  
  ########################################
  output$distributiontext <- renderText({ 
    "A jittered-cum-boxplot shows the distribution of the armed conflicts incidents in Myanamr and how it has changed over the years.
    It helps to determine how spread out the incidents were across the years, 
    how it started/ ended in certain years or high concentrations in certain years.
    Points that are clustered together show higher frequency during the selected time period.
    "  
  })
  
  #==========================================================
  # Explore Network Relationship ---
  #==========================================================    
  final_explorenetwork <- reactive({
    req(input$YearSelect_en1)
    req(input$EventSelect_en1)
    req(input$AdminSelect_en1)
    req(input$FreqSlider_en1)
    filter(final, year %in% input$YearSelect_en1) %>%
      filter(event_type %in% input$EventSelect_en1) %>%
      filter(admin1 %in% input$AdminSelect_en1)
  })
  
  output$network_en1 <- renderPlot({
    
    network_data <-  final_explorenetwork()
    
    # select year
    years <- input$YearSelect_en1
    
    # calculate frequency
    conflict_count <- network_data %>% 
      group_by(actor1) %>% 
      filter (year == years) %>%
      summarise(frequency = n()) %>%
      arrange(desc(frequency)) 
    
    conflict_count2 <- network_data %>% 
      group_by(actor2) %>% 
      filter (year == years) %>%
      summarise(frequency = n()) %>%
      arrange(desc(frequency))
    
    colnames(conflict_count) = c("actor","Freq")
    colnames(conflict_count2) = c("actor","Freq")
    
    # combine both actor 1 & 2
    final_conflict_count = rbind(conflict_count,conflict_count2)
    
    final_conflict_count2 = final_conflict_count %>%
      group_by(actor) %>%
      summarise(FrequencyConflicts = sum(Freq)) %>%
      arrange(desc(FrequencyConflicts))
    
    # trim actor/ assoc_actor
    network_data$actor1 = trimws(str_replace(network_data$actor1, "[Õ]", ""))
    network_data$actor2  = trimws(str_replace(network_data$actor2, "[Õ]", ""))
    network_data$assoc_actor_1 = trimws(str_replace(network_data$assoc_actor_1, "[Õ]", ""))
    network_data$assoc_actor_2 = trimws(str_replace(network_data$assoc_actor_2, "[Õ]", ""))
    
    assoc1 = network_data %>%
      filter(!is.na(actor1)) %>%
      filter(!is.na(assoc_actor_1)) %>%
      filter(!(actor1 == "")) %>%
      filter(!(assoc_actor_1 == "")) %>%
      select(actor1,assoc_actor_1)
    
    assoc2 = network_data %>%
      filter(!is.na(actor2)) %>%
      filter(!is.na(assoc_actor_2)) %>%
      filter(!(actor2 == "")) %>%
      filter(!(assoc_actor_2 == "")) %>%
      select(actor2,assoc_actor_2)
    
    colnames(assoc1) = c("actor","assoc_actor") 
    colnames(assoc2) = c("actor","assoc_actor") 
    
    # combine both assoc 1 & 2
    combined1 = rbind(assoc1,assoc2)
    
    combined2 = combined1 %>%
      group_by(actor,assoc_actor) %>%
      tally(sort = TRUE) 
    
    
    final_conflict_count3 = trimws(final_conflict_count2$actor)
    
    combined3 = combined2 %>%
      filter(actor %in% final_conflict_count3)
    
    
    # network graph
    viz_actors_12 <- function(actors_12) {
      set.seed(1234)
      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
      
      actors_12 %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "lightcoral") +
        geom_node_point(size = 5) +
        geom_node_text(aes(label = name), repel = TRUE,
                       point.padding = unit(0.5, "lines")) +
        theme_void()
    }
    
    # Render network --- 
    combined3 %>%
      filter(n >= input$FreqSlider_en1) %>%
      viz_actors_12
  })
  

  
  ########################################
  output$networktext <- renderText({ 
    "A network graph schematically depict the nodes and connections amongst the actors and their associations involved in the armed conflicts in Myanmar.
     The line thickness indicates the strength of connections (i.e. thicker line denotes stronger connection) among actors in the network." 
  })
  
  
  #==========================================================  
  # Explore Summary ---
  #==========================================================  
  final_exploreCloud <- reactive({
    req(input$YearSelect_es1)
    req(input$EventSelect_es1)
    req(input$AdminSelect_es1)
    filter(final, year %in% input$YearSelect_es1) %>%
      filter(event_type %in% input$EventSelect_es1) %>%
      filter(admin1 %in% input$AdminSelect_es1)
  })
  
  
  # Render network --- 
  output$cloud_es1 <- renderPlot({
    
    cloud_data <-  final_exploreCloud()
    
    # select year
    years <- input$YearSelect_es1
    
    cloudtext1 <- cloud_data %>%
      select(year, notes)
    
    cloudtext2 <- function(year) {
      subset_data <- cloudtext1 %>%
        filter(notes != "")
      
      docs <- Corpus(VectorSource(subset_data$notes))
      docs <- tm_map(docs, removeNumbers)
      docs <- tm_map(docs, removeWords, stopwords("english"))
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, stripWhitespace)
      docs <- tm_map(docs, stemDocument)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m), decreasing = TRUE)
      d <- data.frame(word = names(v), freq = v)
      
      wordcloud(d$word, d$freq, colors = brewer.pal(9, "Set3"), random.order = FALSE, rot.per = 0)
      title(main = paste(year), font.main = 1, col.main = "black", cex.main = 1.5)
    }
    
    # Word clouds for each year
    cloud_patch <- lapply(years, cloudtext2)
  })
  
  ########################################
  output$ExploreSummaryTable <- DT::renderDataTable({
    table_data <-  final_exploreCloud()
  
    DT::datatable(
      table_data, 
      class = "compact",
      filter = "top", 
      extensions = c("Buttons"),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(targets = c(1:7, 9, 11:23, 26), className = "dt-center"), 
          list(targets = c(8, 10, 24, 25), visible = FALSE)),
        buttons = list(
          list(extend = c("colvis"), columns = c(1:26))),
        dom = "Bpiltf"),
      caption = "Table 1: Armed Conflicts in Myanmar (2010-2023)"
    )
  })
  
}
# =============================    
########### SERVER - END
# =============================


# =============================    
########### RUN APPLICATION
# =============================
shinyApp(ui = ui, server = server)
