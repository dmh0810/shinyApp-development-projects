## Load packages
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(purrr)) install.packages("purrr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(sp)) install.packages("sp")
if(!require(plotly)) install.packages("plotly")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(leaflet)) install.packages("leaflet")
if(!require(leaflet.extras)) install.packages("leaflet.extras")
if(!require(tigris)) install.packages("tigris")
if(!require(spatialEco)) install.packages("spatialEco")
if(!require(wordcloud2)) install.packages("wordcloud2")
if(!require(tm)) install.packages("tm")
if(!require(SnowballC)) install.packages("SnowballC")
if(!require(jsonify)) install.packages("jsonify")
if(!require(mapdeck)) install.packages("mapdeck")
if(!require(plotrix)) install.packages("plotrix")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr))install.packages("stringr")
if(!require(threejs)) install.packages("threejs")
if(!require(maps))install.packages("maps")
if(!require(readr))  install.packages("readr")
if(!require(dplyr))  install.packages("dplyr")
if(!require(rgdal))  install.packages("rgdal")
if(!require(DT))  install.packages("DT")
if(!require(shiny))  install.packages("shiny")
if(!require(shinythemes))  install.packages("shinythemes")
if(!require(shinydashboard))  install.packages("shinydashboard")
if(!require(shinyWidgets))  install.packages("shinyWidgets")

#Load Data
proj_data <- read_excel("Projektdaten.xlsx")
LP_data <- read_excel("LP Daten.xlsx", na = c(".  ./.", ""))

gesamt_data <- proj_data %>% 
  mutate(Beginnjahr = as.numeric(Beginnjahr)) %>% 
  #mutate(Endejahr = as.numeric(Endejahr)) %>% 
  filter(Partnerland != "(ungültiger Wer") %>%
  filter(!is.na(Kontinent)) %>%
  filter(!is.na(`BMZ-Schwerpunkt`)) %>%
  filter(!is.na(Auftraggeber)) %>%
  filter(Kontinent != 0)

ts_data <- gesamt_data %>% 
  filter(!is.na(Beginnjahr)) %>%
  filter(Beginnjahr != 0 & Beginnjahr != 1008 & Beginnjahr != 2207)

# Word Clouds of FZ + Projektbezeichnung
# Word cloud for Projektbezeichnung:

tm_data_proj_bezeichnung <- gesamt_data %>% 
  filter(!is.na(`Projektbezeichnung, deutsch`)) %>%
  select(`Projektbezeichnung, deutsch`)

corpus_proj_bezeichnung <- Corpus(VectorSource(tm_data_proj_bezeichnung))

#Conversion to Lowercase
corpus_proj_bezeichnung = tm_map(corpus_proj_bezeichnung, PlainTextDocument)
corpus_proj_bezeichnung = tm_map(corpus_proj_bezeichnung, tolower)
#Removing Punctuation
corpus_proj_bezeichnung = tm_map(corpus_proj_bezeichnung, removePunctuation)
#Remove stopwords
corpus_proj_bezeichnung = tm_map(corpus_proj_bezeichnung, removeWords, c("cloth", stopwords("german")))
# Stemming
corpus_proj_bezeichnung = tm_map(corpus_proj_bezeichnung, stemDocument)
# Eliminate white spaces
corpus_proj_bezeichnung = tm_map(corpus_proj_bezeichnung, stripWhitespace)
DTM <- TermDocumentMatrix(corpus_proj_bezeichnung)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

# Map of sum #PNs and AW worldwide (all Partnerland without global + überregional)
land_per_year <- gesamt_data %>%
  group_by(Beginnjahr, Partnerland, Kontinent, ISO, Long, Lat) %>%
  summarise(n = n(), sum_aw = sum(`Projektwert ("Auftragswert")`, na.rm = T)) #this df includes NA in coords 

mean_pn_aw_land <- ts_data %>%
  group_by(Partnerland, Kontinent, Long, Lat, ISO) %>%
  summarise(n=n(), mean_aw = mean(`Projektwert ("Auftragswert")`, na.rm = T))

##### GLobal map
land_spdf <- readOGR( 
  dsn="shapefile/world_shape_file",
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

world_polygons <- geo_join(land_spdf, mean_pn_aw_land, "ISO2", "ISO")

#this bins is for avg. project value (per year) in each country
bins <- c(0, 1000000, 5000000, 10000000, 20000000, Inf )
pal <- colorBin("YlOrRd", domain = mean_pn_aw_land$mean_aw, bins = bins)

glob_basemap <- leaflet(world_polygons) %>%
  addTiles() %>%
  addLegend(pal = pal, values = ~world_polygons$mean_aw, opacity = 0.7, title = "<small>Durchschn. Auftragswert pro Projekt (EUR/Jahr)</small>", position = "bottomright") %>%
  setView(10, 51, 2)

##### Global (insgesamt) Entwicklung 
global_df <- ts_data %>% 
  group_by(Beginnjahr) %>%
  summarise(sum_aw=sum(`Projektwert ("Auftragswert")`, na.rm = TRUE), n=n())

#### Länderportfolio
africa_ts <- land_per_year %>%
  filter(Beginnjahr != 0 & Beginnjahr != 1008) %>%
  filter(Kontinent == "Afrika")

asia_ts <- land_per_year %>% 
  filter(Beginnjahr != 0 & Beginnjahr != 1008) %>%
  filter(Kontinent == "Asien") %>%
  filter(!is.na(sum_aw))

america_ts <- land_per_year %>% 
  filter(Beginnjahr != 0 & Beginnjahr != 1008) %>%
  filter(Kontinent == "Amerika") %>%
  filter(!is.na(sum_aw))

europe_ts <- land_per_year %>% 
  filter(Beginnjahr != 0 & Beginnjahr != 1008) %>%
  filter(Kontinent == "Europa") %>%
  filter(!is.na(sum_aw))

africa_polygons <- geo_join(land_spdf, africa_ts, "ISO2", "ISO")
asia_polygons <- geo_join(land_spdf, asia_ts, "ISO2", "ISO")
america_polygons <- geo_join(land_spdf, america_ts, "ISO2", "ISO")

europe_polygons <- geo_join(land_spdf, europe_ts, "ISO2", "ISO")

pal_africa <- colorBin("YlOrRd", domain = africa_polygons$sum_aw, bins = bins)
pal_asia <- colorBin("YlOrRd", domain = asia_polygons$sum_aw, bins = bins)
pal_america <- colorBin("YlOrRd", domain = america_polygons$sum_aw, bins = bins)
pal_europe <- colorBin("YlOrRd", domain = europe_polygons$sum_aw, bins = bins)

africa_basemap <- leaflet(africa_polygons) %>%
  addTiles() %>%
  addLegend("bottomright", pal = pal_africa, values = ~africa_polygons$sum_aw, title = "<small>Summe aller Aufträge (EUR)</small>") %>%
  setView(10, 10, zoom=3)

asia_basemap <- leaflet(asia_polygons) %>%
  addTiles() %>%
  addLegend("bottomright", pal = pal_asia, values = ~asia_polygons$sum_aw, title = "<small>Summe aller Aufträge (EUR)</small>") %>%
  setView(78, 20, zoom=3)

america_basemap <- leaflet(america_polygons) %>%
  addTiles() %>%
  addLegend("bottomright", pal = pal_america, values = ~america_polygons$sum_aw, title = "<small>Summe aller Aufträge (EUR)</small>") %>%
  setView(-74, 4, zoom=3)

europe_basemap <- leaflet(europe_polygons) %>%
  addTiles() %>%
  addLegend("bottomright", pal = pal_europe, values = ~europe_polygons$sum_aw, title = "<small>Summe aller Aufträge (EUR)</small>") %>%
  setView(10, 52, zoom = 3)

######### LP data

#Remove variables that contain 0 information:
LP_data <- LP_data %>% 
  filter(!is.na(Partnerland))%>%
  mutate(Betrag = as.numeric(Betrag))%>%
  mutate(Beginnjahr = as.numeric(Beginnjahr))

#Anzahl LPs, die eine KoFi haben
kofi_df <- LP_data %>% filter(`Vertragstyp Knz.` != 0) %>%
  filter(!is.na(Beginnjahr)) %>%
  filter(!is.na(Betrag))

arc_df <- kofi_df %>% 
  select(`Geber Land`, KoFiGeber, Long_Orig, Lat_Orig, Partnerland, Long_Desti, Lat_Desti, Betrag, Beginnjahr) %>%
  filter(!is.na(Long_Orig))  %>%
  filter(!is.na(Long_Desti)) %>%
  filter(!is.na(Beginnjahr)) %>%
  filter(!is.na(Betrag))

set_token("pk.eyJ1IjoiZG1oMDgxMCIsImEiOiJja2JmaHFub3YwdnVkMzFxZWNqYXg3cTJvIn0.Ng_cCdfyl7hMLcabtvDw7A")

kofi_df$Long_DE <- rep(10.45153)
kofi_df$Lat_DE <- rep(51.16569)
kofi_df$DE <- rep("Deutschland")

arc_df$Long_DE <- rep(10.45153)
arc_df$Lat_DE <- rep(51.16569)
arc_df$DE <- rep("Deutschland")

base_mapdeck <- mapdeck(style = mapdeck_style("dark"), pitch=45, location = c(unique(arc_df$Long_DE), unique(arc_df$Lat_DE)), zoom=3) 

# Get the image of the globe - NASA
earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg" 

#Display the data on the globe
globe <- globejs(
  img = earth, lat = arc_df$Lat_Desti, long = arc_df$Long_Desti, arcs = arc_df[, c("Lat_Orig", "Long_Orig", "Lat_Desti", "Long_Desti")],
  arcsOpacity = 0.20, arcsHeight = 0.5, arcsLwd = 2,
  arcsColor = "orange", atmosphere = TRUE, height = 700,
  width = 700, bg = "white", value = 4
) 

kofi_agg_pn <- kofi_df %>% 
  count(Beginnjahr,`PNR AVOE`) %>%
  complete(`PNR AVOE`, Beginnjahr, fill=list(n=0)) %>% 
  split(f = .$Beginnjahr) %>% 
  accumulate(., ~bind_rows(.x, .y)) %>% 
  bind_rows(.id = "frame") 

plot_agg_pn <- kofi_agg_pn %>%
  plot_ly(x=~Beginnjahr, y=~n, color=~`PNR AVOE`) %>% 
  add_lines(frame=~frame, ids=~`PNR AVOE`)

######################### UI ##############################

ui <- bootstrapPage(
  tags$style(type = "text/css", "
	                              html, body {width:100%;height:100%}     
	                             #controls{background-color:white;padding:20px;} "),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, 
             "GIZ Historie", id="nav", 
             
             # Tab of wordcloud:
             tabPanel("Wer sind wir?",
                      wordcloud2::wordcloud2Output("wc_all", height = "750px", width = "100%")
             ),
             
             # Tab of global map         
             tabPanel("Globale Entwicklung", 
                      sidebarLayout(
                        sidebarPanel(width=3, fluid = FALSE,
                                     pickerInput(
                                       "Projektart", "Wähle die Projektart aus", choices = unique(ts_data$`P-Art`), multiple = T, selected = c("STD")),
                                     pickerInput(
                                       "Status", "Wähle den Status des Projekts aus", choices = sort(unique(ts_data$Status)), multiple = T, selected = c(4,5,6)),
                                     pickerInput(
                                       "Auftraggeber", "Wähle den/die Auftraggeber aus", choices = unique(ts_data$Auftraggeber), multiple = T, selected = haupt_ag),
                                     pickerInput(
                                       "Schwerpunkt", "Wähle den/die BMZ-Schwerpunkt(e) aus", choices=unique(ts_data$`BMZ-Schwerpunkt`), multiple = T, selected = bmz_schwerpunkt)
                        ),
                        mainPanel(width=9, fluid = F,
                                  valueBoxOutput("n_proj_box"),
                                  valueBoxOutput("n_kofi"),
                                  valueBoxOutput("max_pn_land"),
                                  valueBoxOutput("max_aw_land"),
                                  leaflet::leafletOutput('globmap', width = "100%",height = "600px"),
                                  absolutePanel(id="controls", style="z-index:500;", class="panel panel-default", 
                                                top=180, left=350, width = 250, fixed=TRUE,
                                                draggable = TRUE, height="auto", 
                                                plotly::plotlyOutput("pn_entwick", height = "200px", width="100%"),
                                                plotly::plotlyOutput("aw_entwick", height="200px", width = "100%"),
                                                sliderInput("Beginnjahr", label = "Beginnjahr", min=min(global_df$Beginnjahr), max=max(global_df$Beginnjahr), value=c(1956,2020))
                                  )
                        )
                      )
             ),
             # Tab Länderportfolio
             navbarMenu("Länderportfolio",
                        ### Tab Panel alle Bereiche       
                        tabPanel("Alle Bereiche",
                                 sidebarLayout(
                                   sidebarPanel(width=3, fluid = FALSE,
                                                pickerInput(
                                                  "Projektart1", "Wähle die Projektart aus", choices = unique(gesamt_data$`P-Art`), multiple = T, selected = c("STD", "DG")),
                                                pickerInput(
                                                  "Status1", "Wähle den Status des Projekts aus", choices = sort(unique(gesamt_data$Status)), multiple = T, selected = c(4,5,6)),
                                                pickerInput(
                                                  "Auftraggeber1", "Wähle den/die Auftraggeber aus", choices = unique(ts_data$Auftraggeber), multiple = T, selected = haupt_ag),
                                                pickerInput(
                                                  "Schwerpunkt1", "Wähle den/die BMZ-Schwerpunkt(e) aus", choices=unique(ts_data$`BMZ-Schwerpunkt`), multiple = T, selected = bmz_schwerpunkt)
                                   ),
                                   mainPanel(width=9, fluid = F,
                                             plotlyOutput("global_pn", height = "100%", width = "100%"),
                                             plotlyOutput("global_aw", height = "100%", width = "100%")
                                   )
                                 )
                        ),
                        tabPanel("Afrika",
                                 sidebarLayout(
                                   sidebarPanel(width=3, fluid = FALSE,
                                                pickerInput(
                                                  "Projektart2", "Wähle die Projektart aus", choices = unique(gesamt_data$`P-Art`), multiple = T, selected = c("STD", "DG")),
                                                pickerInput(
                                                  "Status2", "Wähle den Status des Projekts aus", choices = sort(unique(gesamt_data$Status)), multiple = T, selected = c(4,5,6)),
                                                pickerInput(
                                                  "Auftraggeber2", "Wähle den/die Auftraggeber aus", choices = unique(ts_data$Auftraggeber), multiple = T, selected = haupt_ag),
                                                pickerInput(
                                                  "Schwerpunkt2", "Wähle den/die BMZ-Schwerpunkt(e) aus", choices=unique(ts_data$`BMZ-Schwerpunkt`), multiple = T, selected =bmz_schwerpunkt)
                                   ),
                                   mainPanel(width=9, fluid = F,
                                             leaflet::leafletOutput('africa_map', height = '600px', width = '100%'),
                                             absolutePanel(id="controls", style="z-index:500;", class="panel panel-default", 
                                                           top=100, fixed=TRUE,
                                                           draggable = TRUE, height="auto", 
                                                           sliderInput("Beginnjahr1", label = "Beginnjahr", min=min(ts_data$Beginnjahr), max = max(ts_data$Beginnjahr), value=2020, timeFormat = "%y", animate = animationOptions(interval=3000, loop=FALSE)))
                                   )
                                 )
                        ),
                        tabPanel("Asien",
                                 sidebarLayout(
                                   sidebarPanel(width=3, fluid = FALSE,
                                                pickerInput(
                                                  "Projektart3", "Wähle die Projektart aus", choices = unique(gesamt_data$`P-Art`), multiple = T, selected = c("STD", "DG")),
                                                pickerInput(
                                                  "Status3", "Wähle den Status des Projekts aus", choices = sort(unique(gesamt_data$Status)), multiple = T, selected = c(4,5,6)),
                                                pickerInput(
                                                  "Auftraggeber3", "Wähle den/die Auftraggeber aus", choices = unique(ts_data$Auftraggeber), multiple = T, selected = haupt_ag),
                                                pickerInput(
                                                  "Schwerpunkt3", "Wähle den/die BMZ-Schwerpunkt(e) aus", choices=unique(ts_data$`BMZ-Schwerpunkt`), multiple = T, selected = bmz_schwerpunkt)
                                   ),
                                   mainPanel(width=9, fluid = F,
                                             leaflet::leafletOutput('asia_map', height = '600px', width = '100%'),
                                             absolutePanel(id="controls", style="z-index:500;", class="panel panel-default", 
                                                           top=100, fixed=TRUE,
                                                           draggable = TRUE, height="auto", 
                                                           sliderInput("Beginnjahr2", label = "Beginnjahr", min=min(ts_data$Beginnjahr), max = max(ts_data$Beginnjahr), value=2020, timeFormat = "%y", animate = animationOptions(interval=3000, loop=FALSE)))
                                   )
                                 )
                        ),
                        tabPanel("Amerika",
                                 sidebarLayout(
                                   sidebarPanel(width=3, fluid = FALSE,
                                                pickerInput(
                                                  "Projektart4", "Wähle die Projektart aus", choices = unique(gesamt_data$`P-Art`), multiple = T, selected = c("STD", "DG")),
                                                pickerInput(
                                                  "Status4", "Wähle den Status des Projekts aus", choices = sort(unique(gesamt_data$Status)), multiple = T, selected = c(4,5,6)),
                                                pickerInput(
                                                  "Auftraggeber4", "Wähle den/die Auftraggeber aus", choices = unique(ts_data$Auftraggeber), multiple = T, selected = haupt_ag),
                                                pickerInput(
                                                  "Schwerpunkt4", "Wähle den/die BMZ-Schwerpunkt(e) aus", choices=unique(ts_data$`BMZ-Schwerpunkt`), multiple = T, selected = bmz_schwerpunkt)
                                   ),
                                   mainPanel(width=9, fluid = F,
                                             leaflet::leafletOutput('america_map', height = '600px', width = '100%'),
                                             absolutePanel(id="controls", style="z-index:500;", class="panel panel-default", 
                                                           top=100, fixed=TRUE,
                                                           draggable = TRUE, height="auto", 
                                                           sliderInput("Beginnjahr3", label = "Beginnjahr", min=min(ts_data$Beginnjahr), max = max(ts_data$Beginnjahr), value=2020, timeFormat = "%y", animate = animationOptions(interval=3000, loop=FALSE)))
                                   )
                                 )
                        ),
                        tabPanel("Europa",
                                 sidebarLayout(
                                   sidebarPanel(width=3, fluid = FALSE,
                                                pickerInput(
                                                  "Projektart5", "Wähle die Projektart aus", choices = unique(gesamt_data$`P-Art`), multiple = T, selected = c("STD", "DG")),
                                                pickerInput(
                                                  "Status5", "Wähle den Status des Projekts aus", choices = sort(unique(gesamt_data$Status)), multiple = T, selected = c(4,5,6)),
                                                pickerInput(
                                                  "Auftraggeber5", "Wähle den/die Auftraggeber aus", choices = unique(ts_data$Auftraggeber), multiple = T, selected = haupt_ag),
                                                pickerInput(
                                                  "Schwerpunkt5", "Wähle den/die BMZ-Schwerpunkt(e) aus", choices=unique(ts_data$`BMZ-Schwerpunkt`), multiple = T, selected = bmz_schwerpunkt)
                                   ),
                                   mainPanel(width=9, fluid = F,
                                             leaflet::leafletOutput('europe_map', height = '600px', width = '100%'),
                                             absolutePanel(id="controls", style="z-index:500;", class="panel panel-default", 
                                                           top=100, fixed=TRUE,
                                                           draggable = TRUE, height="auto", 
                                                           sliderInput("Beginnjahr4", label = "Beginnjahr", min=min(ts_data$Beginnjahr), max = max(ts_data$Beginnjahr), value=2020, timeFormat = "%y", animate = animationOptions(interval=3000, loop=FALSE)))
                                   )
                                 )
                        )
             ),
             
             # Tab Kofinanzierungen
             navbarMenu("Kofinanzierungen",
                        ### Tab tm:       
                        tabPanel("KoFi-Geber",
                                 # wordcloud2::wordcloud2Output("wc_lp"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     pickerInput(
                                       "Status_lp", "Wähle den Status der KoFi aus", choices = sort(unique(kofi_df$Status)), multiple = T, selected = c(4,5,6)),
                                     pickerInput(
                                       "Bereich_lp", "Wähle den/die Geschäftsbereich(e) aus", choices = sort(unique(kofi_df$Geschäftsbereich)), multiple = T, selected = c(0,1)),
                                     pickerInput(
                                       "FZ", "Wähle die fachl. Zuordnung aus", choices=unique(kofi_df$`Fachliche Zuordnung`), multiple = T, selected = unique(kofi_df$`Fachliche Zuordnung`)),
                                     pickerInput(
                                       "AG_lp", "Wähle den/die Auftraggeber aus", choices = unique(kofi_df$`AG-Name`), multiple = T, selected = unique(kofi_df$`AG-Name`)),
                                     sliderInput("Beginnjahr_lp", label = "Beginnjahr", min=min(kofi_df$Beginnjahr), max = max(kofi_df$Beginnjahr), value=c(2000,2020), timeFormat = "%y")
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Grafik", 
                                                textOutput("kofi_num"),
                                                #valueBoxOutput("kofi_num"),
                                                #valueBox("max_kofi"),
                                                plotly::plotlyOutput("lp_pn"),
                                                plotly::plotlyOutput("lp_aw")
                                       ),
                                       tabPanel("Datensatz",
                                                DT::dataTableOutput("kofi_dt"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("GIZ vernetzt", top = 150,
                                 threejs::globeOutput("globe", width = "100%")
                        ),
                        tabPanel("KoFi Bereichportfolio",
                                 sidebarLayout(
                                   sidebarPanel(width=3, fluid = FALSE,
                                                pickerInput(
                                                  "Status_lp1", "Wähle den Status der KoFi aus", choices = sort(unique(kofi_df$Status)), multiple = T, selected = c(4,5,6)),
                                                pickerInput(
                                                  "Bereich_lp1", "Wähle den/die Geschäftsbereich(e) aus", choices = sort(unique(kofi_df$Geschäftsbereich)), multiple = T, selected = c(0,1)),
                                                pickerInput(
                                                  "FZ1", "Wähle die fachl. Zuordnung aus", choices=unique(kofi_df$`Fachliche Zuordnung`), multiple = T, selected = unique(kofi_df$`Fachliche Zuordnung`)),
                                                pickerInput(
                                                  "AG_lp1", "Wähle den/die Auftraggeber aus", choices = unique(kofi_df$`AG-Name`), multiple = T, selected = unique(kofi_df$`AG-Name`))
                                   ),
                                   mainPanel(width=9, fluid = F,
                                             absolutePanel(id="controls", style="z-index:500;", class="panel panel-default", 
                                                           top=120, left=500,fixed=TRUE,
                                                           draggable = TRUE, height="auto", 
                                                           sliderInput("Beginnjahr_lp3", label = "Beginnjahr", min=min(kofi_df$Beginnjahr), max=max(kofi_df$Beginnjahr), value=c(1956,2020))
                                             ),
                                             plotly::plotlyOutput('kofi_pn', height = '300px', width = '100%'),
                                             plotly::plotlyOutput('kofi_val', height = '600px', width = '100%')
                                   )
                                 )
                        ),
                        tabPanel("KoFi Fluglinien (2 Karten)", height = "100%",
                                 mapdeck::mapdeckOutput('arc_map_1'),
                                 mapdeck::mapdeckOutput('arc_map_2'),
                                 verbatimTextOutput(
                                   outputId = "observed_click"
                                 ),
                                 absolutePanel(id="controls", top=120, left = 20, draggable = T, fixed = T, 
                                               sliderInput("Beginnjahr_lp2", label = "Beginnjahr", min=min(kofi_df$Beginnjahr), max = max(kofi_df$Beginnjahr), value = c(1980,2020))),
                                 pickerInput(
                                   "Status_lp2", "Wähle den Status der KoFi aus", choices = sort(unique(kofi_df$Status)), multiple = T, selected = c(4,5,6)),
                                 pickerInput(
                                   "Bereich_lp2", "Wähle den/die Geschäftsbereich(e) aus", choices = sort(unique(kofi_df$Geschäftsbereich)), multiple = T, selected = c(0,1)),
                                 pickerInput(
                                   "FZ2", "Wähle die fachl. Zuordnung aus", choices=unique(kofi_df$`Fachliche Zuordnung`), multiple = T, selected = unique(kofi_df$`Fachliche Zuordnung`)),
                                 pickerInput(
                                   "AG_lp2", "Wähle den/die Auftraggeber aus", choices = unique(kofi_df$`AG-Name`), multiple = T, selected = unique(kofi_df$`AG-Name`))
                        )
             )
  ))

############################## SERVER ###################################
server = function(input, output, session) {
  
  #Tab Word cloud
  output$wc_all <- renderWordcloud2({
    wordcloud2(dat)
  })
  
  #Tab global map
  
  # Reactive global df
  reactive_ts_data <- reactive({
    ts_data %>% 
      filter(`P-Art` %in% input$Projektart) %>%
      filter(Status %in% input$Status) %>%
      filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2])%>%
      filter(Auftraggeber %in% input$Auftraggeber) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt)
  })
  
  reactive_land_df <- reactive({
    #reactive_land_per_year() %>% 
    #gesamt_data %>%
    #  filter(`P-Art` %in% input$Projektart) %>%
    #  filter(Status %in% input$Status) %>%
    # filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2]) %>%
    reactive_ts_data() %>%  
      #  filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2])%>%
      group_by(Partnerland,Kontinent,Long,Lat,ISO) %>%
      summarise(sum_pn = n(), sum_aw=mean(`Projektwert ("Auftragswert")`, na.rm = T))
    #  filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2])
  })
  
  reactive_mean_pn_aw_land <- reactive({
    countries_avail <- reactive_land_df() %>% filter(ISO %in% land_spdf$ISO2)
    land_spdf_subset <- land_spdf[land_spdf$ISO2 %in% countries_avail$ISO,]
    countries_avail <- countries_avail[match(land_spdf_subset$ISO2, countries_avail$ISO),]
    countries_avail
  })
  
  reactive_global_df <- reactive({
    reactive_ts_data() %>% 
      group_by(Beginnjahr) %>%
      summarise(sum_aw=sum(`Projektwert ("Auftragswert")`, na.rm = TRUE), n=n())
    #filter(Beginnjahr == input$Beginnjahr)
    #filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2])
  })
  
  reactive_world_polygons <- reactive({
    land_spdf[land_spdf$ISO2 %in% reactive_mean_pn_aw_land()$ISO,]
  })
  
  output$n_proj_box <- renderValueBox({
    valueBox(nrow(reactive_ts_data()), "Projekte weltweit")
  })
  
  reactive_kofi_all <- reactive({
    reactive_ts_data() %>%
      filter(`Kofinanzierung Betrag`!=0)
  })
  
  output$n_kofi <- renderValueBox({
    valueBox(nrow(reactive_kofi_all()), "Projekte mit Kofinanzierung")
  })
  
  #  reactive_max_pn_land <- reactive({
  #    reactive_land_df()[which.max(reactive_land_df()$sum_pn),] 
  #  })
  
  #  output$max_pn_land <- renderValueBox({
  #    valueBox(paste0(reactive_max_pn_land()$Partnerland, " ", reactive_max_pn_land()$sum_pn, " Projekte"), "Partnerland mit meisten Projekten")
  #  })
  
  #  reactive_max_aw_land <- reactive({
  #    reactive_land_df()[which.max(reactive_land_df()$sum_aw),] 
  #  })
  
  #  output$max_aw_land <- renderValueBox({
  #    valueBox(paste0(reactive_max_pn_land()$Partnerland, " ", round(reactive_max_aw_land()$sum_aw/1000000,2), " Mio"), "Partnerland mit höchstem Auftragswert")
  #  })
  
  output$globmap <- leaflet::renderLeaflet({
    glob_basemap 
  })
  
  observeEvent({
    input$Projektart 
    input$Status
    input$Beginnjahr
    input$Auftraggeber
    input$Schwerpunkt} , {
      
      leafletProxy("globmap") %>%
        clearShapes() %>%
        addPolygons(data = reactive_world_polygons(),
                    fillColor = ~pal(reactive_mean_pn_aw_land()$sum_aw),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7, 
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE), 
                    label = sprintf(
                      "<strong>%s</strong><br/>%s<br/>%s ",
                      reactive_mean_pn_aw_land()$Partnerland, paste0(round(reactive_mean_pn_aw_land()$sum_aw/1000000,2)," Mio"), paste(reactive_mean_pn_aw_land()$sum_pn, "Projekt(e)")) %>% 
                      lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    })
  
  output$pn_entwick <- plotly::renderPlotly({
    reactive_global_df() %>%
      plot_ly(x=~Beginnjahr, y=~n) %>%
      add_bars() %>%
      layout(xaxis=list(title="Auftragsbeginn",nticks=10), yaxis=list(title="Anzahl Aufträge (p.a.)"))
  })
  
  output$aw_entwick <- plotly::renderPlotly({
    reactive_global_df() %>%
      plot_ly(x=~Beginnjahr, y=~sum_aw) %>%
      add_lines() %>%
      layout(xaxis = list(title = "Auftragsbeginn",nticks=10), yaxis=list(title="Gesamter Auftragswert (p.a.)",nticks=10))
  })
  
  #### Tab Laenderportfolio: 
  
  #a.: Sub-tab nach Kontinent
  reactive_agg_continent_pn_ts <-  reactive({
    ts_data %>% 
      filter(`P-Art` %in% input$Projektart1) %>%
      filter(Status %in% input$Status1)  %>%
      filter(Auftraggeber %in% input$Auftraggeber1) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt1) %>%
      count(Beginnjahr, Kontinent) %>%
      complete(Kontinent, Beginnjahr, fill=list(n=0)) %>% 
      split(f = .$Beginnjahr) %>% 
      accumulate(., ~bind_rows(.x, .y)) %>% 
      bind_rows(.id = "frame") 
  })
  
  output$global_pn <- renderPlotly({
    reactive_agg_continent_pn_ts() %>% 
      plot_ly(x=~Beginnjahr, y=~n, color=~Kontinent) %>% 
      add_lines(frame=~frame, ids=~Kontinent) %>%
      layout(yaxis = list(title = "Anzahl Auträge (p.a.)"))
  })
  
  #Entwicklung der AW in jedem Jahr:
  reactive_ts_data_aw <- reactive({
    ts_data %>%
      filter(`P-Art` %in% input$Projektart1) %>%
      filter(Status %in% input$Status1) %>%
      filter(Auftraggeber %in% input$Auftraggeber1) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt1)
  })
  
  reactive_agg_continent_aw_ts <- reactive({
    aggregate(`Projektwert ("Auftragswert")`~Beginnjahr+Kontinent, data=reactive_ts_data_aw(), sum) %>%
      complete(Kontinent, Beginnjahr, fill=list(n=0)) %>% 
      split(f = .$Beginnjahr) %>% 
      accumulate(., ~bind_rows(.x, .y)) %>% 
      bind_rows(.id = "frame") 
  })
  
  output$global_aw <- renderPlotly({
    reactive_agg_continent_aw_ts() %>%
      replace_na(list(`Projektwert ("Auftragswert")` = 0)) %>%
      plot_ly(x=~Beginnjahr, y=~`Projektwert ("Auftragswert")`, color=~Kontinent) %>%
      add_lines(frame=~frame, ids=~Kontinent) %>%
      layout(xaxis=list(nticks=10), yaxis=list(title= "Gesamter Auftragswert (p.a.)", nticks=10))
  })
  
  ### b. Sub-Tab: Afrika
  
  reactive_africa_ts <- reactive({
    ts_data %>% 
      filter(Kontinent == "Afrika") %>%
      filter(`P-Art` %in% input$Projektart2) %>%
      filter(Status %in% input$Status2) %>%
      filter(Beginnjahr == input$Beginnjahr1)%>%
      filter(Auftraggeber %in% input$Auftraggeber2) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt2)
  })
  
  reactive_africa_df <- reactive({
    reactive_africa_ts() %>%  
      #  filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2])%>%
      group_by(Beginnjahr, Partnerland,Kontinent,Long,Lat,ISO) %>%
      summarise(n = n(), sum_aw=sum(`Projektwert ("Auftragswert")`, na.rm = T))
    #  filter(Beginnjahr >= input$Beginnjahr[1] & Beginnjahr <= input$Beginnjahr[2])
  })
  
  reactive_mean_pn_aw_africa <- reactive({
    countries_africa <- reactive_africa_df() %>% filter(ISO %in% land_spdf$ISO2)
    africa_spdf_subset <- land_spdf[land_spdf$ISO2 %in% countries_africa$ISO,]
    countries_africa <- countries_africa[match(africa_spdf_subset$ISO2, countries_africa$ISO),]
    countries_africa
  })
  
  reactive_africa_polygons <- reactive({
    land_spdf[land_spdf$ISO2 %in% reactive_mean_pn_aw_africa()$ISO,]
  })
  
  output$africa_map <- leaflet::renderLeaflet({
    africa_basemap
  })
  
  observeEvent({
    input$Projektart2 
    input$Status2
    input$Beginnjahr1
    input$Auftraggeber2
    input$Schwerpunkt2} , {
      
      leafletProxy("africa_map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(data =reactive_africa_polygons(), stroke = F, smoothFactor = 0.1, opacity = 1, fillOpacity = 0.5, fillColor = ~pal_africa(reactive_mean_pn_aw_africa()$sum_aw)) %>%
        addCircleMarkers(data=reactive_africa_df(), lng = ~Long, lat=~Lat, radius=~n, fillOpacity = 0.3,
                         label = paste0(reactive_africa_df()$Partnerland, ": ", paste0(round(reactive_africa_df()$sum_aw/1000000,2), " Mio"), ", ", paste0(reactive_africa_df()$n, " Projekt(e)")),
                         #label = sprintf(
                         #"<strong>%s</strong><br/>%s<br/>%s ",
                         #reactive_africa_df()$Partnerland, paste0(round(reactive_africa_df()$sum_aw/1000000,2)," Mio"), paste(reactive_africa_df()$n, "Projekt(e)")) %>% 
                         #lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"))
    })
  
  ###c. Subtab: Asien
  reactive_asia_ts <- reactive({
    ts_data %>% 
      filter(Kontinent == "Asien") %>%
      filter(`P-Art` %in% input$Projektart3) %>%
      filter(Status %in% input$Status3) %>%
      filter(Beginnjahr == input$Beginnjahr2)%>%
      filter(Auftraggeber %in% input$Auftraggeber3) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt3)
  })
  
  reactive_asia_df <- reactive({
    reactive_asia_ts() %>%  
      group_by(Beginnjahr, Partnerland,Kontinent,Long,Lat,ISO) %>%
      summarise(n = n(), sum_aw=sum(`Projektwert ("Auftragswert")`, na.rm = T))
  })
  
  reactive_mean_pn_aw_asia <- reactive({
    countries_asia <- reactive_asia_df() %>% filter(ISO %in% land_spdf$ISO2)
    asia_spdf_subset <- land_spdf[land_spdf$ISO2 %in% countries_asia$ISO,]
    countries_asia <- countries_asia[match(asia_spdf_subset$ISO2, countries_asia$ISO),]
    countries_asia
  })
  
  reactive_asia_polygons <- reactive({
    land_spdf[land_spdf$ISO2 %in% reactive_mean_pn_aw_asia()$ISO,]
  })
  
  output$asia_map <- leaflet::renderLeaflet({
    asia_basemap
  })
  
  observeEvent({
    input$Projektart3 
    input$Status3
    input$Beginnjahr2
    input$Auftraggeber3
    input$Schwerpunkt3} , {
      
      leafletProxy("asia_map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(data=reactive_asia_polygons(), stroke = F, smoothFactor = 0.1, opacity = 1, fillOpacity = 0.5, fillColor = ~pal_asia(reactive_mean_pn_aw_asia()$sum_aw)) %>%
        addCircleMarkers(data=reactive_asia_df(), lng = ~Long, lat=~Lat, radius=~n, fillOpacity = 0.3,
                         label = paste0(reactive_asia_df()$Partnerland, ": ", paste0(round(reactive_asia_df()$sum_aw/1000000,2), " Mio"), ", ", paste0(reactive_asia_df()$n, " Projekt(e)")),
                         #label = sprintf(
                         #"<strong>%s</strong><br/>%s<br/>%s ",
                         #reactive_africa_df()$Partnerland, paste0(round(reactive_africa_df()$sum_aw/1000000,2)," Mio"), paste(reactive_africa_df()$n, "Projekt(e)")) %>% 
                         #lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"))
    })
  
  ###d.Subtab: Amerika
  reactive_america_ts <- reactive({
    ts_data %>% 
      filter(Kontinent == "Amerika") %>%
      filter(`P-Art` %in% input$Projektart4) %>%
      filter(Status %in% input$Status4) %>%
      filter(Beginnjahr == input$Beginnjahr3)%>%
      filter(Auftraggeber %in% input$Auftraggeber4) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt4)
  })
  
  reactive_america_df <- reactive({
    reactive_america_ts() %>%  
      group_by(Beginnjahr, Partnerland,Kontinent,Long,Lat,ISO) %>%
      summarise(n = n(), sum_aw=sum(`Projektwert ("Auftragswert")`, na.rm = T))
  })
  
  reactive_mean_pn_aw_america <- reactive({
    countries_america <- reactive_america_df() %>% filter(ISO %in% land_spdf$ISO2)
    america_spdf_subset <- land_spdf[land_spdf$ISO2 %in% countries_america$ISO,]
    countries_america <- countries_america[match(america_spdf_subset$ISO2, countries_america$ISO),]
    countries_america
  })
  
  reactive_america_polygons <- reactive({
    land_spdf[land_spdf$ISO2 %in% reactive_mean_pn_aw_america()$ISO,]
  })
  
  output$america_map <- leaflet::renderLeaflet({
    america_basemap
  })
  
  observeEvent({
    input$Projektart4 
    input$Status4
    input$Beginnjahr3
    input$Auftraggeber4
    input$Schwerpunkt4} , {
      
      leafletProxy("america_map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(data =reactive_america_polygons(), stroke = F, smoothFactor = 0.1, opacity = 1, fillOpacity = 0.5, fillColor = ~pal_america(reactive_mean_pn_aw_america()$sum_aw)) %>%
        addCircleMarkers(data=reactive_america_df(), lng = ~Long, lat=~Lat, radius=~n, fillOpacity = 0.3,
                         label = paste0(reactive_america_df()$Partnerland, ": ", paste0(round(reactive_america_df()$sum_aw/1000000,2), " Mio"), ", ", paste0(reactive_america_df()$n, " Projekt(e)")),
                         #label = sprintf(
                         #"<strong>%s</strong><br/>%s<br/>%s ",
                         #reactive_africa_df()$Partnerland, paste0(round(reactive_africa_df()$sum_aw/1000000,2)," Mio"), paste(reactive_africa_df()$n, "Projekt(e)")) %>% 
                         #lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"))
    })
  
  ###e.Subtab: Europe
  reactive_europe_ts <- reactive({
    ts_data %>% 
      filter(Kontinent == "Europa") %>%
      filter(`P-Art` %in% input$Projektart5) %>%
      filter(Status %in% input$Status5) %>%
      filter(Beginnjahr == input$Beginnjahr4)%>%
      filter(Auftraggeber %in% input$Auftraggeber5) %>%
      filter(`BMZ-Schwerpunkt` %in% input$Schwerpunkt5)
  })
  
  reactive_europe_df <- reactive({
    reactive_europe_ts() %>%  
      group_by(Beginnjahr, Partnerland,Kontinent,Long,Lat,ISO) %>%
      summarise(n = n(), sum_aw=sum(`Projektwert ("Auftragswert")`, na.rm = T))
  })
  
  reactive_mean_pn_aw_europe <- reactive({
    countries_europe <- reactive_europe_df() %>% filter(ISO %in% land_spdf$ISO2)
    europe_spdf_subset <- land_spdf[land_spdf$ISO2 %in% countries_europe$ISO,]
    countries_europe <- countries_europe[match(europe_spdf_subset$ISO2, countries_europe$ISO),]
    countries_europe
  })
  
  reactive_europe_polygons <- reactive({
    land_spdf[land_spdf$ISO2 %in% reactive_mean_pn_aw_europe()$ISO,]
  })
  
  output$europe_map <- leaflet::renderLeaflet({
    europe_basemap
  })
  
  observeEvent({
    input$Projektart5 
    input$Status5
    input$Beginnjahr4
    input$Auftraggeber5
    input$Schwerpunkt5} , {
      
      leafletProxy("europe_map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(data =reactive_europe_polygons(), stroke = F, smoothFactor = 0.1, opacity = 1, fillOpacity = 0.5, fillColor = ~pal_europe(reactive_mean_pn_aw_europe()$sum_aw)) %>%
        addCircleMarkers(data=reactive_europe_df(), lng = ~Long, lat=~Lat, radius=~n, fillOpacity = 0.3,
                         label = paste0(reactive_europe_df()$Partnerland, ": ", paste0(round(reactive_europe_df()$sum_aw/1000000,2), " Mio"), ", ", paste0(reactive_europe_df()$n, " Projekt(e)")),
                         #label = sprintf(
                         #"<strong>%s</strong><br/>%s<br/>%s ",
                         #reactive_africa_df()$Partnerland, paste0(round(reactive_africa_df()$sum_aw/1000000,2)," Mio"), paste(reactive_africa_df()$n, "Projekt(e)")) %>% 
                         #lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"))
    })
  
  ## Tab: Kofi
  ####a. sub-tab: kofi_geber
  reactive_kofi_geber <- reactive({
    kofi_df %>%
      filter(Status %in% input$Status_lp) %>%
      filter(Geschäftsbereich %in% input$Bereich_lp) %>%
      filter(`AG-Name` %in% input$AG_lp) %>%
      filter(`Fachliche Zuordnung` %in% input$FZ)%>%
      filter(Beginnjahr >= input$Beginnjahr_lp[1] & Beginnjahr <= input$Beginnjahr_lp[2]) %>%
      group_by(KoFiGeber) %>%
      summarise(n=n_distinct(KoFiID, na.rm = T), sum_val = sum(Betrag, na.rm = T))
  })
  
  reactive_kofi_num <- reactive({
    kofi_df %>%
      filter(Status %in% input$Status_lp) %>%
      filter(Geschäftsbereich %in% input$Bereich_lp) %>%
      filter(`AG-Name` %in% input$AG_lp) %>%
      filter(`Fachliche Zuordnung` %in% input$FZ)%>%
      filter(Beginnjahr >= input$Beginnjahr_lp[1] & Beginnjahr <= input$Beginnjahr_lp[2]) %>%
      group_by(KoFiID) %>%
      summarise(kofi_num = n())
  })
  
  output$kofi_num <- renderText({
    paste(nrow(reactive_kofi_num()), "KoFi's")
  })
  
  output$lp_pn <- renderPlotly({
    reactive_kofi_geber() %>%
      plot_ly(x=~KoFiGeber, y=~n) %>%
      add_bars() %>%
      layout(xaxis = list(title = "KoFi-Geber"), yaxis=list(title="Anzahl KoFi'S",nticks=10))
  })
  
  output$lp_aw <- renderPlotly({
    reactive_kofi_geber() %>%
      plot_ly(x=~KoFiGeber, y=~sum_val) %>%
      add_bars() %>%
      layout(xaxis = list(title = "KoFi-Geber"), yaxis=list(title="Summe KoFi-Betrag",nticks=10))
  })
  
  output$kofi_dt <- DT::renderDataTable({
    kofi_df %>% 
      filter(Status %in% input$Status_lp) %>%
      filter(Geschäftsbereich %in% input$Bereich_lp) %>%
      filter(`AG-Name` %in% input$AG_lp) %>%
      filter(`Fachliche Zuordnung` %in% input$FZ)%>%
      filter(Beginnjahr >= input$Beginnjahr_lp[1] & Beginnjahr <= input$Beginnjahr_lp[2])
    #select(`*Projekt-LP`, `Drittmittel-Geber kurz`, Partnerland, Einsatzland, Kontinent, Betrag, Beginnjahr, `Ende-Ist`, Status, Geschäereich, `AG-Name`, `Vertragstyp Knz.`, `Fachliche Zuordnung`)
  })
  
  ##b. Subtab: KoFi vernetzt
  output$globe <- threejs::renderGlobe({
    globe
  })
  
  output$wc_lp <- renderWordcloud2({
    wordcloud2(dat_lp)
  })
  
  ## c. subtap: Kofi nach Kontinent
  reactive_kofi_ts <- reactive({
    kofi_df %>% 
      filter(Status %in% input$Status_lp1) %>%
      filter(Geschäftsbereich %in% input$Bereich_lp1) %>%
      filter(`AG-Name` %in% input$AG_lp1) %>%
      filter(`Fachliche Zuordnung` %in% input$FZ1)
  })
  
  output$kofi_pn <- renderPlotly({
    reactive_kofi_ts() %>%
      filter(Beginnjahr >= input$Beginnjahr_lp3[1] & Beginnjahr <= input$Beginnjahr_lp3[2])%>%
      group_by(Kontinent)%>%
      summarise(n=n_distinct(KoFiID)) %>%
      plot_ly(x=~Kontinent, y=~n, color=~Kontinent) %>%
      add_bars() %>%
      layout(yaxis=list(title="Gesamte Anzahl KoFi's", nticks=10))
  })
  
  #Entwicklung der AW in jedem Jahr:
  
  reactive_kofi_agg_val <- reactive({
    aggregate(Betrag~Beginnjahr+Kontinent, data=reactive_kofi_ts(), sum) %>%
      complete(Kontinent, Beginnjahr, fill=list(n=0)) %>% 
      split(f = .$Beginnjahr) %>% 
      accumulate(., ~bind_rows(.x, .y)) %>% 
      bind_rows(.id = "frame") 
  })
  
  output$kofi_val <- renderPlotly({
    reactive_kofi_agg_val() %>%
      replace_na(list(Betrag = 0)) %>%
      plot_ly(x=~Beginnjahr, y=~Betrag, color=~Kontinent) %>%
      add_lines(frame=~frame, ids=~Kontinent) %>%
      layout(xaxis=list(nticks=10), yaxis=list(title = "KoFi-Betrag (p.a.)",nticks=10))
  })
  
  ###d. Sub-tab: arc_map (Abflug d.g Flüge nach Deutschland)
  reactive_arc_df_1  <- reactive({
    kofi_df %>% 
      filter(Status %in% input$Status_lp2) %>%
      filter(Geschäftsbereich %in% input$Bereich_lp2) %>%
      filter(`AG-Name` %in% input$AG_lp2) %>%
      filter(`Fachliche Zuordnung` %in% input$FZ2)%>%
      #select(`Geber Land`, KoFiGeber, Long_Orig, Lat_Orig, Partnerland, Long_Desti, Lat_Desti, Betrag, Beginnjahr, Long_DE, Lat_DE, DE) %>%
      filter(!is.na(Long_Orig))  %>%
      filter(!is.na(Long_Desti)) %>%
      filter(!is.na(Beginnjahr)) %>%
      filter(!is.na(Betrag)) %>%
      filter(Beginnjahr >= input$Beginnjahr_lp2[1] & Beginnjahr <= input$Beginnjahr_lp2[2])%>%
      group_by(Beginnjahr, `Geber Land`, KoFiGeber, Long_Orig, Lat_Orig, DE, Long_DE, Lat_DE) %>%
      summarise(sum_betrag = sum(Betrag, na.rm = T)) %>%
      mutate(Info = paste0(`Geber Land`, " :", KoFiGeber, " - Deutschland", "; Betrag: ", round(sum_betrag/1000000, 2), " Mio"))
  })
  
  output$arc_map_1 <- renderMapdeck({
    base_mapdeck
  })
  
  observeEvent({
    input$Beginnjahr_lp2
    input$Status_lp2
    input$Bereich_lp2
    input$AG_lp2
    input$FZ2
  }, {
    mapdeck_update(map_id = 'arc_map_1') %>%
      add_arc(
        data = reactive_arc_df_1(),
        layer_id = "arc_layer",
        origin = c("Long_Orig", "Lat_Orig"),
        destination = c("Long_DE", "Lat_DE"),
        stroke_from = "Geber Land",
        stroke_to = "DE",
        stroke_width = 4,
        auto_highlight = TRUE,
        tooltip = "Info",
        update_view = FALSE
      )
  })
  
  ###d. Sub-tab: arc_map (Abflug d.g Flüge nach Deutschland)
  reactive_arc_df_2  <- reactive({
    kofi_df %>% 
      filter(Status %in% input$Status_lp2) %>%
      filter(Geschäftsbereich %in% input$Bereich_lp2) %>%
      filter(`AG-Name` %in% input$AG_lp2) %>%
      filter(`Fachliche Zuordnung` %in% input$FZ2)%>%
      #select(`Geber Land`, KoFiGeber, Long_Orig, Lat_Orig, Partnerland, Long_Desti, Lat_Desti, Betrag, Beginnjahr, Long_DE, Lat_DE, DE) %>%
      filter(!is.na(Long_Orig))  %>%
      filter(!is.na(Long_Desti)) %>%
      filter(!is.na(Beginnjahr)) %>%
      filter(!is.na(Betrag)) %>%
      filter(Beginnjahr >= input$Beginnjahr_lp2[1] & Beginnjahr <= input$Beginnjahr_lp2[2])%>%
      group_by(Beginnjahr, Partnerland, Long_Desti, Lat_Desti, DE, Long_DE, Lat_DE) %>%
      summarise(sum_betrag = sum(Betrag, na.rm = T)) %>%
      mutate(Info = paste0("Deutschland - ",Partnerland, "; Betrag: ", round(sum_betrag/1000000, 2), " Mio"))
  })
  
  output$arc_map_2 <- renderMapdeck({
    base_mapdeck
  })
  
  observeEvent({
    input$Beginnjahr_lp2
    input$Status_lp2
    input$Bereich_lp2
    input$AG_lp2
    input$FZ2
  }, {
    mapdeck_update(map_id = 'arc_map_2') %>%
      add_animated_arc(
        data = reactive_arc_df_2(),
        layer_id = "arc_layer",
        origin = c("Long_DE", "Lat_DE"),
        destination = c("Long_Desti", "Lat_Desti"),
        stroke_from = "DE",
        stroke_to = "Partnerland",
        stroke_width = 4,
        auto_highlight = TRUE,
        tooltip = "Info",
        update_view = FALSE
      )
  })
  
}

shinyApp(ui, server)
