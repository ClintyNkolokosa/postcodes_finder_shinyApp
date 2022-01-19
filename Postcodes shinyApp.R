library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(sf)
library(leafpop)
library(leaflet.extras)
library(DT)
library(htmltools)
library(shinycssloaders)

#setwd("C:/R/Postcode finder/Postcode finder")

here::here()

mw_postcodes <- read.csv("data/Malawi postcodes.csv") |> 
  tibble()
postcodes_df <- st_read("data/postcodes_sf.shp")
saveRDS(postcodes_df, "data/postcodes.rds")

postcodes_data <- read_rds(here::here("data/postcodes.rds"))

postcodes_data$POSTCODE_2 <- match(postcodes_data$LOCATION, mw_postcodes$LOCATION)

north <- postcodes_data |>
  filter(ADM1_EN == "Northern")
# add double quotes and comma to names of places
cat(sprintf('"%s",', north$LOCATION), sep = "\n")

central <- postcodes_data |> 
  filter(ADM1_EN == "Central")
cat(sprintf('"%s",', central$LOCATION), sep = "\n")

south <- postcodes_data |> 
  filter(ADM1_EN == "Southern")
cat(sprintf('"%s",', south$LOCATION), sep = "\n")

getColor <- function(postcodes_data){
  sapply(postcodes_data$ADM1_EN, function(ADM1_EN){
    if(ADM1_EN == "Central"){
      "green"
    } else if(ADM1_EN == "Southern"){
      "Orange"
    } else {
      "navy"
    }
  })
}

icons <- awesomeIcons(icon = 'ios-Close',
                      iconColor = 'black',
                      library = 'ion',
                      markerColor = getColor(postcodes_data))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 1, right = 10, style = "z-index:500; text-align: right;",
                tags$h2("Malawi Postcodes Finder"),
                tags$a("Clinton Nkolokosa",
                       href = "https://rpubs.com/Clinty")
  ),
  
  absolutePanel(fixed = TRUE,
                top = 50, left = 10, right = "auto", draggable = TRUE, 
                bottom = "auto", width = 300, height = "auto",
                style = "z-index:500; min-width: 300px; text-align: left;",
                h4(textOutput("reactive_postcode"), align = "left"),
                div(pickerInput("postcodes", label = "Search Location:",
                            width = "auto", inline = TRUE,
                            choices = list("All postcodes", 
                                           `Northern Region` = c("Chasefu Ward",
                                                                 "Chibanja Ward",
                                                                 "Chiputula Ward",
                                                                 "Chitipa Boma",
                                                                 "Jombo Ward",
                                                                 "Kaning'ina Ward",
                                                                 "Karonga Town",
                                                                 "Katawa Ward",
                                                                 "Katoto Ward",
                                                                 "Likoma Boma",
                                                                 "Lupaso Ward",
                                                                 "Masasa Ward",
                                                                 "Mchengautuwa Ward",
                                                                 "Msongwe Ward",
                                                                 "Muzilawayingwe Ward",
                                                                 "Mzimba Boma",
                                                                 "New Airport Site Ward",
                                                                 "Nkhata Bay Boma",
                                                                 "Nkhorongo Ward",
                                                                 "Nyika National Park - Chitipa",
                                                                 "Nyika National Park - Karonga",
                                                                 "Nyika National Park - Rumphi",
                                                                 "Rumphi Boma",
                                                                 "SC Chapinduka",
                                                                 "SC Fukamalaza",
                                                                 "SC Jaravikuba Munthali",
                                                                 "SC Kachulu",
                                                                 "SC Kampingo Sibande",
                                                                 "SC Khosolo Gwaza Jere",
                                                                 "SC Malanda",
                                                                 "SC Mkondowe",
                                                                 "SC Mkumbira",
                                                                 "SC Mwahenga",
                                                                 "SC Mwakaboko",
                                                                 "SC Mwalweni",
                                                                 "SC Mwankhunikira",
                                                                 "SC Mwirang'ombe",
                                                                 "SC Nyaluwanga",
                                                                 "SC Zilakoma",
                                                                 "TA Boghoyo",
                                                                 "TA Chikulamayembe",
                                                                 "TA Chindi",
                                                                 "TA Fukamapiri",
                                                                 "TA Kabunduli",
                                                                 "TA Kameme",
                                                                 "TA Katumbi",
                                                                 "TA Kilupula",
                                                                 "TA Kyungu",
                                                                 "TA M'Mbelwa",
                                                                 "TA Mabulabo",
                                                                 "TA Malenga Mzoma",
                                                                 "TA Mankhambira",
                                                                 "TA Mkumpha",
                                                                 "TA Mpherembe",
                                                                 "TA Mtwalo",
                                                                 "TA Musisya",
                                                                 "TA Mwabulambya",
                                                                 "TA Mwamlowe",
                                                                 "TA Mwenemisuku",
                                                                 "TA Mwenewenya",
                                                                 "TA Mzikubola",
                                                                 "TA Mzukuzuku",
                                                                 "TA Nthalire",
                                                                 "TA Timbiri",
                                                                 "TA Wasambo",
                                                                 "TA Zolokere",
                                                                 "Viphya Ward",
                                                                 "Vwaza Marsh Reserve - Mzimba",
                                                                 "Vwaza Marsh Reserve - Rumphi",
                                                                 "Zolozolo Ward"),
                                           `Central Region` = c("Area 1",
                                                                "Area 10",
                                                                "Area 11",
                                                                "Area 12",
                                                                "Area 13",
                                                                "Area 14",
                                                                "Area 15",
                                                                "Area 16",
                                                                "Area 17",
                                                                "Area 18",
                                                                "Area 19",
                                                                "Area 2",
                                                                "Area 20",
                                                                "Area 21",
                                                                "Area 22",
                                                                "Area 23",
                                                                "Area 24",
                                                                "Area 25",
                                                                "Area 26",
                                                                "Area 27",
                                                                "Area 28",
                                                                "Area 29",
                                                                "Area 3",
                                                                "Area 30",
                                                                "Area 31",
                                                                "Area 32",
                                                                "Area 33",
                                                                "Area 34",
                                                                "Area 35",
                                                                "Area 36",
                                                                "Area 37",
                                                                "Area 38",
                                                                "Area 39",
                                                                "Area 4",
                                                                "Area 40",
                                                                "Area 41",
                                                                "Area 42",
                                                                "Area 43",
                                                                "Area 44",
                                                                "Area 45",
                                                                "Area 46",
                                                                "Area 47",
                                                                "Area 48",
                                                                "Area 49",
                                                                "Area 5",
                                                                "Area 50",
                                                                "Area 51",
                                                                "Area 52",
                                                                "Area 53",
                                                                "Area 54",
                                                                "Area 55",
                                                                "Area 56",
                                                                "Area 57",
                                                                "Area 58",
                                                                "Area 6",
                                                                "Area 7",
                                                                "Area 8",
                                                                "Area 9",
                                                                "Dedza Boma",
                                                                "Dowa Boma",
                                                                "Kasungu Boma",
                                                                "Kasungu National Park",
                                                                "Lake Malawi Natl. Park - Salima",
                                                                "Mchinji Boma",
                                                                "Nkhotakota Boma",
                                                                "Nkhotakota Game Reserve",
                                                                "Ntcheu Boma",
                                                                "Ntchisi Boma",
                                                                "Salima Town",
                                                                "SC Chakhaza",
                                                                "SC Champiti",
                                                                "SC Chauma",
                                                                "SC Chilikumwendo",
                                                                "SC Chilooko",
                                                                "SC Chilowamatambe",
                                                                "SC Chisikwa",
                                                                "SC Chitekwele",
                                                                "SC Dambe",
                                                                "SC Goodson Ganya",
                                                                "SC Kafuzila",
                                                                "SC Kambalame",
                                                                "SC Kambwiri",
                                                                "SC Kamenya Gwaza",
                                                                "SC Kawamba",
                                                                "SC Kayembe",
                                                                "SC Lukwa",
                                                                "SC M'nyanja",
                                                                "SC Makwangwala",
                                                                "SC Mavwere",
                                                                "SC Mduwa",
                                                                "SC Mkukula",
                                                                "SC Mphonde",
                                                                "SC Mponela",
                                                                "SC Msosa",
                                                                "SC Mtema",
                                                                "SC Mwansambo",
                                                                "SC Mwanza",
                                                                "SC Njewa",
                                                                "SC Njombwa",
                                                                "SC Nthondo",
                                                                "SC Simlemba",
                                                                "SC Tsabango",
                                                                "TA Chadza",
                                                                "TA Chakhumbira",
                                                                "TA Chikho",
                                                                "TA Chimutu",
                                                                "TA Chiseka",
                                                                "TA Chitukula",
                                                                "TA Chiwere",
                                                                "TA Chulu",
                                                                "TA Dzoole",
                                                                "TA Kabudula",
                                                                "TA Kachindamoto",
                                                                "TA Kalolo",
                                                                "TA Kaluluma",
                                                                "TA Kalumba",
                                                                "TA Kalumbu",
                                                                "TA Kalumo",
                                                                "TA Kanyenda",
                                                                "TA Kaomba",
                                                                "TA Kapelula",
                                                                "TA Kaphuka",
                                                                "TA Karonga",
                                                                "TA Kasakula",
                                                                "TA Kasumbu",
                                                                "TA Khombedza",
                                                                "TA Khongoni",
                                                                "TA Kuluunda",
                                                                "TA Kwataine",
                                                                "TA Maganga",
                                                                "TA Malenga Chanzi",
                                                                "TA Malili",
                                                                "TA Masasa",
                                                                "TA Mazengera",
                                                                "TA Mkanda",
                                                                "TA Mlonyeni",
                                                                "TA Mpando",
                                                                "TA Msakambewa",
                                                                "TA Mwadzama",
                                                                "TA Mwase",
                                                                "TA Ndindi",
                                                                "TA Njolomole",
                                                                "TA Pemba",
                                                                "TA Pemba",
                                                                "TA Phambala",
                                                                "TA Santhe",
                                                                "TA Tambala",
                                                                "TA Wimbe",
                                                                "TA Zulu"),
                                           `Southern Region` = c("Balaka Town",
                                                                 "Bangwe Ward",
                                                                 "Blantyre Central Ward",
                                                                 "Blantyre East Ward",
                                                                 "Blantyre West Ward",
                                                                 "Chambo Ward",
                                                                 "Chichiri Ward",
                                                                 "Chigumula Ward",
                                                                 "Chikamveka North Ward",
                                                                 "Chikamveka Ward",
                                                                 "Chikwawa Boma",
                                                                 "Chilomoni Ward",
                                                                 "Chiradzulu Boma",
                                                                 "Chirunga East Ward",
                                                                 "Chirunga Ward",
                                                                 "Lake Chilwa",
                                                                 "Lake Chilwa",
                                                                 "Lake Chiuta",
                                                                 "Lake Malawi Natl. Park - Mangochi",
                                                                 "Lake Malombe",
                                                                 "Lengwe National Park",
                                                                 "Likangala Central Ward",
                                                                 "Likangala South Ward",
                                                                 "Likangala Ward",
                                                                 "Likhubula Ward",
                                                                 "Limbe Central Ward",
                                                                 "Limbe East Ward",
                                                                 "Limbe West Ward",
                                                                 "Liwonde National Park",
                                                                 "Liwonde Town",
                                                                 "Luchenza Town",
                                                                 "Machinga Boma",
                                                                 "Majete Game Reserve - Chikwawa",
                                                                 "Majete Game Reserve - Neno",
                                                                 "Mangochi Town",
                                                                 "Mapanga Ward",
                                                                 "Masongola Ward",
                                                                 "Mbedza Ward",
                                                                 "Michiru Ward",
                                                                 "Misesa Ward",
                                                                 "Monkey Bay Urban",
                                                                 "Msamba Ward",
                                                                 "Mtiya Ward",
                                                                 "Mulanje Boma",
                                                                 "Mulanje Mountain Reserve",
                                                                 "Mwabvi Game Reserve",
                                                                 "Mwanza Boma",
                                                                 "Mzedi Ward",
                                                                 "Namiyango Ward",
                                                                 "Nancholi Ward",
                                                                 "Ndirande North Ward",
                                                                 "Ndirande South Ward",
                                                                 "Ndirande West Ward",
                                                                 "Neno Boma",
                                                                 "Nkolokoti Ward",
                                                                 "Nsanje Boma",
                                                                 "Nyambadwe Ward",
                                                                 "Phalombe Boma",
                                                                 "Sadzi Ward",
                                                                 "SC Chamba",
                                                                 "SC Chikweo",
                                                                 "SC Chiwalo",
                                                                 "SC Chowe",
                                                                 "SC Juma",
                                                                 "SC Kwethemule",
                                                                 "SC Laston Njema",
                                                                 "SC Makoko",
                                                                 "SC Mbawela",
                                                                 "SC Mbenje",
                                                                 "SC Mbiza",
                                                                 "SC Mbwana Nyambi",
                                                                 "SC Mkumbira",
                                                                 "SC Mlomba",
                                                                 "SC Mphuka",
                                                                 "SC Mposa",
                                                                 "SC Namabvi",
                                                                 "SC Ngokwe",
                                                                 "SC Sitola",
                                                                 "SC Thukuta",
                                                                 "Soche East Ward",
                                                                 "Soche West Ward",
                                                                 "South Lunzu Ward",
                                                                 "TA Bvumbwe",
                                                                 "TA Changata",
                                                                 "TA Chapananga",
                                                                 "TA Chigaru",
                                                                 "TA Chikowi",
                                                                 "TA Chikumbu",
                                                                 "TA Chimaliro",
                                                                 "TA Chimombo",
                                                                 "TA Chimwala",
                                                                 "TA Chitera",
                                                                 "TA Chiwalo",
                                                                 "TA Dambe",
                                                                 "TA Jalasi",
                                                                 "TA Kadewere",
                                                                 "TA Kalembo",
                                                                 "TA Kanduku",
                                                                 "TA Kapeni",
                                                                 "TA Kapichi",
                                                                 "TA Kasisi",
                                                                 "TA Katuli",
                                                                 "TA Katunga",
                                                                 "TA Kawinga",
                                                                 "TA Kuntaja",
                                                                 "TA Kunthembwe",
                                                                 "TA Kuntumanji",
                                                                 "TA Likoswe",
                                                                 "TA Liwonde",
                                                                 "TA Lundu",
                                                                 "TA Lundu",
                                                                 "TA Mabuka",
                                                                 "TA Machinjili",
                                                                 "TA Makanjila",
                                                                 "TA Makata",
                                                                 "TA Makhwira",
                                                                 "TA Malemia",
                                                                 "TA Malemia",
                                                                 "TA Maseya",
                                                                 "TA Mkhumba",
                                                                 "TA Mlauli",
                                                                 "TA Mlolo",
                                                                 "TA Mlumbe",
                                                                 "TA Mpama",
                                                                 "TA Mponda",
                                                                 "TA Mwambo",
                                                                 "TA Nankumba",
                                                                 "TA Nazombe",
                                                                 "TA Nchema",
                                                                 "TA Nchilamwela",
                                                                 "TA Ndamera",
                                                                 "TA Ngabu",
                                                                 "TA Ngabu",
                                                                 "TA Ngozi",
                                                                 "TA Nkalo",
                                                                 "TA Nkanda",
                                                                 "TA Nsabwe",
                                                                 "TA Nsamala",
                                                                 "TA Nthache",
                                                                 "TA Nthiramanja",
                                                                 "TA Nyachikadza",
                                                                 "TA Nyambi",
                                                                 "TA Somba",
                                                                 "TA Symon",
                                                                 "TA Tengani",
                                                                 "TA Thomas",
                                                                 "Thyolo Boma",
                                                                 "Zakazaka Ward",
                                                                 "Zomba Central Ward")),
                            options = list(
                              
                              `live-search` = TRUE)
                ))
                
        
                ),
  
  absolutePanel(top = "auto", bottom = 10, right = 10, left = "auto",
                width = 350, style = "z-index:500; text-align: right;",                
                tags$i(h6("Data:",
                  a(
                    href = "https://www.leymanck.com/wp-content/uploads/2019/07/Postcodes-2019-final-gazetted-1.pdf",
                    "Malawi Communications Regulatory Authority (MACRA).")
                )), 
                style = "color:#2F4F4F"
  )
)


server <- function(input, output, session) {
  
  
  filteredData <- reactive({
    if (input$postcodes == "All postcodes") {
      postcodes_data
    } else {
      filter(postcodes_data, LOCATION == input$postcodes)
    }
  })
  
  
 output$map <- renderLeaflet({
    
    # labels <- sprintf("%s: %g", filteredData()$LOCATION,
    #                   filteredData()$POSTCODE) %>%
    #   lapply(htmltools::HTML)
    
    leaflet(filteredData(), options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x){
        L.control.zoom({position: 'bottomright'}).addTo(this)
      }") %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addCircleMarkers(~lng, ~lat,
                       label = sprintf("<strong>%s</strong><br/>Postcode: %g<br/>Region: %s",
                                       filteredData()$LOCATION, filteredData()$POSTCODE, filteredData()$ADM1_EN) |> 
                                       lapply(htmltools::HTML),
                       labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                padding = "3x 8px"),
                                                   textsize = "15px", direction = "auto"),
                       popup = sprintf("<strong>%s</strong><br/>Postcode: %g<br/>Region: %s",
                                       filteredData()$LOCATION, filteredData()$POSTCODE, filteredData()$ADM1_EN) |> 
                         lapply(htmltools::HTML),
                       group = 'circles',
                       clusterOptions = markerClusterOptions()) |> 
      #addScaleBar(position = "bottomright") |> 
      addControlGPS(options = gpsOptions(position = "bottomleft",
                                         activate = TRUE,
                                         autoCenter = TRUE,
                                         maxZoom = 13,
                                         setView = TRUE))
  })
  

  
  observeEvent(input$postcodes, {
    withProgress(message = 'Yembekezani...',
                 value = 1/5,{
                   req(filteredData())
                   leafletProxy("map", data = filteredData()) %>%
                     clearShapes() %>%
                     addCircleMarkers(~lng, ~lat,
                                      label = sprintf("<strong>%s</strong><br/>Postcode: %g<br/>Region: %s",
                                                      filteredData()$LOCATION, 
                                                      filteredData()$POSTCODE, 
                                                      filteredData()$ADM1_EN) |> 
                                        lapply(htmltools::HTML),
                                      labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                               padding = "3x 8px"),
                                                                  textsize = "15px", direction = "auto"),
                                      popup = sprintf("<strong>%s</strong><br/>Postcode: %g<br/>Region: %s",
                                                      filteredData()$LOCATION, 
                                                      filteredData()$POSTCODE, 
                                                      filteredData()$ADM1_EN) |> 
                                      #popup = paste0("Postcode:", as,character(filteredData()$POSTCODE)) |> 
                                        lapply(htmltools::HTML),
                                      group = 'circles',
                                      clusterOptions = markerClusterOptions())})
  })
  
  filteredPostcode <- reactive({
    if (input$postcodes == "All postcodes") {
      NULL
    } else {
      filter(postcodes_data, LOCATION == input$postcodes)
    }
  })
  
  output$reactive_postcode <- renderText({
    paste0("Postcode: ", filteredPostcode()$POSTCODE)
  })
  
 
}





shinyApp(ui, server)
