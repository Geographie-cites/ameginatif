###########################################
##### Ameginatif - server           #######
###########################################

shinyServer(function(input, output, session) {
  
  # get data ----
  
  select_data <- reactive({
    req(input$reloc, input$excess, input$modetrans, input$filterpop)
    spaceConfig <- paste(input$reloc,input$excess, sep = "_")
    tempFlows <- listTabflows[[spaceConfig]] 
    if(input$filterpop == "TOU"){
      tempFlows <- tempFlows
    } else if (input$filterpop == "AGR") {
      tempFlows <- tempFlows %>% filter(CSP == 1)
    } else if (input$filterpop == "ART") {
      tempFlows <- tempFlows %>% filter(CSP == 2)
    } else if (input$filterpop == "CAD") {
      tempFlows <- tempFlows %>% filter(CSP == 3)
    } else if (input$filterpop == "INT") {
      tempFlows <- tempFlows %>% filter(CSP == 4)
    } else if (input$filterpop == "EMP") {
      tempFlows <- tempFlows %>% filter(CSP == 5)
    } else if (input$filterpop == "OUV") {
      tempFlows <- tempFlows %>% filter(CSP == 6)
    } else if (input$filterpop == "VP") {
      tempFlows <- tempFlows %>% filter(MODE == "VP")
    } else if (input$filterpop == "TC") {
      tempFlows <- tempFlows %>% filter(MODE == "TC")
    } else if (input$filterpop == "DO") {
      tempFlows <- tempFlows %>% filter(MODE == "IM")
    } else if (input$filterpop == "NM") {
      tempFlows <- tempFlows %>% filter(MODE == "NM")
    }
    
    infoCom <- spatunit_indices(pol = pol, tabflows = tempflows, idpol = CODGEO, idori = ORI, iddes = DES, idflow = FLOW, iddist = DIST)
    tempflowsAgr <- arrflow_aggregate(before = arrDesagg, after = arrAggreg, tabflows = tempflows, idori = "ORI", iddes = "DES")
    polAgr <- arrunit_aggregate(before = arrDesagg, after = arrAggreg, pol = pol, idpol = CODGEO)
    infoComAgr <- spatunit_indices(pol = polAgr, 
                                   tabflows = tempflowsAgr, 
                                   idpol = CODGEO, 
                                   idori = ORI, 
                                   iddes = DES, 
                                   idflow = FLOW, 
                                   iddist = DIST)

    return(list(TF = tempflows, TFAGR = tempflowsAgr, COM = infoCom, COMAGR = infoComAgr))
  })
  
  
  #Indicators map Display  ----
  output$mapindic <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
      addMapPane("choropleth", zIndex = 410) %>%  # Level 1
      addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
      addMapPane("voie_ferré", zIndex = 430) %>%  # Level 3
      addMapPane("station", zIndex = 440) %>%  # Level 4
      addMapPane("cercles", zIndex = 450) %>%  # Level 5
      addMapPane("label", zIndex = 460) %>%  # Level 6
      addProviderTiles(provider = "CartoDB.PositronNoLabels",
                       options = providerTileOptions(opacity = 0.5)) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Réseau routier principal", "Réseau ferré","Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)
      ) %>%
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>%
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>%
      hideGroup("Réseau routier principal") %>%
      hideGroup("Réseau ferré") %>%
      hideGroup("Stations ferroviaires") %>%
      hideGroup(index$polygons)  %>%
      hideGroup(index$layer)
  })
  

  observe({
    shinyjs::showElement(id = 'loading-content')
    shinyjs::showElement(id = 'loading')
    req(input$selindex)
    
    brks <- classIntervals(var = select_data()$COM[input$selindex],
                           n = 6,
                           style = "fisher")$brks
    
    build_pal <- colorBin(palette = "Purples",
                       bins = brks,
                       domain = select_data()$COM[input$selindex],
                       na.color = "transparent")

    leafletProxy("mapindic") %>%
      clearShapes() %>%
      addPolygons(data = select_data()$COM, 
                  stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
                  fillColor = ~build_pal(eval(parse(text = input$selindex))), 
                  fillOpacity = 0.9, 
                  label = select_data()$COM$LIBGEO) %>%
      addPolylines(data = externalFeatures$ROAD,
                   color = "grey",
                   opacity = 0.6,
                   weight = 1.3 ,
                   stroke = TRUE,
                   group = "Réseau routier principal",
                   options = pathOptions(pane = "réseau_routier")) %>%
      addPolylines(data = externalFeatures$RAIL,
                   color = "grey",
                   opacity = 1,
                   weight = 1 ,
                   stroke = TRUE,
                   group = "Réseau ferré",
                   dashArray = 2,
                   options = pathOptions(pane = "voie_ferré")) %>%
      addCircleMarkers(data = externalFeatures$RAILSTATION,
                       radius = 2,
                       stroke = FALSE,
                       color = "grey",
                       fillOpacity = 0.8,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "station")) %>%
      
    shinyjs::hideElement(id = 'loading-content')
    shinyjs::hideElement(id = 'loading')
  })
  
  # # Flow map Display ----
  # output$mapflu <- renderLeaflet({
  #   lflFlow <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
  #     addMapPane("communes", zIndex = 410) %>%  # Level 1
  #     addMapPane("réseau_routier", zIndex = 420) %>%  # Level 2
  #     addMapPane("voie_ferré", zIndex = 430) %>%  # Level 3
  #     addMapPane("station", zIndex = 440) %>%  # Level 4
  #     addMapPane("lignes", zIndex = 450) %>%  # Level 5
  #     addMapPane("label", zIndex = 460) %>%  # Level 6
  #     addProviderTiles(provider = "CartoDB.PositronNoLabels",
  #                      options = providerTileOptions(opacity = 0.5)) %>% 
  #     addLayersControl(
  #       position = "bottomright",
  #       overlayGroups = c("Réseau routier principal", "Réseau ferré", "Stations ferroviaires"),
  #       options = layersControlOptions(collapsed = T)
  #     ) %>%
  #     fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>%
  #     setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>%
  #     hideGroup("Réseau routier principal") %>%
  #     hideGroup("Réseau ferré") %>%
  #     hideGroup("Stations ferroviaires")
  #   
  #   topDes <- isolate(GetTopLinks())
  #   cityVal <- isolate(Get_CityValue())
  #   lflFlow %>%
  #     addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau routier principal",
  #                  options = pathOptions(pane = "réseau_routier")) %>%
  #     addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
  #                  options = pathOptions(pane = "voie_ferré")) %>%
  #     addCircleMarkers(lng = station$longitude,
  #                      lat = station$latitude,
  #                      radius = 2,
  #                      stroke = F,
  #                      color = "grey",
  #                      fillOpacity = 0.8,
  #                      group = "Stations ferroviaires",
  #                      options = pathOptions(pane = "station")) %>%
  #     addPolylines(data = topDes, color = "black", opacity = 0.8, weight = 1.2, stroke = TRUE, options = pathOptions(pane = "lignes"), group = "lignes") %>%
  #     addPolygons(data = cityVal$SPCOM, fillColor = colorBin(palette = "Purples",
  #                                                            bins = cityVal$BREAKS,
  #                                                            domain = cityVal$SPCOM$DATA,
  #                                                            na.color = "#dfdfe5")(cityVal$SPCOM$DATA)
  #                 ,
  #                 weight = 0.7,
  #                 opacity = 0.5,
  #                 color = "grey",
  #                 fillOpacity = 0.7,
  #                 highlight = highlightOptions(
  #                   weight = 2,
  #                   color = "white",
  #                   opacity = 0.7,
  #                   fillOpacity = 1,
  #                   bringToFront = TRUE),
  #                 label = sprintf(
  #                   "<strong>%s</strong><br/> %.0f %s",
  #                   toupper(cityVal$SPCOM$nomcom),
  #                   cityVal$SPCOM$DATA,
  #                   cityVal$INDEX
  #                 )%>% lapply(htmltools::HTML),
  #                 labelOptions = labelOptions(
  #                   style = list("font-weight" = "normal", padding = "3px 8px"),
  #                   textsize = "15px",
  #                   direction = "auto"),
  #                 options = pathOptions(pane = "réseau_routier")
  #     ) %>%
  #     addLegend(pal = colorBin(palette = "Purples",
  #                              bins = cityVal$BREAKS,
  #                              domain = cityVal$SPCOM$DATA,pretty = TRUE,
  #                              na.color = "#dfdfe5"),
  #               values = cityVal$SPCOM$DATA, opacity = 0.7,
  #               title = NULL, position = "bottomright")
  #   
  # })
  # 
  # observe({
  #   topDes <- GetTopLinks()
  #   cityVal <- Get_CityValue()
  #   shinyjs::showElement(id = 'loading-content')
  #   shinyjs::showElement(id = 'loading')
  #   leafletProxy("mapflu", data = cityVal$SPCOM) %>%
  #     clearShapes() %>%
  #     addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau routier principal",
  #                  options = pathOptions(pane = "réseau_routier")) %>%
  #     addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
  #                  options = pathOptions(pane = "voie_ferré")) %>%
  #     addCircleMarkers(lng = station$longitude,
  #                      lat = station$latitude,
  #                      radius = 2,
  #                      stroke = F,
  #                      color = "grey",
  #                      fillOpacity = 0.8,
  #                      group = "Stations ferroviaires",
  #                      options = pathOptions(pane = "station")) %>%
  #     addPolylines(data = topDes, color = "black", opacity = 0.8, weight = 1.2, stroke = TRUE, options = pathOptions(pane = "lignes"), group = "lignes") %>%
  #     addPolygons(
  #       fillColor = ~colorBin(palette = "Purples",
  #                             bins = cityVal$BREAKS,
  #                             domain = cityVal$SPCOM$DATA,
  #                             na.color = "#dfdfe5")(cityVal$SPCOM$DATA),
  #       weight = 0.7,
  #       opacity = 0.5,
  #       color = "grey",
  #       fillOpacity = 0.7,
  #       highlight = highlightOptions(
  #         weight = 2,
  #         color = "white",
  #         opacity = 0.7,
  #         fillOpacity = 1,
  #         bringToFront = TRUE),
  #       label = sprintf(
  #         "<strong>%s</strong><br/> %.0f %s",
  #         toupper(cityVal$SPCOM$nomcom),
  #         cityVal$SPCOM$DATA,
  #         cityVal$INDEX
  #       )%>% lapply(htmltools::HTML),
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "15px",
  #         direction = "auto"),
  #       options = pathOptions(pane = "réseau_routier")
  #     )
  #   shinyjs::hideElement(id ='loading-content')
  #   shinyjs::hideElement(id ='loading')
  # })
  # 
  # observe({
  #   cityVal <- Get_CityValue()
  #   proxy <- leafletProxy("mapflu", data =cityVal$SPCOM)
  #   proxy %>% clearControls()
  #   proxy %>% addLegend(pal = colorBin(palette = "Purples",
  #                                      bins = cityVal$BREAKS,
  #                                      domain = cityVal$SPCOM$DATA,pretty = TRUE,
  #                                      na.color = "#dfdfe5"),
  #                       values = ~cityVal$SPCOM$DATA, opacity = 0.7,
  #                       title = NULL, position = "bottomright"
  #   )
  # })
  # 
  # # Dominant flow map Display  ----
  # output$mapfluDom <- renderLeaflet({
  #   
  #   lflStructure <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13, incl.data=TRUE)) %>%
  #     addMapPane("communes", zIndex = 410) %>%         # Level 1
  #     addMapPane("réseau_routier", zIndex = 420) %>%   # Level 2
  #     addMapPane("voie_ferré", zIndex = 430) %>%       # Level 3
  #     addMapPane("station", zIndex = 440) %>%          # Level 4
  #     addMapPane("flux", zIndex = 450) %>%             # Level 6
  #     addMapPane("cercles", zIndex = 460) %>%          # Level 7
  #     addMapPane("label", zIndex = 470) %>%            # Level 5
  #     addProviderTiles(provider = "CartoDB.PositronNoLabels",
  #                      options = providerTileOptions(opacity = 0.5)) %>%
  #     addLayersControl(
  #       position = "bottomright",
  #       overlayGroups = c("Communes", "Réseau routier principal", "Réseau ferré", "Stations ferroviaires"),
  #       options = layersControlOptions(collapsed = T)
  #     ) %>%
  #     fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>%
  #     setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>%
  #     hideGroup("Réseau routier principal") %>%
  #     hideGroup("Réseau ferré") %>%
  #     hideGroup("Communes") %>%
  #     hideGroup("Stations ferroviaires")
  #   
  #   structure <- isolate(Get_Structure())
  #   lflStructure %>%
  #     addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
  #                 fillColor = "grey", fillOpacity = 0, group = "Communes",
  #                 options = pathOptions(pane = "communes")) %>%
  #     addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau routier principal",
  #                  options = pathOptions(pane = "réseau_routier")) %>%
  #     addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
  #                  options = pathOptions(pane = "voie_ferré")) %>%
  #     addCircleMarkers(lng = station$longitude,
  #                      lat = station$latitude,
  #                      radius = 2,
  #                      stroke = F,
  #                      color = "grey",
  #                      fillOpacity = 0.8,
  #                      group = "Stations ferroviaires",
  #                      options = pathOptions(pane = "station")) %>%
  #     addPolylines(data = st_transform(structure$dataflu, crs = 4326), color = "#82909E", opacity = 0.2, weight = 1.5 ,
  #                  stroke = TRUE) %>%
  #     addCircles(lng = structure$cercle[["lon"]],
  #                lat = structure$cercle[["lat"]],
  #                color = colorNumeric(palette = c("#B35605","#F1A340","#828F9E"), domain = structure$cercle[["status"]])(structure$cercle[["status"]]),
  #                radius = structure$rayon,
  #                stroke = F,
  #                fillOpacity = 0.8,
  #                highlight = highlightOptions(
  #                  weight = 5,
  #                  color = "white",
  #                  opacity = 1,
  #                  fillOpacity = 1,
  #                  bringToFront = F),
  #                label = sprintf(
  #                  "<strong>%s</strong><br/> %s %.0f",
  #                  structure$comm,
  #                  structure$nom,
  #                  structure$valCercle
  #                )%>% lapply(htmltools::HTML),
  #                labelOptions = labelOptions(
  #                  style = list("font-weight" = "normal",
  #                               padding = "3px 8px"),
  #                  textsize = "15px",
  #                  direction = "auto"),
  #                options = pathOptions(pane = "cercles")
  #     )%>%
  #     addLegendCustom(colors = c("#B35605","#F1A340","#97A7B8"), labels = c("Dominant", "Intermédiaire", "Dominé"), sizes = c(25, 20, 15))
  #   
  #   
  # })
  # 
  # observe({
  #   structure <- Get_Structure()
  #   shinyjs::showElement(id = 'loading-content')
  #   shinyjs::showElement(id = 'loading')
  #   leafletProxy(mapId = "mapfluDom", data = c(structure$dataflu,structure$rayon,structure$col)) %>%
  #     clearShapes() %>%
  #     addPolygons(data = st_transform(pol, crs = 4326), stroke = TRUE, weight = 0.5, opacity = 0.5, color = "grey", fill = TRUE,
  #                 fillColor = "grey", fillOpacity = 0, group = "Communes",
  #                 options = pathOptions(pane = "communes")) %>%
  #     addPolylines(data = st_transform(routier, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau routier principal",
  #                  options = pathOptions(pane = "réseau_routier")) %>%
  #     addPolylines(data = st_transform(vferre, crs = 4326), color = "grey", opacity = 0.6, weight = 1.3 ,
  #                  stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
  #                  options = pathOptions(pane = "voie_ferré")) %>%
  #     addCircleMarkers(lng = station$longitude,
  #                      lat = station$latitude,
  #                      radius = 2,
  #                      stroke = F,
  #                      color = "grey",
  #                      fillOpacity = 0.8,
  #                      group = "Stations ferroviaires",
  #                      options = pathOptions(pane = "station")) %>%
  #     addPolylines(data = st_transform(structure$dataflu, crs = 4326), color = "#82909E", opacity = 0.2, weight = 1.5 ,
  #                  stroke = TRUE)%>%
  #     addCircles(lng = structure$cercle[["lon"]],
  #                lat = structure$cercle[["lat"]],
  #                radius = structure$rayon,
  #                color = ~colorNumeric(palette = c("#B35605","#F1A340","#828F9E"), domain = structure$cercle[["status"]])(structure$cercle[["status"]]),
  #                stroke = F,
  #                fillOpacity = 0.8,
  #                highlight = highlightOptions(
  #                  weight = 5,
  #                  color = "white",
  #                  opacity = 1,
  #                  fillOpacity = 1,
  #                  bringToFront = F),
  #                label = sprintf(
  #                  "<strong>%s</strong><br/> %s %.0f",
  #                  structure$comm,
  #                  structure$nom,
  #                  structure$valCercle
  #                )%>% lapply(htmltools::HTML),
  #                labelOptions = labelOptions(
  #                  style = list("font-weight" = "normal",
  #                               padding = "3px 8px"),
  #                  textsize = "15px",
  #                  direction = "auto"),
  #                options = pathOptions(pane = "cercles")
  #     )
  #   shinyjs::hideElement(id ='loading-content')
  #   shinyjs::hideElement(id ='loading')
  # })
  # 
  # observe({
  #   structure <- Get_Structure()
  #   proxy <- leafletProxy("mapfluDom", data =c(structure$dataflu,structure$rayon,structure$col))
  #   proxy %>% clearControls()
  #   proxy %>% addLegendCustom(colors = c("#B35605","#F1A340","#97A7B8"), labels = c("Dominant", "Intermédiaire", "Dominé"), sizes = c(25, 20, 15))
  # })
  # 
  # 
  # 
  # # description ----
  # 
  # observeEvent(input$index_descr, {
  #   showModal(modalDialog(
  #     includeHTML("coat/index_descr.html"),
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # observeEvent(input$flux_descr, {
  #   showModal(modalDialog(
  #     includeHTML("coat/flux_descr.html"),
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # observeEvent(input$pool_descr, {
  #   showModal(modalDialog(
  #     includeHTML("coat/pool_descr.html"),
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # observeEvent(input$fludom_descr, {
  #   showModal(modalDialog(
  #     includeHTML("coat/fludom_descr.html"),
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # showModal(modalDialog(
  #   includeHTML("coat/welcome.html"),
  #   easyClose = TRUE,
  #   footer = NULL
  # ))
  # 
  # 
  # 
  # # FUNCTIONS  ----
  # 
  # 
  # # Function Flux ----
  # 
  # GetTopLinks <- reactive({
  #   shinyjs::showElement(id = 'loading-content')
  #   shinyjs::showElement(id = 'loading')
  #   req(input$fluref, input$fluvar, input$flucom, input$fluthr)
  #   topLinks <- GetLinks(tabnav = Get_Filter_Flux(), tabdist = tabDist, idori = "ORI", iddes = "DES", iddist = "DIST", ref = input$fluref, varsort = input$fluvar, oneunit = substring(input$flucom, 9), thres = input$fluthr)
  #   shinyjs::hideElement(id = 'loading-content')
  #   shinyjs::hideElement(id = 'loading')
  #   return(topLinks)
  # })
  # 
  # Get_CityValue <- reactive({
  #   shinyjs::showElement(id = 'loading-content')
  #   shinyjs::showElement(id = 'loading')
  #   req(input$fluref, input$flucom, input$fluvar)
  #   cityValue <- city_Value(tabflows = Get_Filter_Flux(), matDist = matDist, pol = pol,idpol = "insee", var = input$fluvar, ref = input$fluref, oneunit = substr(input$flucom, 1, 5))
  #   shinyjs::hideElement(id = 'loading-content')
  #   shinyjs::hideElement(id = 'loading')
  #   return(cityValue)
  # })
  # 
  # 
  # # Function Structure ----  
  # 
  # Get_Structure <- reactive({
  #   shinyjs::showElement(id = 'loading-content')
  #   shinyjs::showElement(id = 'loading')
  #   req(input$radioFlu)
  #   structure <- Structure(Flu = input$radioFlu ,domFlowJob = Get_Filter_Structure()$domFlowJob ,domFlowPop = Get_Filter_Structure()$domFlowPop,domFlowJP = Get_Filter_Structure()$domFlowJP)
  #   shinyjs::hideElement(id = 'loading-content')
  #   shinyjs::hideElement(id = 'loading')
  #   return(structure)
  # })
  
})