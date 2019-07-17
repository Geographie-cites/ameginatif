###########################################
##### Ameginatif - server           #######
###########################################

shinyServer(function(input, output, session) {
  
  # select data (scenario) and filter data (subpopulation) ----
  
  
  observe({
    print(input$selindex)
  })
  
  select_data <- reactive({
    req(input$selindex, input$reloc, input$excess, input$modetrans, input$filterpop)
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
    
    infoCom <- spatunit_indices(pol = muniBound, tabflows = tempFlows, idpol = CODGEO, idori = ORI, iddes = DES, idflow = FLOW, iddist = DIST)
    tempflowsAgr <- arrflow_aggregate(before = arrDesagg, after = arrAggreg, tabflows = tempFlows, idori = "ORI", iddes = "DES")
    polAgr <- muniBoundAgr
    infoComAgr <- spatunit_indices(pol = polAgr, 
                                   tabflows = tempflowsAgr, 
                                   idpol = CODGEO, 
                                   idori = ORI, 
                                   iddes = DES, 
                                   idflow = FLOW, 
                                   iddist = DIST)
    
    return(list(TF = tempFlows, TFAGR = tempflowsAgr, COM = infoCom, COMAGR = infoComAgr))
  })
  
  # TAB1: initialize leaflet device ----
  
  output$mapindic <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
      addProviderTiles(provider = "CartoDB.PositronNoLabels",
                       options = providerTileOptions(opacity = 0.5)) %>%
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>%
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>%
      addMapPane("mymap", zIndex = 410) %>% 
      addMapPane("road", zIndex = 420) %>%  
      addMapPane("rail", zIndex = 430) %>%  
      addMapPane("railstation", zIndex = 440) %>%  
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Réseau routier principal", "Réseau ferré","Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)) %>%
      hideGroup("Réseau routier principal") %>%
      hideGroup("Réseau ferré") %>%
      hideGroup("Stations ferroviaires")
  })
  
  # TAB1: update leaflet proxy ----    
  observe({
    unitVar <- dicoUnits[names(dicoUnits) == input$selindex]
    coef <- ifelse(input$selindex %in% c("TOTORI", "TOTDES"), 0.2, 0.1)
    
    if(unitVar == "%"){
      labelVar <- sprintf("<strong>%s</strong><br/> %.1f %s", select_data()$COM$LIBGEO, select_data()$COM[[input$selindex]], unitVar)
    } else {
      labelVar <- sprintf("<strong>%s</strong><br/> %.0f %s", select_data()$COM$LIBGEO, select_data()$COM[[input$selindex]], unitVar)
    }
    
    leafBase <- leafletProxy("mapindic") %>%
      clearShapes() %>% clearControls() %>% clearMarkers() %>%
      addPolylines(data = externalFeatures$ROAD, color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "road")) %>%
      addPolylines(data = externalFeatures$RAIL, color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "rail")) %>%
      addCircleMarkers(data = externalFeatures$RAILSTATION,
                       radius = 2,
                       stroke = FALSE,
                       color = "grey",
                       fillOpacity = 0.7,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "railstation"))
    
    if(input$selindex %in% c("TOTORI", "TOTDES", "SUMDISTORI", "SUMDISTDES")){
      leafBase %>%
        addPolygons(data = select_data()$COM,
                    stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                    fillColor = "#D9D9D9",
                    fillOpacity = 0.3,
                    options = pathOptions(pane = "mymap")) %>%
        addCircleMarkers(lng = select_data()$COM[["LON"]],
                         lat = select_data()$COM[["LAT"]],
                         color = unname(vecColors[names(vecColors) == input$selindex]),
                         radius = coef * sqrt(select_data()$COM[[input$selindex]] / pi),
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         options = pathOptions(pane = "mymap"),
                         label = lapply(X = labelVar, FUN = htmltools::HTML))
    } else if(input$selindex == "ABSBAL") {
      leafBase %>%
        addPolygons(data = select_data()$COM,
                    stroke = TRUE, weight = 0.7, opacity = 0.4, color = "grey", fill = TRUE,
                    fillColor = "#D9D9D9",
                    fillOpacity = 0.3,
                    options = pathOptions(pane = "mymap")) %>%
        addCircleMarkers(lng = select_data()$COM[["LON"]],
                         lat = select_data()$COM[["LAT"]],
                         color = ifelse(select_data()$COM[[input$selindex]] > 0, "#fab013", "#54478f"),
                         radius = 1.7 * sqrt(sqrt(abs(select_data()$COM[[input$selindex]]) / pi)),
                         stroke = TRUE,
                         weight = 0.9,
                         fillColor = build_palette(select_data()$COM[[input$selindex]])(select_data()$COM[[input$selindex]]),
                         fillOpacity = 0.7,
                         options = pathOptions(pane = "mymap"),
                         label = lapply(X = labelVar, FUN = htmltools::HTML))
    } else {
      leafBase %>%
        addPolygons(data = select_data()$COM,
                    stroke = TRUE, weight = 0.9, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = build_palette(select_data()$COM[[input$selindex]], palseq = unname(vecPals[names(vecPals) == input$selindex]))(select_data()$COM[[input$selindex]]),
                    fillOpacity = 0.7,
                    options = pathOptions(pane = "mymap"),
                    label = lapply(X = labelVar, FUN = htmltools::HTML)) %>%
        addLegend(pal = build_palette(select_data()$COM[[input$selindex]], palseq = unname(vecPals[names(vecPals) == input$selindex])),
                  values = select_data()$COM[[input$selindex]],
                  opacity = 0.7,
                  title = NULL,
                  position = "bottomright")
    }
    
  })
  
  # TAB2: get top links ----
  
  extract_links <- reactive({
    req(input$fluref, input$fluvar, input$flucom, input$fluthr)
    topLinks <- get_toplinks(tabflows = select_data()$TF, 
                             pol = muniBound, 
                             ref = input$fluref, 
                             varsort = input$fluvar, 
                             oneunit = input$flucom, 
                             thres = input$fluthr)
    return(topLinks)
  })
  
  # TAB2: initialize leaflet device ----
  
  output$mapflow <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
      addProviderTiles(provider = "CartoDB.PositronNoLabels",
                       options = providerTileOptions(opacity = 0.5)) %>%
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)%>%
      setMaxBounds(-0.05, 46.62, 5.05, 50.73) %>%
      addMapPane("mymap", zIndex = 410) %>% 
      addMapPane("road", zIndex = 420) %>%  
      addMapPane("rail", zIndex = 430) %>%  
      addMapPane("railstation", zIndex = 440) %>%  
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Réseau routier principal", "Réseau ferré","Stations ferroviaires"),
        options = layersControlOptions(collapsed = T)) %>%
      hideGroup("Réseau routier principal") %>%
      hideGroup("Réseau ferré") %>%
      hideGroup("Stations ferroviaires")
  })
  
  
  # TAB2: update leaflet proxy ---- 
  
  
  observe({
    leafBase <- leafletProxy("mapflow") %>%
      clearShapes() %>% clearControls() %>% clearMarkers() %>%
      addPolylines(data = externalFeatures$ROAD, color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau routier principal",
                   options = pathOptions(pane = "road")) %>%
      addPolylines(data = externalFeatures$RAIL, color = "grey", opacity = 0.6, weight = 1.3 ,
                   stroke = TRUE, group = "Réseau ferré",  dashArray = 2,
                   options = pathOptions(pane = "rail")) %>%
      addCircleMarkers(data = externalFeatures$RAILSTATION,
                       radius = 2,
                       stroke = FALSE,
                       color = "grey",
                       fillOpacity = 0.7,
                       group = "Stations ferroviaires",
                       options = pathOptions(pane = "railstation"))
    
    topDes <- extract_links()
    leafBase %>%
      addPolygons(data = topDes$POLYG,
                  label = topDes$POLYG$LIBGEO,         
                  stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                  fillColor = "#D9D9D9",
                  fillOpacity = 0.3,
                  options = pathOptions(pane = "mymap")) %>% 
      addPolylines(data = topDes$LINES, color = "#FF7F00", opacity = 0.8, weight = 1.5, stroke = TRUE)
  })
  
  
  
  # tabs description ----
  
  observeEvent(input$index_descr, {
    showModal(modalDialog(
      includeHTML("coat/index_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$flux_descr, {
    showModal(modalDialog(
      includeHTML("coat/flux_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$pool_descr, {
    showModal(modalDialog(
      includeHTML("coat/pool_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$fludom_descr, {
    showModal(modalDialog(
      includeHTML("coat/fludom_descr.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  showModal(modalDialog(
    includeHTML("coat/welcome.html"),
    easyClose = TRUE,
    footer = NULL
  ))
  
  
})