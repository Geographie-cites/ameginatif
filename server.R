###########################################
##### Ameginatif - server           #######
###########################################

shinyServer(function(input, output, session) {
  
  # select data (scenario) and filter data (subpopulation) ----
  
  select_dataone <- reactive({
    req(input$selindex, input$reloc, input$excess, input$modetrans, input$filterpop1)
    spaceConfig <- paste(input$reloc, input$excess, input$modetrans, sep = "_")
    tempFlows <- listTabflows[[spaceConfig]] 
    if(input$filterpop1 == "TOU"){
      tempFlows <- tempFlows
    } else if (input$filterpop1 == "AGR") {
      tempFlows <- tempFlows %>% filter(CSP == 1)
    } else if (input$filterpop1 == "ART") {
      tempFlows <- tempFlows %>% filter(CSP == 2)
    } else if (input$filterpop1 == "CAD") {
      tempFlows <- tempFlows %>% filter(CSP == 3)
    } else if (input$filterpop1 == "INT") {
      tempFlows <- tempFlows %>% filter(CSP == 4)
    } else if (input$filterpop1 == "EMP") {
      tempFlows <- tempFlows %>% filter(CSP == 5)
    } else if (input$filterpop1 == "OUV") {
      tempFlows <- tempFlows %>% filter(CSP == 6)
    } else if (input$filterpop1 == "VP") {
      tempFlows <- tempFlows %>% filter(MODE == "VP")
    } else if (input$filterpop1 == "TC") {
      tempFlows <- tempFlows %>% filter(MODE == "TC")
    } else if (input$filterpop1 == "DO") {
      tempFlows <- tempFlows %>% filter(MODE == "IM")
    } else if (input$filterpop1 == "NM") {
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
  
  select_datatwo <- reactive({
    req(input$selindex, input$reloc, input$excess, input$modetrans, input$filterpop2)
    spaceConfig <- paste(input$reloc, input$excess, input$modetrans, sep = "_")
    tempFlows <- listTabflows[[spaceConfig]] 
    if(input$filterpop2 == "TOU"){
      tempFlows <- tempFlows
    } else if (input$filterpop2 == "AGR") {
      tempFlows <- tempFlows %>% filter(CSP == 1)
    } else if (input$filterpop2 == "ART") {
      tempFlows <- tempFlows %>% filter(CSP == 2)
    } else if (input$filterpop2 == "CAD") {
      tempFlows <- tempFlows %>% filter(CSP == 3)
    } else if (input$filterpop2 == "INT") {
      tempFlows <- tempFlows %>% filter(CSP == 4)
    } else if (input$filterpop2 == "EMP") {
      tempFlows <- tempFlows %>% filter(CSP == 5)
    } else if (input$filterpop2 == "OUV") {
      tempFlows <- tempFlows %>% filter(CSP == 6)
    } else if (input$filterpop2 == "VP") {
      tempFlows <- tempFlows %>% filter(MODE == "VP")
    } else if (input$filterpop2 == "TC") {
      tempFlows <- tempFlows %>% filter(MODE == "TC")
    } else if (input$filterpop2 == "DO") {
      tempFlows <- tempFlows %>% filter(MODE == "IM")
    } else if (input$filterpop2 == "NM") {
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
  
  select_datathree <- reactive({
    req(input$selindex, input$reloc, input$excess, input$modetrans, input$filterpop3)
    spaceConfig <- paste(input$reloc, input$excess, input$modetrans, sep = "_")
    tempFlows <- listTabflows[[spaceConfig]] 
    if(input$filterpop3 == "TOU"){
      tempFlows <- tempFlows
    } else if (input$filterpop3 == "AGR") {
      tempFlows <- tempFlows %>% filter(CSP == 1)
    } else if (input$filterpop3 == "ART") {
      tempFlows <- tempFlows %>% filter(CSP == 2)
    } else if (input$filterpop3 == "CAD") {
      tempFlows <- tempFlows %>% filter(CSP == 3)
    } else if (input$filterpop3 == "INT") {
      tempFlows <- tempFlows %>% filter(CSP == 4)
    } else if (input$filterpop3 == "EMP") {
      tempFlows <- tempFlows %>% filter(CSP == 5)
    } else if (input$filterpop3 == "OUV") {
      tempFlows <- tempFlows %>% filter(CSP == 6)
    } else if (input$filterpop3 == "VP") {
      tempFlows <- tempFlows %>% filter(MODE == "VP")
    } else if (input$filterpop3 == "TC") {
      tempFlows <- tempFlows %>% filter(MODE == "TC")
    } else if (input$filterpop3 == "DO") {
      tempFlows <- tempFlows %>% filter(MODE == "IM")
    } else if (input$filterpop3 == "NM") {
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
      addProviderTiles(provider = "Stamen.TonerLite",
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
    coef <- ifelse(input$selindex %in% c("TOTORI", "TOTDES"), 0.16, 0.08)
    
    if(unitVar == "%"){
      labelVar <- sprintf("<strong>%s</strong><br/> %.1f %s", select_dataone()$COM$LIBGEO, select_dataone()$COM[[input$selindex]], unitVar)
    } else {
      labelVar <- sprintf("<strong>%s</strong><br/> %.0f %s", select_dataone()$COM$LIBGEO, select_dataone()$COM[[input$selindex]], unitVar)
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
    
    if(nrow(select_dataone()$TF) == 0){
      leafBase %>%
        addPolygons(data = select_dataone()$COM,
                    stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                    fillColor = "#D9D9D9",
                    fillOpacity = 0.3,
                    options = pathOptions(pane = "mymap"))
    } else if(input$selindex %in% c("TOTORI", "TOTDES", "SUMDISTORI", "SUMDISTDES")){
      leafBase %>%
        addPolygons(data = select_dataone()$COM,
                    stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                    fillColor = "#D9D9D9",
                    fillOpacity = 0.3,
                    options = pathOptions(pane = "mymap")) %>%
        addCircleMarkers(lng = select_dataone()$COM[["LON"]],
                         lat = select_dataone()$COM[["LAT"]],
                         color = unname(vecColors[names(vecColors) == input$selindex]),
                         radius = coef * sqrt(select_dataone()$COM[[input$selindex]] / pi),
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         options = pathOptions(pane = "mymap"),
                         label = lapply(X = labelVar, FUN = htmltools::HTML))
    } else if(input$selindex == "ABSBAL") {
      leafBase %>%
        addPolygons(data = select_dataone()$COM,
                    stroke = TRUE, weight = 0.7, opacity = 0.4, color = "grey", fill = TRUE,
                    fillColor = "#D9D9D9",
                    fillOpacity = 0.3,
                    options = pathOptions(pane = "mymap")) %>%
        addCircleMarkers(lng = select_dataone()$COM[["LON"]],
                         lat = select_dataone()$COM[["LAT"]],
                         color = ifelse(select_dataone()$COM[[input$selindex]] > 0, "#fab013", "#54478f"),
                         radius = 1.7 * sqrt(sqrt(abs(select_dataone()$COM[[input$selindex]]) / pi)),
                         stroke = TRUE,
                         weight = 0.9,
                         fillColor = build_palette(select_dataone()$COM[[input$selindex]])(select_dataone()$COM[[input$selindex]]),
                         fillOpacity = 0.7,
                         options = pathOptions(pane = "mymap"),
                         label = lapply(X = labelVar, FUN = htmltools::HTML))
    } else {
      leafBase %>%
        addPolygons(data = select_dataone()$COM,
                    stroke = TRUE, weight = 0.9, opacity = 0.5, color = "grey", fill = TRUE,
                    fillColor = build_palette(select_dataone()$COM[[input$selindex]], palseq = unname(vecPals[names(vecPals) == input$selindex]))(select_dataone()$COM[[input$selindex]]),
                    fillOpacity = 0.7,
                    options = pathOptions(pane = "mymap"),
                    label = lapply(X = labelVar, FUN = htmltools::HTML)) %>%
        addLegend(pal = build_palette(select_dataone()$COM[[input$selindex]], palseq = unname(vecPals[names(vecPals) == input$selindex])),
                  values = select_dataone()$COM[[input$selindex]],
                  opacity = 0.7,
                  title = NULL,
                  position = "bottomright")
    }
    
  })
  
  # TAB2: get top links ----
  
  extract_links <- reactive({
    req(input$fluref, input$fluvar, input$flucom, input$fluthr)
    topLinks <- get_toplinks(tabflows = select_datatwo()$TF, 
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
      addProviderTiles(provider = "Stamen.TonerLite",
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
    if(input$fluvar == "FLOW"){
      labelVar <- sprintf("<strong>%s</strong><br/> %.0f %s", topDes$POLYG$LIBGEO, topDes$POLYG$FLOW, "individus")
    } else {
      labelVar <- sprintf("<strong>%s</strong><br/> %.0f %s", topDes$POLYG$LIBGEO, topDes$POLYG$SUMDIST, "km")
    }
    
    leafBase %>%
      addPolygons(data = topDes$POLYG,
                  label = lapply(X = labelVar, FUN = htmltools::HTML),         
                  stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                  fillColor = "#FDBF6F",
                  fillOpacity = 0.45,
                  options = pathOptions(pane = "mymap")) %>% 
      addPolylines(data = topDes$LINES, color = "#FF7F00", opacity = 0.8, weight = 1.5, stroke = TRUE)
  })
  
  
  # TAB3: initialize leaflet device ----
  
  output$mapstruc <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 13)) %>%
      addProviderTiles(provider = "Stamen.TonerLite",
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
  
  
  # TAB3: update leaflet proxy ---- 
  
  observe({
    req(input$strucalgo)
    leafBase <- leafletProxy("mapstruc") %>%
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
    
    if(nrow(select_datathree()$TF) == 0){
      leafBase %>%
        addPolygons(data = select_dataone()$COM,
                    stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                    fillColor = "#D9D9D9",
                    fillOpacity = 0.3,
                    options = pathOptions(pane = "mymap"))
    } else if(input$strucalgo == "domflo"){
      oneStruc <- nystuen_dacey(pol = select_datathree()$COMAGR, 
                                tabflows = select_datathree()$TFAGR, 
                                idpol = CODGEO, 
                                idori = ORI, 
                                iddes = DES, 
                                idflow = FLOW)
      leafBase %>%
        addPolylines(data = oneStruc$FLOWS, color = "#FF7F00", opacity = 0.5, weight = 1, stroke = TRUE) %>% 
        addCircleMarkers(data = oneStruc$NODES,
                         label = oneStruc$NODES$LIBGEO,
                         radius = map_values(oneStruc$NODES$TYPE, from = c(1, 2, 3), to = c(30, 10, 2)),
                         stroke = FALSE, 
                         color = c("#80B139", "#F26418", "#7B7971")[oneStruc$NODES$TYPE],
                         fillOpacity = 0.6,
                         options = pathOptions(pane = "mymap"))
      
    } else if (input$strucalgo == "modularity"){
      oneStruc <- louvain_regions(pol = select_datathree()$COMAGR, 
                                  tabflows = select_datathree()$TFAGR, 
                                  idpol = CODGEO, 
                                  idori = ORI, 
                                  iddes = DES, 
                                  idflow = FLOW)
      
      if(length(unique(oneStruc$CLUS)) > 25){
        leafBase %>%
          addPolygons(data = select_dataone()$COM,
                      stroke = TRUE, weight = 0.8, opacity = 0.4, color = "grey", fill = TRUE,
                      fillColor = "#D9D9D9",
                      fillOpacity = 0.3,
                      options = pathOptions(pane = "mymap"))
      } else {
        colPal <- hclDark[1:length(unique(oneStruc$CLUS))]
        leafBase %>%
          addPolygons(data = oneStruc,
                      stroke = TRUE, weight = 1, opacity = 0.7, color = colPal[oneStruc$CLUS], fill = TRUE,
                      fillColor = colPal[oneStruc$CLUS],
                      fillOpacity = 0.7,
                      options = pathOptions(pane = "mymap"),
                      label = paste0("Cluster : ", oneStruc$NAMECLUS))
      }
    }
  })
  
  
  
  # TAB4: configure plotly device ----
  
  output$numsum <- renderPlotly({
    req(input$reloc, input$excess, input$modetrans)
    spaceConfig <- paste(input$reloc, input$excess, input$modetrans, sep = "_")
    draw_plotly(data = listAggregates, scenar = spaceConfig, variable = input$varsplit, indic = input$varindic)
  })
  
  
  
  # tabs description ----
  
  output$mytext <- renderText({paste(" ")})
  
  observeEvent(input$index_descr, {
    showModal(modalDialog(
      includeMarkdown("coat/index_descr.md"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  observeEvent(input$config_descr, {
    showModal(modalDialog(
      includeMarkdown("coat/config_descr.md"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  observeEvent(input$flux_descr, {
    showModal(modalDialog(
      includeMarkdown("coat/flux_descr.md"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  observeEvent(input$fludom_descr, {
    showModal(modalDialog(
      includeMarkdown("coat/fludom_descr.md"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  showModal(modalDialog(
    includeMarkdown("coat/synth_descr.md"),
    easyClose = TRUE,
    size = "l",
    footer = NULL
  ))
  
  showModal(modalDialog(
    includeMarkdown("coat/welcome.md"),
    easyClose = TRUE,
    size = "l",
    footer = NULL
  ))
  
})