##############################
# Shiny App: Postcar Explicatif
# Server
##############################

shinyServer(function(input, output, session) {
  
  # outputs ----
  
  output$stock <- renderPlot({
    req(input$equip, input$modetrans, input$reloc, input$excess)
    grid.arrange(GetAggregates()$STOCKDIST, GetAggregates()$STOCKTPS, nrow = 1, ncol = 2)
  })
  
  output$ratio <- renderPlot({
    req(input$equip, input$modetrans, input$reloc, input$excess)
    grid.arrange(GetAggregates()$RATIODIST, GetAggregates()$RATIOTPS, nrow = 1, ncol = 2)
  })
  
  output$intra <- renderPlot({
    req(input$equip, input$modetrans, input$reloc, input$excess)
    grid.arrange(GetAggregates()$INTRACOM, GetAggregates()$INTRADEP, nrow = 1, ncol = 2)
  })
  
  
  output$mappot <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = "CartoDB.DarkMatter") %>% 
      fitBounds(lng1 = 1.44, lat1 = 48.12, lng2 = 3.55, lat2 = 49.24)
  })
  
  
  # observers ----
  
  observe({
    if(input$pottyp %in% c("dif", "oricomp", "descomp")){
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>%
        addRasterImage(x = SelecPotential(), project = FALSE, colors = PotentialPalette(SelecPotential()), opacity = 0.4)
    } else if (input$pottyp == "conn"){
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>%
        addPolygons(data = sfComConn, stroke = TRUE, weight = 1, color = "grey35", fill = TRUE, fillColor = "ghostwhite", fillOpacity = 0.3, label = sfComConn$TCLINE)
    } else if (input$pottyp == "pol") {
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>%
        addPolygons(data = sfPol, stroke = TRUE, weight = 1, color = "grey35", fill = TRUE, fillColor = "ghostwhite", fillOpacity = 0.3, label = sfPol$pole2014)
    } else {
      leafletProxy("mappot") %>%
        clearImages() %>% clearShapes() %>%
        addRasterImage(x = sqrt(SelecPotential()), project = FALSE, colors = PotentialPalette(sqrt(SelecPotential())), opacity = 0.4) %>%
        addPolygons(data = DrawContour(), stroke = TRUE, fill = FALSE, color = "#a9a9a9", weight = 1, label = as.character(round(DrawContour()$center^2)))
    }
  })
  
  
  # functions ----
  
  GetAggregates <- reactive({
    keyCom <- paste(input$modetrans, input$reloc, input$excess, sep = "_")
    keyOther <- paste(input$modetrans, input$equip, input$reloc, input$excess, sep = "_")
    comConf <- listCommuteAggregates[[keyCom]]
    comRef <- listCommuteAggregates[["ACT_ACT_ACT"]]
    otherConf <- listOtherAggregates[[keyOther]]
    otherRef <- listOtherAggregates[["ACT_ACT_ACT_ACT"]]
    setOfPlots <- PlotAggrDist(comconf = comConf, otherconf = otherConf, comref = comRef, otherref = otherRef)
    return(setOfPlots)
  })
  
  
  SelecPotential <- reactive({
    req(input$pottyp, input$reloc, input$excess)
    if(input$pottyp %in% c("ori", "des", "dif")){
      keyRas <- paste(input$pottyp, input$reloc, input$excess, sep = "_")
      resRaster <- listPotentials[[keyRas]]
    } else {
      keyRas <- paste(substr(input$pottyp, 1, 3), input$reloc, input$excess, sep = "_")
      keyRasRef <- paste(substr(input$pottyp, 1, 3), "ACT_ACT", sep = "_")
      oneRaster <- listPotentials[[keyRas]]
      refRaster <- listPotentials[[keyRasRef]]
      resRaster <- oneRaster - refRaster
    }
    return(resRaster)
  })
  
  DrawContour <- reactive({
    potCont <- PotentialContour(ras = sqrt(SelecPotential()))
    return(potCont)
  })
  
  
  
})

