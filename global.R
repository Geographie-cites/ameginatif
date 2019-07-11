###########################################
##### Ameginatif - global functions #######
###########################################

# load packages ----

library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(cartography)
library(raster)
library(leaflet)
library(flows)
library(reshape2)
library(dplyr)


# Load data ----

pol <- readRDS("data/pol.Rds")

matDist <- readRDS(file = "data/matDist.Rds")
matDistAgr <- readRDS(file = "data/matDistAGR.Rds")

tabDist <- melt(matDist, value.name = "DIST", as.is = TRUE)
colnames(tabDist) <- c("ORI","DES","DIST")

tabDistAgr <- melt(matDistAgr, value.name = "DIST", as.is = TRUE)
colnames(tabDistAgr) <- c("ORIAGR","DESAGR","DIST")

listTabflows <- readRDS(file ="data/listTabflows2.Rds")

tabFlows <- readRDS(file = "data/tabflows2.Rds")

tabFlowsAgr <- toyspace::city_aggregate(before = before,after = after,tabflows = tabFlows,idori = "ORI",iddes = "DES")

tabFlowsNoMode <- tabFlows %>%
  group_by(ORI, DES) %>%
  summarise(FLOW = sum(FLOW))


#couches tiers (voies ferré, réseau routier, gares.)
vferre <- readRDS(file = "data/vferre.Rds")
routier <- readRDS(file = "data/routier.Rds")
station <- readRDS(file = "data/station.Rds")

polAgr <- toyspace::pol_aggregate(before = before, after = after, pol = pol, idpol = "insee", namepol = "nomcom", nameAgr = "paris")

#Coordonnées X Y des communes

centPol <-readRDS("data/centPol.Rds")
centPolAgr <-readRDS("data/centPolAgr.Rds")

# commData <- toyspace::mob_indic(tabflows = tabFlows, tabdist = tabDist, idori = "ORI", iddes = "DES", idflow = "FLOW",iddist = "DIST", pol = pol, idpol = "insee")

domFlowJob <- readRDS("data/domFlowJob.Rds")
domFlowPop <- readRDS("data/domFlowPop.Rds")
domFlowJP <- readRDS("data/domFlowJP.Rds")


# FUNCTION GET ----

# get index----

Filter_indice <- function(tabflows, tabdist, idori, iddes, idflow, iddist, pol, idpol,variable, label, centPol, poptab){
  print("filter indice debut")
  start_time <- Sys.time()
  shinyjs::showElement(id = 'loading')
  if(is.null(variable) == TRUE | is.null(label) == TRUE){
    tabflows <- tabflows
  }else{
    tabflows <- tabflows[tabflows[[variable]]==label,]
  }
  commdata <- toyspace::mob_indic(tabflows = tabflows, poptab = poptab, idori = idori, iddes = iddes, idflow = idflow, pol = pol, idpol = idpol)
  commdata <- left_join(commdata, centPol[c("insee","lat","lon")],by = "insee")
  shinyjs::hideElement(id = 'loading')
  
  print("filter indice fin")
  end_time <- Sys.time()
  print(end_time - start_time)
  return(commdata)
}


Index <- function(mobi,commdata){
  if(mobi=="emploi"){
    data <- commdata$TOTDES
    nom <- ""
    unit <- "emplois"
    color <- "PuOr"
    breaks <- replace(sort(append(0,getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks"))), 6, max(commdata$RelBal)+0.1)
    layer <- "taux"
    polygons <- ""
  } else if(mobi=="popact"){
    data <- commdata$TOTORI
    nom <- ""
    unit <- "actifs"
    color <- "PuOr"
    breaks <- replace(sort(append(0,getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks"))), 6, max(commdata$RelBal)+0.1)
    layer <- "taux"
    polygons <- ""}
  else if(mobi=="soldeRel"){
    data <- commdata$RelBal
    nom <- "Solde relatif : "
    unit <- ""
    color <- "PuOr"
    breaks <- unique(sort(round(replace(append(0,getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks")), length(getBreaks(commdata$RelBal,nclass = 6,method = "fisher-jenks"))+1, max(commdata$RelBal, na.rm = TRUE)+0.1), digit = 2)))
    layer <- "stock"
    polygons <- "communes"}
  else if(mobi=="Contention"){
    data <- commdata$Contention
    nom <- "Auto-Contention : "
    unit <- "%"
    color <- "Purples"
    breaks <- unique(sort(replace(round(getBreaks(commdata$Contention,nclass = 6,method = "fisher-jenks"), digit = 2), length(getBreaks(commdata$Contention,nclass = 6,method = "fisher-jenks"))+1, max(commdata$Contention, na.rm = TRUE)+0.1)))
    layer <- "stock"
    polygons <- "communes"}
  else if(mobi=="Suffisance"){
    data <- commdata$AutoSuff
    nom <- "Auto-Suffisance : "
    unit <- "%"
    color <- "Purples"
    breaks <- unique(sort(replace(round(getBreaks(commdata$AutoSuff,nclass = 6,method = "fisher-jenks"), digit = 2), length(getBreaks(commdata$AutoSuff,nclass = 6,method = "fisher-jenks"))+1, max(commdata$AutoSuff, na.rm = TRUE)+0.1)))
    layer <- "stock"
    polygons <- "communes"}
  else if(mobi=="meanDistOri"){
    data <- commdata$DISTORI
    nom <- "Distance moyenne à l'origine : "
    unit <- "km"
    color <- "Purples"
    breaks <- unique(sort(replace(round(getBreaks(commdata$DISTORI,nclass = 6,method = "fisher-jenks"), digit = 2),length(getBreaks(commdata$DISTORI,nclass = 6,method = "fisher-jenks"))+1, max(commdata$DISTORI, na.rm = TRUE)+0.1)))
    layer <- "stock"
    polygons <- "communes"}
  else if(mobi=="meanDistDes"){
    data <- commdata$DISTDES
    nom <- "Distance moyenne à destination : "
    unit <- "km"
    color <- "Purples"
    breaks <- unique(sort(replace(round(getBreaks(commdata$DISTDES,nclass = 6,method = "fisher-jenks"), digit = 2),length(getBreaks(commdata$DISTDES,nclass = 6,method = "fisher-jenks"))+1, max(commdata$DISTDES, na.rm = TRUE)+0.1)))
    layer <- "stock"
    polygons <- "communes"}
  else if(mobi=="perOri"){
    data <- commdata$PerOri
    nom <- "Part des flux à l'origine : "
    unit <- "%"
    color <- "Purples"
    breaks <- unique(sort(replace(round(getBreaks(commdata$PerOri,nclass = 6,method = "fisher-jenks"), digit = 2), length(getBreaks(commdata$PerOri,nclass = 6,method = "fisher-jenks"))+1, max(commdata$PerOri, na.rm = TRUE)+0.1)))
    layer <- "stock"
    polygons <- "communes"}
  else if(mobi=="perDes"){
    data <- commdata$PerDes
    nom <- "Part des flux à la destination : "
    unit <- "%"
    color <- "Purples"
    breaks <- unique(sort(replace(round(getBreaks(commdata$PerDes,nclass = 6,method = "fisher-jenks"), digit = 2), length(getBreaks(commdata$PerDes,nclass = 6,method = "fisher-jenks"))+1, max(commData$PerDes, na.rm = TRUE)+0.1)))
    layer <- "stock"
    polygons <- "communes"}
  return(list(data = data,nom = nom,unit = unit,color = color,breaks = breaks,layer = layer,polygons = polygons, commdata = commdata))
}





# get flux ----

Filter_flux <- function(tabflows, variable, label){
  if(is.null(variable) == TRUE | is.null(label) == TRUE){
    tabflows <- tabflows
  }else{
    tabflows <- tabflows[tabflows[[variable]]==label,]
  }
  shinyjs::hideElement(id = 'loading')
  return(tabflows)
}


GetLinks <- function(tabnav, tabdist, idori, iddes, iddist, ref, varsort, oneunit, thres){
  print("getlink debut")
  start_time <- Sys.time()
  tabnav$KEY <- paste(tabnav[[idori]], tabnav[[iddes]], sep = "_")
  tabdist$KEY <- paste(tabdist[[idori]], tabdist[[iddes]], sep = "_")
  tabnav <- left_join(tabnav, tabdist[, c(iddist, "KEY")], by = "KEY") 
  tabnav$DISTTOT <- tabnav$DIST*tabnav$FLOW
  
  refLib <- paste0(ref, "LIB")
  oriDes <- paste0(c("ORI", "DES"), "LIB")
  invRef <- oriDes[oriDes != refLib]
  tabSel <- tabnav %>% 
    group_by(ORI, DES) %>% 
    summarise(FLOW = sum(FLOW), DIST = first(DIST), DISTTOT = sum(DISTTOT), ORILIB = first(ORILIB), DESLIB = first(DESLIB)) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  tabSel <- tabSel[tabSel[[refLib]] == oneunit, ]
  tabSel <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
  nbRows <- ifelse(thres > nrow(tabSel), nrow(tabSel), thres)
  if (dim(tabSel)[1] == 0) {
    #get a "false" link with the same departure and arrival so it seems unexisting
    spLinks <- getLinkLayer(x = pol, df = tabFlows[1:0, c("ORI", "DES")])
  }else{
    spLinks <- getLinkLayer(x = pol, df = tabSel[1:nbRows, c("ORI", "DES")])
  }
  topDes <- spLinks
  print("getlink fin")
  end_time <- Sys.time()
  print(end_time - start_time)
  return(topDes)
}

city_Value <- function(tabflows,matDist,pol,idpol,var, ref,oneunit){
  print("cityval debut")
  start_time <- Sys.time()
  tabIndex <- expand.grid(ORI = pol[[idpol]],
                          DES = pol[[idpol]],
                          stringsAsFactors = FALSE)
  tabIndex <- left_join(x = tabIndex, y = tabflows, by = c("ORI", "DES"))
  if(ref == "ORI"){
    matflow <- dcast(tabIndex, formula = DES ~ ORI, value.var = "FLOW", fun.aggregate = sum)
    matflow <- as.matrix(matflow[, -1])
    matflow[is.na(matflow)] <- 0
    tabCityDist <- as.data.frame(matDist[oneunit,])
    tabCityFlow <- as.data.frame(matflow[,oneunit])
  }else{
    matflow <- dcast(tabIndex, formula = ORI ~ DES, value.var = "FLOW", fun.aggregate = sum)
    matflow <- as.matrix(matflow[, -1])
    matflow[is.na(matflow)] <- 0
    tabCityDist <- as.data.frame(matDist[,oneunit])
    tabCityFlow <- as.data.frame(matflow[,oneunit])
  }
  colnames(tabCityFlow)<-"DATA"
  colnames(tabCityDist)<-"DIST"
  tabCityFlow$ID <- colnames(matflow)
  tabCityDist$ID <- rownames(tabCityDist)
  tabCityDist <- dplyr::left_join(tabCityFlow,tabCityDist, by = "ID")
  tabCityDist$DATA <- tabCityDist$DATA * tabCityDist$DIST
  if(var == "FLOW"){
    tabCity <- tabCityFlow
    index <- "actifs"
  }else{
    tabCity <- tabCityDist
    index <- "km"
  }
  spcom <- dplyr::left_join(pol,tabCity, by = c("insee" ="ID"))
  if(sum(tabCityFlow$DATA) == 0 |sum(tabCityDist$DATA) == 0){
    breaks <- c(0,1)
  }else{
    breaks <- sort(unique(append(abs(round(replace(getBreaks(spcom$DATA, nclass = 6,method = "fisher-jenks"),length(getBreaks(spcom$DATA, nclass = 6,method = "fisher-jenks"))+1,max(spcom$DATA)+1))),0)))
  }
  print("cityval fin")
  end_time <- Sys.time()
  print(end_time - start_time)
  return(list( SPCOM = spcom, INDEX = index, BREAKS = breaks))
}

# get structure----
Structure <- function(Flu,domFlowJob,domFlowPop,domFlowJP){
  if(Flu=="iEmploi"){
    dataflu <- domFlowJob$FLOWS
    rayon <- ((sqrt(domFlowJob$PTS$TOTDES)/pi)*20)+200
    cercle <- domFlowJob$PTS
    valCercle <- domFlowJob$PTS$TOTDES
    nom <- "Emploi : "
    comm <- toupper(domFlowJob$PTS$nomcom)}
  else if(Flu=="iPopulation"){
    dataflu <- domFlowPop$FLOWS
    rayon <- ((sqrt(domFlowPop$PTS$TOTORI)/pi)*20)+200
    cercle <- domFlowPop$PTS
    valCercle <- domFlowPop$PTS$TOTORI
    nom <- "Population : "
    comm <- toupper(domFlowPop$PTS$nomcom)}
  else if(Flu=="iEmpPop"){
    dataflu <- domFlowJP$FLOWS
    rayon <- ((sqrt(domFlowJP$PTS$TOTORIDES)/pi)*20)+200
    cercle <- domFlowJP$PTS
    valCercle <- domFlowJP$PTS$TOTORIDES
    nom <- "Population et emplois : "
    comm <- toupper(domFlowJP$PTS$nomcom)}
  return(list(dataflu = dataflu, rayon = rayon, cercle = cercle, valCercle = valCercle, nom = nom, comm = comm, dataflu = dataflu))
}

Filter_structure <- function(tabflows, idflow, before, after, pol, idpol, namepol, nameAgr, variable, label, centPol, idCentPol, poptab){
  shinyjs::showElement(id = 'loading')
  print("structure debut")
  start_time <- Sys.time()
  if(is.null(variable) == TRUE | is.null(label) == TRUE){
    tabflows <- tabflows
  }else{
    tabflows <- tabflows[tabflows[[variable]]==label,]
  }
  tabFlowsAgr <- toyspace::city_aggregate(before = before,after = after,tabflows = tabflows,idori = "ORI",iddes = "DES")
  domFlowJob <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW", poptab,
                                        weight = "destination", threspct = 1, pol = polAgr, idpol = "IDAGR", centPol = centPolAgr, idCentPol = "IDAGR")
  domFlowPop <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow, poptab,
                                        weight = "origin", threspct = 1, pol = polAgr, idpol = "IDAGR", centPol = centPolAgr, idCentPol = "IDAGR")
  domFlowJP <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow, poptab,
                                       weight = "sum", threspct = 1, pol = polAgr, idpol = "IDAGR", centPol = centPolAgr, idCentPol = "IDAGR")
  shinyjs::hideElement(id = 'loading')
  
  print("structure fin")
  end_time <- Sys.time()
  print(end_time - start_time)
  return(list(domFlowJob = domFlowJob, domFlowPop = domFlowPop, domFlowJP = domFlowJP))
}

# Miscelaneous ----

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.8){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-bottom: 10px;line-height: ", sizes, "px;'>", labels, "</div>")
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = "bottomright"))
}


arr_aggregate <- function(before, after, tabflows, idori, iddes){
  dicoAgr <- data.frame("OLDCODE" = before, "NEWCODE" = after)
  tabflows$ORIAGR <- map_values(x = tabflows[[idori]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  tabflows$DESAGR <- map_values(x = tabflows[[iddes]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  return(tabflows)
}

spatunit_indices <- function(tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[, idori]
  tabflows$DES <- tabflows[, iddes]
  tabflows$FLOW <- tabflows[, idflow]
  tabflowIntra <- tabflows %>% filter(ORI == DES)
  if(nrow(tabflowIntra) == 0){
    totIntra <- data.frame(ORI = NA, TOTINTRA = NA)
  } else {
    totIntra <- tabflowIntra %>% 
      group_by(ORI) %>% 
      summarise(TOTINTRA = sum(FLOW, na.rm = TRUE)) %>% 
      ungroup()
  }
  
  tabflowOri <- tabflows[tabflows[idori] != tabflows[iddes], ]
  tabflowOri <- aggregate(x = tabflowOri[[idflow]], by = list(tabflowOri[[idori]]), FUN = sum)
  colnames(tabflowOri) <- c("ORI","TOTORI")
  tabflowDes <- tabflows[tabflows[idori] != tabflows[iddes], ]
  tabflowDes <- aggregate(x = tabflowDes[[idflow]], by = list(tabflowDes[[iddes]]), FUN = sum)
  colnames(tabflowDes) <- c("DES","TOTDES")
  tabflowDistOri <- aggregate(x = tabflows[[iddist]] , by = list(tabflows[[idori]]), FUN = sum)
  colnames(tabflowDistOri) <- c("ORI","DISTORI")
  tabflowDistDes <- aggregate(x = tabflows[[iddist]], by = list(tabflows[[iddes]]), FUN = sum)
  colnames(tabflowDistDes) <- c("DES","DISTDES")
  
  if(nrow(tabflowIntra) == 0){
    tabflowIntra <- data.frame(ORI = unique(c(tabflows[[idori]], tabflows[[iddes]])), TOTINTRA = 0)
  } else {
    tabflowIntra <- tabflowIntra}
  poptab <- merge(x = tabflowIntra, y = tabflowOri, by.x = "ORI", by.y ="ORI", all.x = TRUE, all.y = TRUE)
  poptab <- merge(x = poptab, y = tabflowDes, by.x = "ORI", by.y ="DES", all.x = TRUE, all.y = TRUE)
  poptab <- merge(x = poptab, y = tabflowDistOri, by.x = "ORI", by.y ="ORI", all.x = TRUE, all.y = TRUE)
  poptab <- merge(x = poptab, y = tabflowDistDes, by.x = "ORI", by.y ="DES", all.x = TRUE, all.y = TRUE)
  
  poptab$TOTINTRA <- ifelse(is.na(poptab$TOTINTRA), 0, poptab$TOTINTRA)
  poptab$TOTORI <- ifelse(is.na(poptab$TOTORI), 0, poptab$TOTORI)
  poptab$TOTDES <- ifelse(is.na(poptab$TOTDES), 0, poptab$TOTDES)
  poptab$DISTORI <- ifelse(is.na(poptab$DISTORI), 0, poptab$DISTORI)
  poptab$DISTDES <- ifelse(is.na(poptab$DISTDES), 0, poptab$DISTDES)
  
  poptab$TOTORIDES <- poptab$TOTORI + poptab$TOTDES
  
  colnames(poptab) <- c("CODGEO", "TOTINTRA","TOTORI", "TOTDES","DISTORI","DISTDES","TOTORIDES")
  return(poptab)
}

mob_indic <- function(tabflows, idori, iddes, idflow, pol, idpol, poptab){
  
  # auto-contention
  poptab$Contention <- (poptab$TOTINTRA / (poptab$TOTORI + poptab$TOTINTRA))*100
  poptab$Contention <- ifelse(is.na(poptab$Contention), 0, poptab$Contention)
  
  # auto-sufficiency
  poptab$AutoSuff <- (poptab$TOTINTRA / (poptab$TOTDES + poptab$TOTINTRA))*100
  poptab$AutoSuff <- ifelse(is.na(poptab$AutoSuff), 0, poptab$AutoSuff)
  
  # Relative Balance
  poptab$RelBal <- (poptab$TOTDES - poptab$TOTORI) / (poptab$TOTORI + poptab$TOTDES)
  poptab$RelBal <- ifelse(is.na(poptab$RelBal), 0, poptab$RelBal)
  
  # Difference
  poptab$Difference <- poptab$TOTDES - poptab$TOTORI
  poptab$Difference <- ifelse(is.na(poptab$Difference), 0, poptab$Difference)
  
  # Percentage of total flows at origin
  poptab$PerOri <- (poptab$TOTORI*100) / sum(poptab$TOTORI)
  poptab$PerOri <- ifelse(is.na(poptab$PerOri), 0, poptab$PerOri)
  
  # Percentage of total flows at destination
  poptab$PerDes <- (poptab$TOTDES*100) / sum(poptab$TOTDES)
  poptab$PerDes <- ifelse(is.na(poptab$PerDes), 0, poptab$PerDes)
  
  # Percentage of total internal flows
  poptab$PerIntra <- (poptab$TOTINTRA*100) / sum(poptab$TOTINTRA)
  poptab$PerIntra <- ifelse(is.na(poptab$PerIntra), 0, poptab$PerIntra)
  
  polTabFull <- left_join(pol, poptab, by = c(idpol = "CODGEO"))
  
  return(polTabFull)
}
