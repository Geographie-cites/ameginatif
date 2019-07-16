###########################################
##### Ameginatif - global functions #######
###########################################

# load packages ----

library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(classInt)
library(sf)
library(raster)
library(leaflet)
library(flows)
library(reshape2)
library(tidyverse)


# Load data ----

muniBound <- readRDS("data/muniboundgeo.Rds")
muniBoundAgr <- readRDS("data/muniboundgeoag.Rds")
listTabflows <- readRDS(file ="data/listtabflowslabels.Rds")
externalFeatures <- readRDS(file = "data/extfeatures.Rds")

# prepare data ----

arrDesagg <- paste0("751", ifelse(1:20 < 10, paste0("0", 1:20), 1:20))
arrAggreg <- "75056"

dicoUnits <- c("actifs", "emplois", "individus", "%", "%", "%", "km/pers.", "km/pers.", "km", "km")
names(dicoUnits) <- c("TOTORI", "TOTDES", "ABSBAL", "RELBAL", "AUTOCONT", "AUTOSUFF", "AVGDISTORI", "AVGDISTDES", "SUMDISTORI", "SUMDISTDES")


# Selection and other functions ----

# GetLinks <- function(tabnav, tabdist, idori, iddes, iddist, ref, varsort, oneunit, thres){
#   print("getlink debut")
#   start_time <- Sys.time()
#   tabnav$KEY <- paste(tabnav[[idori]], tabnav[[iddes]], sep = "_")
#   tabdist$KEY <- paste(tabdist[[idori]], tabdist[[iddes]], sep = "_")
#   tabnav <- left_join(tabnav, tabdist[, c(iddist, "KEY")], by = "KEY") 
#   tabnav$DISTTOT <- tabnav$DIST*tabnav$FLOW
#   
#   refLib <- paste0(ref, "LIB")
#   oriDes <- paste0(c("ORI", "DES"), "LIB")
#   invRef <- oriDes[oriDes != refLib]
#   tabSel <- tabnav %>% 
#     group_by(ORI, DES) %>% 
#     summarise(FLOW = sum(FLOW), DIST = first(DIST), DISTTOT = sum(DISTTOT), ORILIB = first(ORILIB), DESLIB = first(DESLIB)) %>% 
#     as.data.frame(stringsAsFactors = FALSE)
#   tabSel <- tabSel[tabSel[[refLib]] == oneunit, ]
#   tabSel <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
#   nbRows <- ifelse(thres > nrow(tabSel), nrow(tabSel), thres)
#   if (dim(tabSel)[1] == 0) {
#     #get a "false" link with the same departure and arrival so it seems unexisting
#     spLinks <- getLinkLayer(x = pol, df = tabFlows[1:0, c("ORI", "DES")])
#   }else{
#     spLinks <- getLinkLayer(x = pol, df = tabSel[1:nbRows, c("ORI", "DES")])
#   }
#   topDes <- spLinks
#   print("getlink fin")
#   end_time <- Sys.time()
#   print(end_time - start_time)
#   return(topDes)
# }
# 
# city_Value <- function(tabflows,matDist,pol,idpol,var, ref,oneunit){
#   print("cityval debut")
#   start_time <- Sys.time()
#   tabIndex <- expand.grid(ORI = pol[[idpol]],
#                           DES = pol[[idpol]],
#                           stringsAsFactors = FALSE)
#   tabIndex <- left_join(x = tabIndex, y = tabflows, by = c("ORI", "DES"))
#   if(ref == "ORI"){
#     matflow <- dcast(tabIndex, formula = DES ~ ORI, value.var = "FLOW", fun.aggregate = sum)
#     matflow <- as.matrix(matflow[, -1])
#     matflow[is.na(matflow)] <- 0
#     tabCityDist <- as.data.frame(matDist[oneunit,])
#     tabCityFlow <- as.data.frame(matflow[,oneunit])
#   }else{
#     matflow <- dcast(tabIndex, formula = ORI ~ DES, value.var = "FLOW", fun.aggregate = sum)
#     matflow <- as.matrix(matflow[, -1])
#     matflow[is.na(matflow)] <- 0
#     tabCityDist <- as.data.frame(matDist[,oneunit])
#     tabCityFlow <- as.data.frame(matflow[,oneunit])
#   }
#   colnames(tabCityFlow)<-"DATA"
#   colnames(tabCityDist)<-"DIST"
#   tabCityFlow$ID <- colnames(matflow)
#   tabCityDist$ID <- rownames(tabCityDist)
#   tabCityDist <- dplyr::left_join(tabCityFlow,tabCityDist, by = "ID")
#   tabCityDist$DATA <- tabCityDist$DATA * tabCityDist$DIST
#   if(var == "FLOW"){
#     tabCity <- tabCityFlow
#     index <- "actifs"
#   }else{
#     tabCity <- tabCityDist
#     index <- "km"
#   }
#   spcom <- dplyr::left_join(pol,tabCity, by = c("insee" ="ID"))
#   if(sum(tabCityFlow$DATA) == 0 |sum(tabCityDist$DATA) == 0){
#     breaks <- c(0,1)
#   }else{
#     breaks <- sort(unique(append(abs(round(replace(getBreaks(spcom$DATA, nclass = 6,method = "fisher-jenks"),length(getBreaks(spcom$DATA, nclass = 6,method = "fisher-jenks"))+1,max(spcom$DATA)+1))),0)))
#   }
#   print("cityval fin")
#   end_time <- Sys.time()
#   print(end_time - start_time)
#   return(list( SPCOM = spcom, INDEX = index, BREAKS = breaks))
# }
# 
# # get structure----
# Structure <- function(Flu,domFlowJob,domFlowPop,domFlowJP){
#   if(Flu=="iEmploi"){
#     dataflu <- domFlowJob$FLOWS
#     rayon <- ((sqrt(domFlowJob$PTS$TOTDES)/pi)*20)+200
#     cercle <- domFlowJob$PTS
#     valCercle <- domFlowJob$PTS$TOTDES
#     nom <- "Emploi : "
#     comm <- toupper(domFlowJob$PTS$nomcom)}
#   else if(Flu=="iPopulation"){
#     dataflu <- domFlowPop$FLOWS
#     rayon <- ((sqrt(domFlowPop$PTS$TOTORI)/pi)*20)+200
#     cercle <- domFlowPop$PTS
#     valCercle <- domFlowPop$PTS$TOTORI
#     nom <- "Population : "
#     comm <- toupper(domFlowPop$PTS$nomcom)}
#   else if(Flu=="iEmpPop"){
#     dataflu <- domFlowJP$FLOWS
#     rayon <- ((sqrt(domFlowJP$PTS$TOTORIDES)/pi)*20)+200
#     cercle <- domFlowJP$PTS
#     valCercle <- domFlowJP$PTS$TOTORIDES
#     nom <- "Population et emplois : "
#     comm <- toupper(domFlowJP$PTS$nomcom)}
#   return(list(dataflu = dataflu, rayon = rayon, cercle = cercle, valCercle = valCercle, nom = nom, comm = comm, dataflu = dataflu))
# }
# 
# Filter_structure <- function(tabflows, idflow, before, after, pol, idpol, namepol, nameAgr, variable, label, centPol, idCentPol, poptab){
#   shinyjs::showElement(id = 'loading')
#   print("structure debut")
#   start_time <- Sys.time()
#   if(is.null(variable) == TRUE | is.null(label) == TRUE){
#     tabflows <- tabflows
#   }else{
#     tabflows <- tabflows[tabflows[[variable]]==label,]
#   }
#   tabFlowsAgr <- toyspace::city_aggregate(before = before,after = after,tabflows = tabflows,idori = "ORI",iddes = "DES")
#   domFlowJob <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = "FLOW", poptab,
#                                         weight = "destination", threspct = 1, pol = polAgr, idpol = "IDAGR", centPol = centPolAgr, idCentPol = "IDAGR")
#   domFlowPop <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow, poptab,
#                                         weight = "origin", threspct = 1, pol = polAgr, idpol = "IDAGR", centPol = centPolAgr, idCentPol = "IDAGR")
#   domFlowJP <- toyspace::nystuen_dacey(tabflows = tabFlowsAgr, idori = "ORIAGR", iddes = "DESAGR", idflow = idflow, poptab,
#                                        weight = "sum", threspct = 1, pol = polAgr, idpol = "IDAGR", centPol = centPolAgr, idCentPol = "IDAGR")
#   shinyjs::hideElement(id = 'loading')
#   
#   print("structure fin")
#   end_time <- Sys.time()
#   print(end_time - start_time)
#   return(list(domFlowJob = domFlowJob, domFlowPop = domFlowPop, domFlowJP = domFlowJP))
# }


######################
#### MISCELANEOUS ----
######################


# build palette

build_palette <- function(x, palseq = NULL) {
  valMin <- min(x, na.rm = TRUE)
  valMax <- max(x, na.rm = TRUE)
  if(valMin >= 0) {
    brks <- classIntervals(var = x, n = 6, style = "fisher")$brks
    myPal <- colorBin(palette = palseq,
                      bins = brks,
                      domain = x,
                      na.color = "transparent",
                      pretty = TRUE,
                      reverse = FALSE)
  } else {
    oneThres <- ifelse(abs(valMin) > valMax, abs(valMin), valMax)
    binWidth <- oneThres / 3
    brks <- c(-oneThres, -2 * binWidth, -1 * binWidth, 0, binWidth, 2 * binWidth, oneThres)
    myPal <- colorBin(palette = "PuOr",
                      bins = brks,
                      domain = x,
                      na.color = "transparent",
                      pretty = TRUE,
                      reverse = TRUE)
  }
  return(myPal)
}


# custom legend ----

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.8){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-bottom: 10px;line-height: ", sizes, "px;'>", labels, "</div>")
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = "bottomright"))
}

# aggregate flows (arrondissements) ----

arrflow_aggregate <- function(before, after, tabflows, idori, iddes){
  dicoAgr <- tibble(OLDCODE = before, NEWCODE = after)
  tabflows$ORIAGR <- map_values(x = tabflows[[idori]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  tabflows$DESAGR <- map_values(x = tabflows[[iddes]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  return(tabflows)
}

# aggregate flows (arrondissements) ----

arrunit_aggregate <- function(before, after, pol, idpol){
  idpol <- enquo(idpol)
  dicoAgr <- tibble(OLDCODE = before, NEWCODE = after)
  polAgr <- pol %>%
    mutate(CODGEO = map_values(x = pol %>% st_set_geometry(NULL) %>% pull(!!idpol), from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)) %>%
    select(!!idpol) %>%
    group_by(!!idpol) %>%
    summarise()
  
  coords <- polAgr %>% st_centroid() %>% st_coordinates()
  polAgr$LON <- coords[, 1]
  polAgr$LAT <- coords[, 2]
  
  return(polAgr)
}

# compute summary statistics at spatial unit level ----

spatunit_indices <- function(pol, tabflows, idpol, idori, iddes, idflow, iddist){
  idpol <- enquo(idpol)
  idori <- enquo(idori)
  iddes <- enquo(iddes)
  idflow <- enquo(idflow)
  iddist <- enquo(iddist)
  
  tabflows <- tabflows %>% 
    mutate(FLOW = !!idflow, 
           DIST = !!iddist, 
           COMDISTPROD = FLOW * DIST)
  
  if(nrow(tabflows %>% filter(!!idori == !!iddes)) == 0){
    intraSum <- tibble(CODGEO = pol %>% pull(!!idpol), TOTINTRA = NA)
  } else {
    intraSum <- tabflows %>% 
      filter(!!idori == !!iddes) %>% 
      group_by(!!idori) %>% 
      summarise(TOTINTRA = sum(!!idflow, na.rm = TRUE)) %>% 
      transmute(CODGEO = !!idori, TOTINTRA)
  }
  
  oriSum <- tabflows %>% 
    group_by(!!idori) %>% 
    summarise(TOTORI = sum(!!idflow, na.rm = TRUE),
              SUMDISTORI = sum(COMDISTPROD, na.rm = TRUE),
              AVGDISTORI = SUMDISTORI / TOTORI) %>% 
    ungroup() %>% 
    transmute(CODGEO = !!idori, TOTORI, SUMDISTORI, AVGDISTORI)
  
  desSum <- tabflows %>% 
    group_by(!!iddes) %>% 
    summarise(TOTDES = sum(!!idflow, na.rm = TRUE),
              SUMDISTDES = sum(COMDISTPROD, na.rm = TRUE),
              AVGDISTDES = SUMDISTDES / TOTDES) %>% 
    ungroup() %>% 
    transmute(CODGEO = !!iddes, TOTDES, SUMDISTDES, AVGDISTDES)
  
  tabUnits <- pol %>%
    mutate(CODGEO = !!idpol) %>% 
    left_join(y = intraSum, by = "CODGEO") %>%
    left_join(y = oriSum, by = "CODGEO") %>%
    left_join(y = desSum, by = "CODGEO") %>%
    replace_na(replace = list(TOTINTRA = 0, TOTORI = 0, TOTDES = 0, DISTORI = 0, DISTDES = 0, AVGDISTORI = 0, AVGDISTDES = 0))
  
  allIndices <- tabUnits %>% 
    mutate(AUTOCONT = 100 * TOTINTRA / TOTORI,
           AUTOSUFF = 100 * TOTINTRA / TOTDES,
           ABSBAL = (TOTDES - TOTINTRA) - (TOTORI - TOTINTRA),
           RELBAL = ((TOTDES - TOTINTRA) - (TOTORI - TOTINTRA)) / ((TOTDES - TOTINTRA) + (TOTORI - TOTINTRA))) %>% 
    replace_na(replace = list(AUTOCONT = 0, AUTOSUFF = 0, ABSBAL = 0, RELBAL = 0))
  
  return(allIndices)
}


# compute dominant flows ----

nystuen_dacey <- function(pol, tabflows, idpol, idori, iddes, idflow, wgt){
  idpol <- enquo(idpol)
  idori <- enquo(idori)
  iddes <- enquo(iddes)
  idflow <- enquo(idflow)
  wgt <- enquo(wgt)
  
  infoCom <- pol %>% 
    st_set_geometry(NULL) %>% 
    mutate(CODGEO = !!idpol,
           WGT = !!wgt)
  
  tabFlows <- tabflows %>% 
    mutate(ORI == !!idori, DES == !!iddes) %>% 
    group_by(ORI, DES) %>% 
    summarise(FLOW = sum(!!idflow, na.rm = TRUE)) %>% 
    ungroup()
  
  flowsMax <- tabFlows %>%
    filter(!!idori != !!iddes) %>% 
    group_by(!!idori) %>% 
    arrange(desc(!!idflow)) %>% 
    slice(1) %>% 
    left_join(y = infoCom[, c("CODGEO", "WGT", "LON", "LAT")], by = c("ORI" = "CODGEO")) %>%
    left_join(y = infoCom[, c("CODGEO", "WGT", "LON", "LAT")], by = c("DES" = "CODGEO")) %>% 
    filter(WGT.x < WGT.y)
  
  charLines <- paste0("LINESTRING(", flowsMax$LON.x, " ", flowsMax$LAT.x, ", ", flowsMax$LON.y, " ", flowsMax$LAT.y, ")")
  spatLines <- st_sf(1:length(charLine), geometry = st_as_sfc(charLine, crs = 4326))
  
  
  domFlows <- graph.data.frame(flowsMax %>% select(ORI, DES), directed = TRUE)
  V(domFlows)$DEG <- degree(domFlows, mode = "in")
  typNode <- igraph::as_data_frame(x = domFlows, what = "vertices") %>% 
    arrange(desc(DEG)) %>% 
    mutate(TYPE = ifelse(DEG > 0.333 * vcount(domFlows), 1, 
                         ifelse(DEG > 2, 2, 3))) %>% 
    left_join(infoCom[, c("CODGEO", "LON", "LAT")], by = c("name" = "CODGEO"))
  
  return(list(FLOWS = spatLines, NODES = typNode))
}



map_values <- function (x, from, to, warn_missing = TRUE)
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- map_values(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}
