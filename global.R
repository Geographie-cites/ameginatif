###########################################
##### Ameginatif - global functions #######
###########################################

# load packages ----

library(shiny)
library(shinythemes)
library(shinyBS)
library(classInt)
library(sf)
library(leaflet)
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

vecColors <- c("#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A")
names(vecColors) <- c("TOTORI", "SUMDISTORI", "TOTDES", "SUMDISTDES")

vecPals <- c("OrRd", "YlOrBr", "PuBu", "BuPu", "Purples")
names(vecPals) <- c("AVGDISTORI", "AUTOCONT", "AVGDISTDES", "AUTOSUFF", "RELBAL")


# get top links ----

get_toplinks <- function(tabflows, pol, ref, varsort, oneunit, thres){
  refLib <- paste0(ref, "LIB")
  oriDes <- paste0(c("ORI", "DES"), "LIB")
  invRef <- oriDes[oriDes != refLib]
  tabflows <- tabflows %>% 
    mutate(FLOWDIST = FLOW * DIST) %>% 
    group_by(ORI, DES) %>% 
    summarise(FLOW = sum(FLOW, na.rm = TRUE), 
              SUMDIST = sum(FLOWDIST, na.rm = TRUE), 
              ORILIB = first(ORILIB), 
              DESLIB = first(DESLIB)) %>% 
    ungroup()
    
  tabSel <- tabflows[tabflows[[refLib]] == oneunit, ] 
  tabSel <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
  
  nbRows <- ifelse(thres > nrow(tabSel), nrow(tabSel), thres)
  spLinks <- get_linklayer(x = pol, df = tabSel[1:nbRows, c("ORI", "DES")])
  spPol <- pol[pol$CODGEO %in% spLinks$DES, ]
  topDes <- list(POLYG = spPol, LINES = spLinks)
  return(topDes)
}


# build palette ----

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

# get link layer (from cartography packages) ----

get_linklayer <- function (x, xid = NULL, df, dfid = NULL, spdf, spdf2 = NULL, 
                           spdfid = NULL, spdf2id = NULL, dfids = NULL, dfide = NULL) 
{
  if (sum(missing(spdf), is.null(spdf2), is.null(spdfid), is.null(spdf2id), 
          is.null(dfids), is.null(dfide)) != 6) {
    stop("spdf, spdf2, spdfid, spdf2id, dfids and dfide are defunct arguments; last used in version 1.4.2.", 
         call. = FALSE)
  }
  if (methods::is(x, "Spatial")) {
    x <- sf::st_as_sf(x)
  }
  if (is.null(xid)) {
    xid <- names(x)[1]
  }
  if (is.null(dfid)) {
    dfid <- names(df)[1:2]
  }
  x2 <- data.frame(id = x[[xid]], sf::st_coordinates(sf::st_centroid(x = sf::st_geometry(x), 
                                                                     of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))), 
                   stringsAsFactors = FALSE)
  df <- df[, dfid]
  link <- merge(df, x2, by.x = dfid[2], by.y = "id", all.x = TRUE)
  link <- merge(link, x2, by.x = dfid[1], by.y = "id", all.x = TRUE)
  names(link)[3:6] <- c("xj", "yj", "xi", "yi")
  d1 <- nrow(link)
  link <- link[!is.na(link$xj) & !is.na(link$xi), ]
  d2 <- nrow(link)
  if (d2 == 0) {
    stop("No links were created. dfid and xid do not match", 
         call. = FALSE)
  }
  if ((d1 - d2) > 0) {
    warning(paste0((d1 - d2), " links were not created. Some dfid were not found in xid"), 
            call. = FALSE)
  }
  stringo <- paste0("LINESTRING(", link$xi, " ", link$yi, ", ", 
                    link$xj, " ", link$yj, ")")
  link <- sf::st_sf(link[, dfid], geometry = sf::st_as_sfc(stringo, 
                                                           crs = sf::st_crs(x)))
  return(link)
}


# map values (from plyr package) ----

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
