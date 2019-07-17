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
library(dplyr)


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
