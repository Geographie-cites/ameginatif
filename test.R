
tabflows <- arrflow_aggregate(before = arrDesagg, after = arrAggreg, tabflows = listTabflows$FIN_ACT, idori = "ORI", iddes = "DES")

pol = spatunit_indices(pol = muniBoundAgr, tabflows = tabFlows, idpol = CODGEO, idori = ORI, iddes = DES, idflow = FLOW, iddist = DIST)


nystuen_dacey <- function(pol, tabflows, idpol, idori, iddes, idflow){
  idpol <- enquo(idpol)
  idori <- enquo(idori)
  iddes <- enquo(iddes)
  idflow <- enquo(idflow)
  
  infoCom <- pol %>% 
    st_set_geometry(NULL) %>% 
    mutate(WGT = TOTORI + TOTDES - TOTINTRA)
  
  tabFlows <- tabflows %>% 
    group_by(ORI, DES) %>% 
    summarise(FLOW = sum(FLOW, na.rm = TRUE)) %>% 
    ungroup()
  
  flowsMax <- tabFlows %>%
    filter(ORI != DES) %>% 
    group_by(ORI) %>% 
    arrange(desc(FLOW)) %>% 
    slice(1) %>% 
    left_join(y = infoCom[, c("CODGEO", "WGT", "LON", "LAT")], by = c("ORI" = "CODGEO")) %>%
    left_join(y = infoCom[, c("CODGEO", "WGT", "LON", "LAT")], by = c("DES" = "CODGEO")) %>% 
    filter(WGT.x < WGT.y) %>% 
    ungroup()
  
  charLines <- paste0("LINESTRING(", flowsMax$LON.x, " ", flowsMax$LAT.x, ", ", flowsMax$LON.y, " ", flowsMax$LAT.y, ")")
  spatLines <- st_sf(1:length(charLines), geometry = st_as_sfc(charLines, crs = 4326))
  
  
  domFlows <- graph.data.frame(flowsMax %>% select(ORI, DES), directed = TRUE)
  V(domFlows)$DEG <- degree(domFlows, mode = "in")
  typNode <- igraph::as_data_frame(x = domFlows, what = "vertices") %>% 
    arrange(desc(DEG)) %>% 
    mutate(TYPE = ifelse(DEG > 0.333 * vcount(domFlows), 1, 
                         ifelse(DEG > 2, 2, 3))) %>% 
    left_join(infoCom[, c("CODGEO", "LIBGEO", "LON", "LAT")], by = c("name" = "CODGEO")) %>% 
    st_as_sf(coords = c("LON", "LAT"), 
             agr = "constant",
             crs = 4326,
             stringsAsFactors = FALSE)
  
  return(list(FLOWS = spatLines, NODES = typNode))
}


ref = "ORI"
varsort = "FLOW"
oneunit = "ABBEVILLE-LA-RIVIERE"
thres = 5
pol = muniBound
tabflows = listTabflows$FIN_ACT

get_toplinks <- function(tabflows, pol, ref, varsort, oneunit, thres){
  refLib <- paste0(ref, "LIB")
  oriDes <- paste0(c("ORI", "DES"), "LIB")
  invRef <- ifelse(ref == "ORI", "DES", "ORI")
  invReflib <- oriDes[oriDes != refLib]
  tabflows <- tabflows %>% 
    mutate(FLOWDIST = FLOW * DIST) %>% 
    group_by(ORI, DES) %>% 
    summarise(FLOW = sum(FLOW, na.rm = TRUE), 
              SUMDIST = sum(FLOWDIST, na.rm = TRUE), 
              ORILIB = first(ORILIB), 
              DESLIB = first(DESLIB)) %>% 
    ungroup()
  
  tabSel <- tabflows[tabflows[[refLib]] == oneunit, ]
  
  if(nrow(tabSel) == 0){
    polSel <- NULL
    spatLines <- NULL
  } else {
    tabSel$CODGEO <- tabSel[[invRef]]
    polSel <- pol[pol$CODGEO %in% unique(c(tabSel$ORI, tabSel$DES)), ] %>% 
      left_join(tabSel[, c("CODGEO", "FLOW", "SUMDIST")], by = "CODGEO")
    
    tabSelSorted <- tabSel[order(tabSel[[varsort]], decreasing = TRUE), ]
    nbRows <- ifelse(thres > nrow(tabSel), nrow(tabSel), thres)
    tabFinal <- tabSelSorted[1:nbRows, ] %>% 
      left_join(polSel %>% st_set_geometry(NULL) %>% select(CODGEO, LON, LAT), by = c("ORI" = "CODGEO")) %>% 
      left_join(polSel %>% st_set_geometry(NULL) %>% select(CODGEO, LON, LAT), by = c("DES" = "CODGEO"))
    
    
    charLines <- paste0("LINESTRING(", tabFinal$LON.x, " ", tabFinal$LAT.x, ", ", tabFinal$LON.y, " ", tabFinal$LAT.y, ")")
    spatLines <- st_sf(1:length(charLines), geometry = st_as_sfc(charLines, crs = 4326))
    spatLines$REF <- tabFinal[, invReflib]
    spatLines$FLOW <- tabFinal$FLOW
    spatLines$SUMDIST <- tabFinal$SUMDIST
  }
  topDes <- list(POLYG = polSel, LINES = spatLines)
  return(topDes)
}
