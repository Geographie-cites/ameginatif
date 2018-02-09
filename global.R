##############################
# Shiny App: Postcar Explicatif
# Global
##############################


# load packages ----

library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(gridExtra)
library(sp)
library(raster)
library(cartography)
library(SpatialPosition)
library(reshape2)
library(sf)
library(dplyr)

# load data ----

listCommuteAggregates <- readRDS("data/listaggregates.Rds")
listOtherAggregates <- readRDS("data/listotherpurpose.Rds")
listPotentials <- readRDS("data/listpotentials.Rds")

# vecMin <- sapply(listPotentials, function(x) cellStats(x = x, stat = min))
# minMin <- min(vecMin[substr(names(vecMin), 1, 3) == "dif"])
# vecMax <- sapply(listPotentials, function(x) cellStats(x = x, stat = max))
# maxMax <- max(vecMax[substr(names(vecMax), 1, 3) == "dif"])


# compute and plot aggregates ----

PlotAggrDist <- function(comconf, otherconf, comref, otherref){
  
  # time and distance aggregates
  tabSum <- tibble(VAR = comconf$VAR[1:6], 
                   CONF = comconf$VALUE[1:6] + otherconf,
                   REF = comref$VALUE[1:6] + otherref)
  tabSum$DIF <- tabSum$CONF - tabSum$REF
  tabLong <- melt(data = tabSum, id.vars = "VAR", measure.vars = c("CONF", "REF"), variable.name = "CONFIG", value.name = "VALUE")
  tabLong$CONFIG <- plyr::mapvalues(x = tabLong$CONFIG, from = c("CONF", "REF"), to = c("Scénario", "Actuel"))
  tabLong$VARTYPE <- substr(tabLong$VAR, 1, 3)
  
  # stocks
  stockDist <- ggplot(tabLong[tabLong$VARTYPE == "DIS", ]) +
    geom_bar(aes(x = VAR, y = VALUE / 1000000, fill = CONFIG), stat= "identity", position = "dodge") +
    scale_fill_manual(values = c("firebrick", "grey80")) +
    scale_x_discrete(labels = c("Modes doux", "Trans.collectif", "Véh.particulier")) +
    scale_y_continuous("Portée totale (millions de km)", breaks = seq(0, 300, 10)) +
    coord_flip() + theme_darklinehc
  
  stockTps <- ggplot(tabLong[tabLong$VARTYPE == "TPS", ]) +
    geom_bar(aes(x = VAR, y = VALUE / 1000000, fill = CONFIG), stat= "identity", position = "dodge") +
    scale_fill_manual(values = c("firebrick", "grey80")) +
    scale_x_discrete(labels = c("Modes doux", "Trans.collectif", "Véh.particulier")) +
    scale_y_continuous("Temps de transport cumulé (millions d'heures)", breaks = seq(0, 30, 1)) +
    coord_flip() + theme_darklinehc
  
  # ratio per individual
  tabLong$VALUE <- tabLong$VALUE / 11415112
  ratioDist <- ggplot(tabLong[tabLong$VARTYPE == "DIS", ]) +
    geom_bar(aes(x = VAR, y = VALUE, fill = CONFIG), stat= "identity", position = "dodge") +
    scale_fill_manual(values = c("firebrick", "grey80")) +
    scale_x_discrete(labels = c("Modes doux", "Trans.collectif", "Véh.particulier")) +
    scale_y_continuous("Portée par individu (km)", breaks = seq(0, 50, 1)) +
    coord_flip() + theme_darklinehc
  
  ratioTps <- ggplot(tabLong[tabLong$VARTYPE == "TPS", ]) +
    geom_bar(aes(x = VAR, y = 60 * VALUE, fill = CONFIG), stat= "identity", position = "dodge") +
    scale_fill_manual(values = c("firebrick", "grey80")) +
    scale_x_discrete(labels = c("Modes doux", "Trans.collectif", "Véh.particulier")) +
    scale_y_continuous("Temps de transport par individu (mn)", breaks = seq(0, 150, 5)) +
    coord_flip() + theme_darklinehc
  
  # intra share
  tabIntra <- tibble(VAR = c("INTRACOMSCE", "INTRACOMREF", "INTRADEPSCE", "INTRADEPREF"),
                     VALUE = c(sum(comconf$VALUE[7:9]) / sum(comconf$VALUE[1:3]),
                               sum(comref$VALUE[7:9]) / sum(comref$VALUE[1:3]),
                               sum(comconf$VALUE[13:15]) / sum(comconf$VALUE[1:3]),
                               sum(comref$VALUE[13:15]) / sum(comref$VALUE[1:3])),
                     TYPE = c("Intracommunal", "Intracommunal", "Intradépartemental", "Intradépartemental"),
                     CONF = factor(x = c(1, 2, 1, 2),
                                   levels = c(1, 2),
                                   labels = c("Scénario", "Actuel")))
  
  intraCom <- ggplot(tabIntra[1:2, ]) +
    geom_bar(aes(x = CONF, y = 100 * VALUE, fill = CONF), stat = "identity") +
    scale_fill_manual(values = c("firebrick", "grey80")) +
    scale_x_discrete() +
    scale_y_continuous("Proportion de la distance intracommunale (%)") +
    coord_flip() + theme_darklinehc
  
  intraDep <- ggplot(tabIntra[3:4, ]) +
    geom_bar(aes(x = CONF, y = 100 * VALUE, fill = CONF), stat = "identity") +
    scale_fill_manual(values = c("firebrick", "grey80")) +
    scale_x_discrete() +
    scale_y_continuous("Proportion de la distance intradépartementale (%)") +
    coord_flip() + theme_darklinehc
  
  sixPlots <- list(STOCKDIST = stockDist, STOCKTPS = stockTps, 
                   RATIODIST = ratioDist, RATIOTPS = ratioTps, 
                   INTRACOM = intraCom, INTRADEP = intraDep)
  return(sixPlots)
}



PotentialPalette <- function(ras) {
  valRas <- c(as.matrix(ras))
  valRasMin <- min(valRas, na.rm = TRUE)
  valRasMax <- max(valRas, na.rm = TRUE)
  valRange <- c(valRasMin, valRasMax)
  if(valRasMin >= 0 & valRasMax > 0){
    palCol <- colorRampPalette(c("grey90", "firebrick"))(100)
  } else if (valRasMax - valRasMin < 40) {
    palCol <- "grey90"
  } else {
    seqVal <- seq(valRasMin, valRasMax, 20)
    getZero <- findInterval(0, seqVal)
    palBlue <- colorRampPalette(c("navyblue", "grey90"))(getZero)
    palRed <- colorRampPalette(c("grey90", "firebrick"))(length(seqVal)-getZero)
    palCol <- c(palBlue, palRed)
  }
  return(palCol)
}


# Draw contour polygons for potentials ----

PotentialContour <- function(ras) {
  potCont <- rasterToContourPoly(r = ras, nclass = 15)
  potContGeo <- st_as_sf(spTransform(potCont, CRSobj = CRS("+init=epsg:4326")))
  return(potContGeo)
}


# ggplot dark theme ----

# without lines
theme_darkhc <- theme_bw() +
  theme(plot.background = element_rect(fill = "#272B30"),
        axis.line = element_line(color = "grey80"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#272B30"),
        axis.title = element_text(family = "sans-serif", color = "grey80"),
        axis.text = element_text(family = "sans-serif", color = "grey80"),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = "#272B30"))

# with lines
theme_darklinehc <- theme_bw() +
  theme(plot.background = element_rect(fill = "#272B30"),
        axis.line = element_line(color = "grey80"),
        panel.grid.major = element_line(color = "grey80", size = 0.1),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#272B30"),
        axis.title.x =  element_text(family = "sans-serif", size = 14, color = "grey80"),
        axis.text.x = element_text(family = "sans-serif", size = 12, color = "grey80"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "sans-serif", size = 14, color = "grey80"),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key =  element_blank(),
        legend.text = element_text(family = "sans-serif", color = "grey80"),
        legend.background = element_rect(fill = "#272B30"))
