##############################
# Shiny App: Postcar Explicatif
# User interface
##############################


shinyUI(fluidPage(
  theme = "darkGrey.css",
  titlePanel("Améginat-IF : aménager en imaginant l'Île-de-France",
             tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                       tags$title("Ameginat-IF"))),
  
  tabsetPanel(
    
    # Guide ----
    
    tabPanel("Guide d'utilisation", 
             fluidRow(
               column(2),
               column(7, includeMarkdown("README.md")))),
    
    # Réseau ----
    
    tabPanel("Scénario",
             fluidRow(
               column(2),
               column(7,
                      hr(),
                      p(icon("compress"), tags$strong("Agir sur les mobilités résidentielle et professionnelle")),
                      radioButtons(inputId = "excess", 
                                   label = "", 
                                   choices = c("Configuration actuelle" = "ACT", 
                                               "Echange sans contrainte" = "GLO",
                                               "Bourse aux emplois selon CSP" = "CS1", 
                                               "Bourse aux emplois selon NAE" = "NA5", 
                                               "Bourse aux emplois selon CSP & NAE" = "CSNA",
                                               "Bourse aux logements selon type de logement" = "TYPL", 
                                               "Bourse aux logements selon type de ménage" = "TYPMR", 
                                               "Bourse aux logements selon type de logement & ménage" = "LGMR"),
                                   selected = "ACT",
                                   inline = FALSE, width = "100%"),
                      hr(),
                      p(icon("delicious"), tags$strong("Relocaliser les populations et les activités")),
                      radioButtons(inputId = "reloc", 
                                   label = "", 
                                   choices = c("Configuration actuelle" = "ACT", 
                                               "Finger plan" = "FIN", 
                                               "Transport-oriented development" = "TOD", 
                                               "Polycentrisation" = "POL", 
                                               "CBDsation" = "CBD"),
                                   selected = "ACT",
                                   inline = FALSE, width = "100%"),
                      hr(),
                      p(icon("car"), tags$strong("Agir sur le mode de transport")),
                      radioButtons(inputId = "modetrans", 
                                   label = "", 
                                   choices = c("Configuration actuelle" = "ACT", 
                                               "Zéro voiture" = "ZVP", 
                                               "Tout voiture" = "TVP",
                                               "Zéro transport collectf" = "ZTC",
                                               "Tout transport collectf" = "TTC",
                                               "Zéro modes doux" = "ZNM",
                                               "Tout modes doux" = "TNM"),
                                   selected = "ACT",
                                   inline = FALSE, width = "100%"),
                      hr(),
                      p(icon("building-o"), tags$strong("Relocaliser les équipements")),
                      radioButtons(inputId = "equip", 
                                   label = "", 
                                   choices = c("Configuration actuelle" = "ACT", 
                                               "Près des résidents" = "ORI",
                                               "Près des emplois" = "DES", 
                                               "Equilibre résidents-emplois" = "EQU"),
                                   selected = "ACT",
                                   inline = FALSE, width = "100%"),
                      p(actionButton("computescenario", label = "Générer le scénario", icon("play-circle"))),
                      hr())
             )),
    
    tabPanel("Résultats en chiffres",
             fluidRow(
               column(12,
                      tags$br(),
                      tags$h4("Distance et temps cumulés (jour ouvrable type)"),
                      plotOutput("stock", height = "500px"),
                      tags$br(),
                      tags$h4("Distance et temps moyens (jour ouvrable type)"),
                      plotOutput("ratio", height = "500px"),
                      tags$br(),
                      tags$h4("Distance intracommunale et intradépartementale (jour ouvrable type)"),
                      plotOutput("intra", height = "400px"))
             )
    ),
    
    
    tabPanel("Résultats en cartes",
             fluidRow(
               column(3, wellPanel(
                 radioButtons("pottyp", 
                              label = "Type de potentiel", 
                              choices = c("Actifs selon scénario" = "ori", 
                                          "Emplois selon scénario" = "des", 
                                          "Différentiel actifs-emplois" = "dif",
                                          "Actifs : scénario vs. actuel" = "oricomp",
                                          "Emplois : scénario vs. actuel" = "descomp"), 
                              selected = "ori")
               )),
               column(9, leafletOutput("mappot", height = "800px"))
             )
    )
  )
)
)