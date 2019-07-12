###########################################
##### Ameginatif - ui               #######
###########################################

shinyUI(bootstrapPage(
  theme = shinytheme("superhero"),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  tags$style(HTML("
                  body {
                  font-family:'Helvetica';
                  }
                  #loading {
                  position: relative;
                  z-index : 1;
                  }
                  #Scénarii{
                  margin: auto.;
                  }
                  #mapIndic {
                  position: absolute;
                  }
                  #mapflu {
                  position: absolute;
                  }
                  #mappot {
                  position: absolute;
                  }
                  #mapfluDom {
                  position: absolute;
                  }
                  .panel-title {
                  text-align: center;
                  }
                  #tabPanel {
                  max-height: 90%;
                  overflow: auto;
                  }
                  .panel-group {
                  margin-bottom: 0px;
                  }
                  .leaflet .legend i{
                  border-radius: 50%;
                  width: 10px;
                  height: 10px;
                  margin-bottom: 10px;
                  }
                  #tabPanel{
                  font-weight: bold;
                  }
                  .panel-title{
                  font-weight: bold;
                  }
                  .nav-tabs > li:hover {
                  text-decoration: underline;
                  }
                  #Filtre{
                  text-align: center;
                  }
                  .panel{
                  box-shadow: 2px 2px 4px rgba(0,0,0,0.5);
                  }
                  #collapseExample{
                  box-shadow: 0px 0px 0px rgba(0,0,0,0);
                  }
                  #loading-content {
                  position: absolute;
                  background: #999999;
                  opacity: 0;
                  z-index: 100;
                  left: 0;
                  right: 0;
                  height: 100%;
                  }
                  ")),
  
  
  # Loading wheel
  absolutePanel(top = "50%", 
                right = "50%",
                class = "panel panel-default",
                shinyjs::hidden(div(id = 'loading', addSpinner(div(), spin = "cube-grid", color = "#B35605")))
  ),
  
  # Loading invisible panel
  div(
    id = "loading-content"
  ),
  
  #Display maps through the panel selection
  absolutePanel(bottom = "0%",
                top = "0%",
                right = "0%",
                left = "0%",
                class = "panel panel-default",
                style = "padding : 10px;
                  text-align: center",
                leafletOutput("mapindic", width="100%", height = "100%")),
  absolutePanel(bottom = "2%",
                right = "25%",
                left = "25%",
                class = "panel panel-default",
                style = "padding : 10px;
                  text-align: center",
                radioButtons(inputId = "filterpop",
                             label = NULL,
                             choices = list("Tout" = "TOU",
                                            "Agriculteur" = "AGR",
                                            "Artisan-commerçant" = "ART",
                                            "Prof. supérieure" = "CAD",
                                            "Prof. intermédiaire" = "INT",
                                            "Employé" = "EMP",
                                            "Ouvrier" = "OUV",
                                            "Automobiliste" = "VP",
                                            "Transporté collectivement" = "TC",
                                            "Travaille à domicile" = "IM",
                                            "Usagers de mode doux" = "NM"),
                             selected = "TOU",
                             inline = T)
  ),
  
  #Scenarios panel
  absolutePanel(id ="Scenarii",
                top = "2%", 
                right = "25%",
                left = "25%",
                class = "panel panel-default",
                style = "padding : 10px",
                bsCollapse(id = "collapseExample", open = "Panel 2",
                           bsCollapsePanel("Scénarios",
                                           radioButtons(inputId = "reloc",
                                                        label = "Relocaliser les populations et les activités",
                                                        choices = c("Configuration actuelle" = "ACT",
                                                                    "Finger plan" = "FIN",
                                                                    "Transport-oriented development" = "TOD",
                                                                    "Polycentrisation" = "POL",
                                                                    "CBDsation" = "CBD"),
                                                        selected = "ACT",
                                                        inline = T, width = "100%"),
                                           radioButtons(inputId = "excess",
                                                        label = "Agir sur les mobilités résidentielle et professionnelle",
                                                        choices = c("Configuration actuelle" = "ACT",
                                                                    "Échange d'emploi" = "EMP",
                                                                    "Échange de logement" = "RES"),
                                                        selected = "ACT",
                                                        inline = T, width = "100%"),
                                           radioButtons(inputId = "modetrans",
                                                        label = "Agir sur le mode de transport",
                                                        choices = c("Configuration actuelle" = "ACT",
                                                                    "Zéro voiture" = "ZVP",
                                                                    "Tout voiture" = "TVP",
                                                                    "Zéro transport collectf" = "ZTC",
                                                                    "Tout transport collectf" = "TTC",
                                                                    "Zéro modes doux" = "ZNM",
                                                                    "Tout modes doux" = "TNM"),
                                                        selected = "ACT",
                                                        inline = T, width = "100%")
                                           
                           )
                )
  ),
  #indicators panel
  absolutePanel( id = "tabPanel",
                 class = "panel panel-default",
                 style = "padding : 10px",
                 top = "2%", 
                 left = "2%",
                 right = "78%",
                 tabsetPanel(id = "tabs", 
                             
                             ####### Panneau Indice ##### 
                             tabPanel("Indices", 
                                      radioButtons("selindex", 
                                                   label = NULL,
                                                   choices = list("Population active" = "TOTORI",
                                                                  "Emploi" = "TOTDES",
                                                                  "Solde absolu" = "ABSBAL",
                                                                  "Solde relatif" = "RELBAL",
                                                                  "Autocontention" = "AUTOCONT",
                                                                  "Autosuffisance" = "AUTOSUFF",
                                                                  "Distance moyenne à l'origine" = "AVGDISTORI",
                                                                  "Distance moyenne à destination" = "AVGDISTDES",
                                                                  "Distance totale à l'origine" = "SUMDISTORI",
                                                                  "Distance totale à destination" = "SUMDISTDES"),
                                                   selected = "TOTORI"),
                                      actionButton("index_descr", "Description")
                             ),
                             ####### Panneau Micro Flux     #####
                             tabPanel("Flux",
                                      selectInput("flucom", 
                                                  label = "Choisir une commune",
                                                  choices = c("PARIS1", "PARIS2"),
                                                  selected = ""),
                                      radioButtons("fluref", label = "Origine ou destination", choices = c("Origine" = "ORI", "Destination" = "DES"), selected = "ORI"),
                                      radioButtons("fluvar", label = "Quantité", choices = c("Nombre d'individus" = "FLOW", "Cumul de distance" = "DISTTOT"), selected = "FLOW"),
                                      sliderInput("fluthr", label = "Top", min = 2, max = 100, step = 1, value = 3),
                                      actionButton("flux_descr", "Description")
                             ),
                             ####### Panneau FluxDom  #####
                             tabPanel("Structure",
                                      radioButtons("radioFlu", label = NULL,
                                                   choices = list("Emploi" = "iEmploi",
                                                                  "Population" = "iPopulation",
                                                                  "Population + Emploi" = "iEmpPop"
                                                   )),
                                      actionButton("fludom_descr", "Description")
                             )
                 )
  )
))