# ========================================================================
# shiny app over ziekteverzuim bij de rijksoverheid
# 
# Basis vormen de gegevens die zijn ontvangen van p-direkt. Geen andere 
# gegevens toegevoegd. 
# 
# datum: 24 september 2022
# Bouke van der Pol, boukevanderpol@gmail.com   
# ========================================================================


library(shiny)
library(tidyverse)
library(tsibble)
library(tsibbledata)
library(fabletools)
library(fable)
library(feasts)

source("./functies/functies_laden.R")
source("./functies/functies_grafieken.R")



ui <- fluidPage(
  
  titlePanel("Ontwikkeling van het ziekteverzuim bij de rijksoverheid"),
  
  # tabbladen
  tabsetPanel(
    # tab intro ------------------------------------------------
    tabPanel("intro",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 br(" "),
                 p("Beste lezer,"),
                 br(" "),
                 p("Hierbij ga ik je vervelen met het resultaat van mijn 
                   hernieuwde kennismaking van het visualisatie tool Shiny. "),
                 p("Als onderwerp heb ik gekozen voor het ziekteverzuim. 
                   En de vraag die ik mijzelf stelde was, hoe heeft het 
                   ziekteverzuim zich ontwikkeld in de afgelopen jaren en 
                   is deze ontwikkeling in grafiek of beeld te vatten. "),
                 p("Ziekteverzuimgegevens liggen niet op straat; het is 
                   niet gebruikelijk om te pochen met deze waarden. Sinds 
                   enige tijd is er de Wet open overheid die je de 
                   mogelijkheid geeft om gegevens op te vragen over de 
                   overheid. Dat heb ik dan ook gedaan en ik heb mij beperkt tot 
                   de Rijksoverheid. De gegevens heb ik mogen ontvangen van 
                   p-direkt en bij deze mijn dank daarvoor. Nog mooier zou zijn 
                   als deze gegevens via een API beschikbaar zou komen. In dat 
                   geval zou deze Shiny-app kunnen worden omgebouwd en de meest 
                   actuele stand laten zien. "), 
                 p("Op het eerste volgende tabblad (\"Het Rijk en ministeries\"), 
                   is de ontwikkeling van 
                   het ziekteverzuimpercentage in de tijd geplaatst. Ook is 
                   dit percentage opgedeeld in kort- en langerdurend 
                   ziekteverlof. Een percentage kan wat onpersoonlijk 
                   overkomen. Onderliggend aan dit percentage zijn mensen 
                   die zich op een zeker moment ziek hebben gemeld en mensen 
                   die een korte of langere periode periode ziek. Naar het 
                   verzuimpercentage, zie je ook het aantal mensen dat 
                   ziek is en het aantal ziekmeldingen. "),
                 p("Op de tab \"vergelijken\", kan je het ziekteverzuimpercentage 
                   van twee orgnisaties naast elkaar zetten om de verschillen 
                   zichtbaar te maken. "),
                 p("In de laatste tab \"voorspellen\", wordt met de 
                   aanwezige historische gegevens en een ARIMA-model een 
                   voorspelling gedaan van het ziekteverzuimpercentage. 
                   LET WEL: De ontwikkeling van het ziekteverzuimpercentage 
                   is natuurlijk niet afhankelijk van een historische reeks 
                   maar van tal van factoren die erbuiten zijn gelaten; zie dit 
                   als spellerei van mijn kant. "),
                 p("Deze Shiny-app is niet helemaal uitontwikkeld. 
                   Een aantal shortcuts zijn genomen om de tijdinvestering 
                   te beperken. Mijn doel in de eerste plaats om opnieuw gevoel 
                   te krijgen met de tooling en als tweede om een inzicht te 
                   bieden in dit onderwerp. "),
                 hr(style = "border-top: 1px solid #000000;")
                 
               )
             )
    ),
    
    # tab rijk en onderdelen ------------------------------------------------
    tabPanel("rijk en ministeries",
             sidebarLayout(
               sidebarPanel(
                 p(strong("Selectiemogelijkheden")),
                 hr(style = "border-top: 1px solid #000000;"),
                 radioButtons(inputId = "onderdelen_2",
                              label   = "Organisatie:", 
                              c("Het Rijk" = "Rijk",
                                "Ministerie van BZK" =  "BZK", 
                                "Ministerie van AZ" = "AZ",
                                "Ministerie van J&V" = "J&V", 
                                "Ministerie van SZW" = "SZW",
                                "Ministerie van OCW" = "OCW", 
                                "Ministerie van VWS" = "VWS", 
                                "Ministerie van EZK" = "EZK", 
                                "Ministerie van FIN" = "FIN", 
                                "Ministerie van I&W" = "I&W", 
                                "Ministerie van LNV" = "LNV", 
                                "Ministerie van BuZa" = "BuZa"),
                              selected = "Rijk"),
                 radioButtons(inputId  = "dag_week_maand_2", 
                              label    = "Periode voor de presentatie van het aantal zieken en ziekmeldingen:", 
                              selected = "week", choiceNames = list(
                                "dag", "week", "maand"), 
                              choiceValues = list("dag", "week", "maand"))
               ),
               mainPanel(br(" "),
                         fluidRow(
                           splitLayout(cellWidths = c("42%", "58%"),
                                       plotOutput("g_zv_alg"),
                                       plotOutput("g_zv_duur"))),
                         br(),
                         hr(style = "border-top: 1px solid #000000;"),
                         br(),
                         plotOutput("g_zieken"),
                         plotOutput("g_ziekmeldingen"))
             )
    ),
    # tab vergelijken ------------------------------------------------
    tabPanel("vergelijken",
             sidebarLayout(
               sidebarPanel(
                 p(strong("Selectiemogelijkheden")),
                 hr(style = "border-top: 1px solid #000000;"),
                 selectInput(inputId = "organisatie_3a", 
                             label = "1ste organisatie:", 
                             c("Het Rijk" = "Rijk",
                               "Ministerie van BZK" =  "BZK", 
                               "Ministerie van AZ" = "AZ",
                               "Ministerie van J&V" = "J&V", 
                               "Ministerie van SZW" = "SZW",
                               "Ministerie van OCW" = "OCW", 
                               "Ministerie van VWS" = "VWS", 
                               "Ministerie van EZK" = "EZK", 
                               "Ministerie van FIN" = "FIN", 
                               "Ministerie van I&W" = "I&W", 
                               "Ministerie van LNV" = "LNV", 
                               "Ministerie van BuZa" = "BuZa"),
                             selected = "Rijk"),
                 selectInput(inputId = "organisatie_3b", 
                             label = "2de organisatie:", 
                             c("Het Rijk" = "Rijk",
                               "Ministerie van BZK" =  "BZK", 
                               "Ministerie van AZ" = "AZ",
                               "Ministerie van J&V" = "J&V", 
                               "Ministerie van SZW" = "SZW",
                               "Ministerie van OCW" = "OCW", 
                               "Ministerie van VWS" = "VWS", 
                               "Ministerie van EZK" = "EZK", 
                               "Ministerie van FIN" = "FIN", 
                               "Ministerie van I&W" = "I&W", 
                               "Ministerie van LNV" = "LNV", 
                               "Ministerie van BuZa" = "BuZa"),
                             selected = "BuZa")
               ),
               mainPanel(
                 br(" "),
                 plotOutput("g_vergelijken_verzuim_alg")
               )
             )
    ),
    # tab voorspellen ------------------------------------------------
    tabPanel("voorspellen",
             sidebarLayout(
               sidebarPanel(
                 p(strong("Selectiemogelijkheden")),
                 hr(style = "border-top: 1px solid #000000;"),
                 sliderInput(inputId = "aantal_mnd_4",
                             label = "Hoeveel maanden voorspellen:",
                             min = 1,
                             max = 16,
                             value = 5),
                 radioButtons(inputId = "onderdelen_4",
                              label   = "Organisatie:", 
                              c("Het Rijk" = "Rijk",
                                "Ministerie van BZK" =  "BZK", 
                                "Ministerie van AZ" = "AZ",
                                "Ministerie van J&V" = "J&V", 
                                "Ministerie van SZW" = "SZW",
                                "Ministerie van OCW" = "OCW", 
                                "Ministerie van VWS" = "VWS", 
                                "Ministerie van EZK" = "EZK", 
                                "Ministerie van FIN" = "FIN", 
                                "Ministerie van I&W" = "I&W", 
                                "Ministerie van LNV" = "LNV", 
                                "Ministerie van BuZa" = "BuZa"),
                              selected = "Rijk")
               ),
               mainPanel(
                 br(" "),
                 p("Een voorspelling op basis van (slechts) de historische 
                   reeks met behulp van ARIMA (autoregressive integrated 
                   moving average) model:"),
                 br(" "),
                 plotOutput("g_voorspelling"),
                 br(" "),
                 p("Voor de liefhebbers en transparantie zijn de specificaties 
                 van het gebruikte model hieronder toegevoegd:"),
                 verbatimTextOutput("tekst_a")
               )
             )
    )
    # einde tabbladen ------------------------------------------------
  ),
  
)

# server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # g_zv_alg -----------------------------------------    
  grafiek_zv_alg <- reactive(g_verzuim_alg(afk = input$onderdelen_2))
  output$g_zv_alg <- renderPlot({grafiek_zv_alg()}, res = 96)
  
  # g_zv_duur ----------------------------------------    
  grafiek_zv_duur <- reactive(g_verzuim_duur(afk = input$onderdelen_2))
  output$g_zv_duur <- renderPlot({grafiek_zv_duur()}, res = 96)
  
  # g_zieken -----------------------------------------
  grafiek_zieken_onderdelen <- reactive(g_zieken(
    afk = input$onderdelen_2,
    periode = input$dag_week_maand_2))
  output$g_zieken <- renderPlot({grafiek_zieken_onderdelen()},
                                res = 96)
  # g_ziekmeldingen ----------------------------------
  grafiek_ziekmeldingen_onderdelen <- reactive(g_ziekmeldingen(
    afk = input$onderdelen_2,
    periode = input$dag_week_maand_2))
  output$g_ziekmeldingen <- renderPlot({grafiek_ziekmeldingen_onderdelen()}, 
                                       res = 96)
  # g_vergelijken ------------------------------------
  grafiek_vergelijken_verzuim_alg <- reactive(
    g_vergelijken_verzuim_alg(afk_3a = input$organisatie_3a,
                              afk_3b = input$organisatie_3b))
  output$g_vergelijken_verzuim_alg <- renderPlot(
    {grafiek_vergelijken_verzuim_alg()}, res = 96)
  # g_voorspelling -----------------------------------
  grafiek_voorspelling_a <- reactive(g_voorspellen_a(
    aantal_maanden = input$aantal_mnd_4,
    afk = input$onderdelen_4))
  output$g_voorspelling <- renderPlot({grafiek_voorspelling_a()}, 
                                      res = 96)
  # tekst_a ------------------------------------------
  grafiek_voorspelling_b <- reactive(g_voorspellen_b(
    aantal_maanden = input$aantal_mnd_4,
    afk = input$onderdelen_4))
  output$tekst_a <- renderPrint({grafiek_voorspelling_b()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

