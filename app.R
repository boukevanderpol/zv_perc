# ========================================================================
# shiny app over ziekteverzuim bij de rijksoverheid
# 
# Basis vormen de gegevens die zijn ontvangen van p-direkt. Geen andere 
# gegevens toegevoegd. 
# 
# datum: 23 september 2022
# Bouke van der Pol, boukevanderpol@gmail.com   
# ========================================================================

#setwd("./ziekteverzuim_rijksoverheid")

library(shiny)
library(tidyverse)
library(tsibble)
library(fabletools)
library(fable)
library(feasts)

source("./functies/functies_laden.R")
source("./functies/functies_grafieken.R")



ui <- fluidPage(

    titlePanel("Ziekteverzuim bij de rijksoverheid"),

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
                     overheid. Dat heb ik dan ook gedaan en mij beperkt tot 
                     de Rijksoverheid. En mijn dank aan p-direkt om het 
                     verstrekken van deze gegevens. "), 
                   p("Op het eerste volgende tabblad, is de ontwikkeling van 
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
                     maar van tal van factoren die erbuiten zijn gelaten; hecht 
                     aan deze voorspelling geen waarde aan en zie dit als 
                     spellerei van mijn kant. "),
                   p("Ik heb deze Shiny-app niet helemaal uitontwikkeld. 
                     Een aantal shortcut zijn genomen om de tijdinvestering 
                     te beperken. Doel voor mij was en de tooling weer eens 
                     te gebruiken. "),
                   hr(style = "border-top: 1px solid #000000;")
                     
                   )
                 )
               ),
      
      # tab rijk en onderdelen ------------------------------------------------
      tabPanel("Het Rijk en ministeries",
               sidebarLayout(
                 sidebarPanel(
                   p(strong("Selectiemogelijkheden")),
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
                   plotOutput("g_voorspelling")
                 )
               )
      )
      # einde tabbladen ------------------------------------------------
    ),
    
)

# server ---------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
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
    grafiek_voorspelling <- reactive(g_voorspellen(
      aantal_maanden = input$aantal_mnd_4,
      afk = input$onderdelen_4))
    output$g_voorspelling <- renderPlot({grafiek_voorspelling()}, 
                                        res = 96)

}

# Run the application 
shinyApp(ui = ui, server = server)