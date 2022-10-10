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
library(shinydashboard)
library(tidyverse)
library(tsibble)
library(tsibbledata)
library(fabletools)
library(fable)
library(feasts)
library(urca)

source("./functies/functies_laden.R")
source("./functies/functies_grafieken.R")
source("./functies/functies_box.R")


header <- dashboardHeader(title = "Ziekteverzuim")

sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "intro", icon = icon("th", verify_fa = FALSE)),
    menuItem("Rijk & ministeries", tabName = "rijk_ministeries", icon = icon("th", verify_fa = FALSE)),
    menuItem("Vergelijken", tabName = "vergelijken", icon = icon("th", verify_fa = FALSE)),
    menuItem("Voorspellen", tabName = "voorspellen", icon = icon("th", verify_fa = FALSE))
  )
)

body <- dashboardBody(
  tabItems(
    # intro ---------------------------
    tabItem(tabName = "intro",
            h2("Intro"),
            br(" "),
            hr(style = "border-top: 1px solid #000000;"),
            p("Beste lezer,"),
            br(" "),
            p("Hierbij ga ik je vervelen met het resultaat van mijn 
               hernieuwde kennismaking van het visualisatie tool Shiny. "),
            p("Als onderwerp heb ik gekozen voor ziekteverzuim. 
               En de vraag die ik mijzelf stelde was, hoe heeft het 
               ziekteverzuim zich ontwikkeld in de afgelopen jaren en 
               is deze ontwikkeling in beeld te vatten. "),
            p("Ziekteverzuimgegevens liggen niet op straat; het is 
               niet gebruikelijk om te pochen met deze waarden. Toch is het 
               gelukt om via P-Direkt aan gegevens te komen die betrekking 
               hebben op ziekteverzuim en een groot deel van de 
               Rijksoverheid tot en met eind augustus 2022. Waarvoor mijn dank. 
               Nog mooier zou zijn als deze gegevens via een API beschikbaar 
               zou zijn. In dat geval zou deze Shiny-app kunnen worden 
               omgebouwd en de meest actuele stand laten zien zoals 
               vrijgegeven in de API. Aan de andere kant, moet je 
               blij zijn met hetgeen je wordt gegund. "), 
            p("Op het eerste volgende tabblad (\"Rijk en ministeries\"), 
               is de ontwikkeling van 
               het ziekteverzuimpercentage in de tijd geplaatst. Ook is 
               dit percentage opgedeeld in kort- en langerdurend 
               ziekteverlof. Een percentage kan wat onpersoonlijk 
               overkomen. Onderliggend aan dat percentage zijn mensen 
               die zich op een zeker moment ziek hebben gemeld en ziek zijn. 
               Naast het verzuimpercentage, zijn ook overzichten opgenomen 
               die het aantal zieken en ziekmeldingen in de tijd laten 
               zien. "),
            p("Op het tabblad \"Vergelijken\", kan je de ziekteverzuimpercentages 
               van twee organisaties naast elkaar zetten om de verschillen 
               zichtbaar te maken. "),
            p("In het laatste tabblad \"Voorspellen\", wordt met de 
               aanwezige historische gegevens en een ARIMA-model een 
               voorspelling gedaan van het ziekteverzuimpercentage. "),
            p("Deze Shiny-app is niet helemaal uitontwikkeld. 
               Een aantal shortcuts zijn genomen om de tijdinvestering 
               te beperken. Mijn doel was in de eerste plaats om opnieuw gevoel 
               te krijgen met de tooling. "),
            hr(style = "border-top: 1px solid #000000;")
            ),
    # rijk ministeries -------------------
    tabItem(tabName = "rijk_ministeries",
            h2("Rijk & ministeries - t/m augustus 2022"),
            fluidRow(
              valueBoxOutput(
                outputId = "valuebox_zv_percentage",
                width = 3),
              valueBoxOutput(
                outputId = "valuebox_aantal_zieken",
                width = 4),
              valueBoxOutput(
                outputId = "valuebox_aantal_ziekmeldingen",
                width = 4),
            ),
            box(
              title = "Inputs", #status = "warning",
              background = "black",
              width = 3,
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
                           label    = "Periode (zieken / ziekmeldingen):", # voor de presentatie van het aantal zieken en ziekmeldingen:", 
                           selected = "week", choiceNames = list(
                             "dag", "week", "maand"), 
                           choiceValues = list("dag", "week", "maand"))
            ),
            fluidRow(
              tabBox(
                title = "",
                width = 8,
                tabPanel(
                  title = "ziekteverzuimpercentage", 
                  splitLayout(cellWidths = c("42%", "57%"),
                              plotOutput("g_zv_alg"),
                              plotOutput("g_zv_duur"))
                ),
                tabPanel(
                  title = "aantal zieken",
                  plotOutput("g_zieken")
                ),
                tabPanel(
                  title = "aantal ziekmeldingen",
                  plotOutput("g_ziekmeldingen")
                ),
              )
            ),
    ),
    # vergelijken ------------------------
    tabItem(tabName = "vergelijken",
            h2("Vergelijken - t/m augustus 2022"),
            box(
              title = "Inputs", #status = "warning",
              background = "black",
              width = 3,
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
                          selected = "BuZa")),
              box(
                title = "Vergelijking ziekteverzuimpercentages",
                #background = "maroon",
                solidHeader = TRUE,
                width = 8,
                status = "primary",
                plotOutput("g_vergelijken_verzuim_alg")
              )
            ),
    # voorspellen ------------------------
    tabItem(tabName = "voorspellen",
            h2("Voorspellen"),
            box(
              title = "Inputs",
              background = "black",
              width = 3,
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
            box(
              title = "Voorspelling obv ARIMA model", status = "primary",
              solidHeader = TRUE,
              width = 8,
              br(" "),
              plotOutput("g_voorspelling"),
              br(" "),
              p("Voor de liefhebbers en transparantie zijn de specificaties 
                 van het gebruikte model hieronder toegevoegd:"),
              verbatimTextOutput("tekst_a")
            ))
  )
)

ui <- dashboardPage(header, sidebar, body)


# server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # valuebox_zv_percentage ---------------------------
  valuebox_zv_perc <- reactive(vb_zv_perc(afk = input$onderdelen_2))
  valuebox_zv_kleur <- reactive(vb_zv_kleur(afk = input$onderdelen_2))
  valuebox_zv_icoon <- reactive(vb_zv_icoon(afk = input$onderdelen_2))
  output$valuebox_zv_percentage <- renderValueBox({
    valueBox(
      value = paste0(valuebox_zv_perc(),"%"),
      subtitle = "huidig zv%",
      color = valuebox_zv_kleur(),
      icon = icon(valuebox_zv_icoon())
    )
  })

  # valuebox_aantal_zieken ---------------------------
  valuebox_aantal_z <- reactive(vb_aantal_zieken(afk = input$onderdelen_2))
  output$valuebox_aantal_zieken <- renderValueBox({
    valueBox(
      value = valuebox_aantal_z(),
      subtitle = "huidig aantal zieken",
      color = "yellow",
      icon = icon("user")
    )
  })

  # valuebox_aantal_ziekmeldingen ---------------------------
  valuebox_aantal_zm <- reactive(vb_aantal_ziekmeldingen(afk = input$onderdelen_2))
  output$valuebox_aantal_ziekmeldingen <- renderValueBox({
    valueBox(
      value = valuebox_aantal_zm(),
      subtitle = "aantal ziekmeldingen laatste week",
      color = "blue",
      icon = icon("rectangle-list")
    )
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

