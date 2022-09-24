# ===============================================================
# Project ziekteverzuim rijksoverheid
# 
# functies waaarmee grafieken worden gemaakt
# 
# 
# 
# ===============================================================





g_verzuim_alg <- function(afk = "AZ") {
  
  # gegevens samenstellen
  geg_tabel <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2019", "2020", "2021")) %>%
    select(jaar, maand, maand_nr, aantal) 
  geg_tabel_lj <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2022")) %>%
    select(jaar, maand, maand_nr, aantal) 
  max_verzuim <- max(verzuim$aantal)
  waarde_aug <-geg_tabel_lj %>% filter(maand_nr == max(maand_nr)) %>% select(aantal)
  waarde_aug <- round(waarde_aug$aantal, 1)
  
  # grafiek samenstellen
  grafiek <- ggplot() +
    geom_line(data = geg_tabel, 
              aes(x = maand, 
                  y = aantal, 
                  colour = jaar, 
                  group = jaar),
              size = rel(1.2)) +
    geom_line(data = geg_tabel_lj, 
              aes(x = maand, 
                  y = aantal, 
                  colour = jaar, 
                  group = jaar),
              size = rel(1.5)) +
    scale_colour_manual(values = c("gray85", "gray50",
                                   "gray10", "red")) +
    geom_hline(yintercept = gem_verzuim_rijk,
               linetype="dashed", colour = "black") +
    annotate(geom = "text", label = paste0("gem. zv% 2019-2021 ", gem_verzuim_rijk, "%"),
             x = 1, y = 5.65, hjust = 0, size = 2.8, colour = "black") +
    annotate(geom = "text", 
             label = paste0("zv% aug 2022: ", waarde_aug, "%"),
             x = 1, y = 0, hjust = 0, vjust = 0, size = rel(3.5), 
             colour = "red", fontface = "bold") +
    ylim(0, max_verzuim * 1.2) + 
    labs(x = NULL,
         y = "percentage ziekteverzuim",
         title = "Ontwikkeling ziekteverzuimpercentage") +
    theme_minimal() + 
    theme(axis.title.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          title = element_text(size = 9),
          axis.text.x = element_text(size = rel(1), angle = 90)) + 
    theme(legend.position = "none",
          legend.text = element_text(size = 9)) + 
    theme(strip.text.y = element_text(size = 8)) 
  
  return(grafiek)
  
}


g_verzuim_duur <- function(afk = "AZ") {
  
  # gegevens samenstellen
  geg_tabel <- verzuim_duurklassen %>%
    filter(onderwerp %in% c("zv% 1-7",
                            "zv% 8-42",
                            "zv% 43-365",
                            "zv%> 1 jaar"),
           org_afk == afk,
           jaar %in% c("2019", "2020", "2021")) %>%
    mutate(duur = factor(str_remove_all(string = onderwerp, 
                                        pattern = "zv%"),
                         levels = c(" 1-7", " 8-42", " 43-365", "> 1 jaar"),
                         ordered = TRUE)) %>% 
    select(jaar, maand, maand_nr, duur, aantal)
  geg_tabel_lj <- verzuim_duurklassen %>%
    filter(onderwerp %in% c("zv% 1-7",
                            "zv% 8-42",
                            "zv% 43-365",
                            "zv%> 1 jaar"),
           org_afk == afk,
           jaar %in% c("2022")) %>%
    mutate(duur = factor(str_remove_all(string = onderwerp, 
                                        pattern = "zv%"),
                         levels = c(" 1-7", " 8-42", " 43-365", "> 1 jaar"),
                         ordered = TRUE)) %>% 
    select(jaar, maand, maand_nr, duur, aantal)
  
  # grafiek samenstellen
  grafiek <- ggplot() +
    geom_line(data = geg_tabel, 
              aes(x = maand, 
                  y = aantal, 
                  colour = jaar, 
                  group = jaar),
              size = rel(1.2)) +
    geom_line(data = geg_tabel_lj, 
              aes(x = maand, 
                  y = aantal, 
                  colour = jaar, 
                  group = jaar),
              size = rel(1.5)) +
    scale_colour_manual(values = c("gray85", "gray50", 
                                   "gray20", "red")) +
    ylim(0, max(geg_tabel$aantal) * 1.05) + 
    facet_grid(rows = vars(duur)) + 
    labs(x = NULL,
         y = "percentage ziekteverzuim",
         title = "Verdeling ziekteverzuimperc. in duur/dagen") +
    theme_minimal() + 
    theme(axis.title.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          title = element_text(size = 9),
          axis.text.x = element_text(size = rel(1), angle = 90),
          axis.text.y = element_text(size = rel(0.9), angle = 0)) + 
    theme(legend.position = "right",
          legend.text = element_text(size = 9)) +
    theme(strip.background = element_rect(fill = "gray90", 
                                          colour = "white",
                                          size = 1),
          strip.text.y = element_text(size = 8))  
  
  return(grafiek)
  
}



g_ziekmeldingen <- function(afk = "BZK",
                            periode = "dag") {

  
  # gegevens samenstellen
  if (periode == "week") {
    geg_tabel <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_wknr %in% c("2019","2020", "2021")) %>%
      group_by(jaar_wknr, week_nr) %>%
      summarise(aantal = sum(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_wknr,
             periode = week_nr) %>%
      select(jaar, periode, aantal)
    geg_tabel_lj <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_wknr %in% c("2022"),
             week_nr < 35) %>%           # LELIJK !!!!!
      group_by(jaar_wknr, week_nr) %>%
      summarise(aantal = sum(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_wknr,
             periode = week_nr) %>%
      select(jaar, periode, aantal)
    gemiddelde <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_wknr != "2022") %>%
      group_by(jaar_wknr, week_nr) %>%
      summarise(aantal = sum(aantal), .groups = "drop")
    gemiddelde <- mean(gemiddelde$aantal)
    aantal_hoog <- bind_rows(geg_tabel, geg_tabel_lj) %>%
      select(aantal)
    aantal_hoog <- max(aantal_hoog$aantal)
    waarde_aug <-geg_tabel_lj %>% filter(periode == max(periode)) %>% select(aantal)
    waarde_aug <- round(waarde_aug$aantal, 0)
    
  } else if (periode == "maand") {
    geg_tabel <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_mndnr %in% c("2019","2020", "2021")) %>%
      group_by(jaar_mndnr, maand_nr) %>%
      summarise(aantal = sum(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_mndnr,
             periode = maand_nr) %>%
      select(jaar, periode, aantal)
    geg_tabel_lj <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_mndnr %in% c("2022")) %>%
      group_by(jaar_mndnr, maand_nr) %>%
      summarise(aantal = sum(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_mndnr,
             periode = maand_nr) %>%
      select(jaar, periode, aantal)
    gemiddelde <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_mndnr != "2022") %>%
      group_by(jaar_mndnr, maand_nr) %>%
      summarise(aantal = sum(aantal), .groups = "drop")
    gemiddelde <- mean(gemiddelde$aantal)
    aantal_hoog <- bind_rows(geg_tabel, geg_tabel_lj) %>%
      select(aantal)
    aantal_hoog <- max(aantal_hoog$aantal)
    waarde_aug <-geg_tabel_lj %>% filter(periode == max(periode)) %>% select(aantal)
    waarde_aug <- round(waarde_aug$aantal, 0)
    
  } else if (periode == "dag") { 
    geg_tabel <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_mndnr %in% c("2022")) %>%
      mutate(jaar = jaar_mndnr,
             periode = datum) %>%
      select(jaar, periode, aantal)
    gemiddelde <- ziekmeldingen_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal ziekmeldingen",
             jaar_mndnr != "2022")
    gemiddelde <- mean(gemiddelde$aantal)
    aantal_hoog <- max(geg_tabel$aantal)
    
  }
  
  # grafiek samenstellen
  if (periode == "dag") {
    grafiek <- ggplot() +
      geom_line(data = geg_tabel,
                aes(x = periode,
                    y = aantal,
                    colour = jaar,
                    group = jaar),
                size = rel(1.2)) +
      geom_hline(yintercept = gemiddelde,
                 linetype="dashed", colour = "black") +
      #annotate(geom = "text", 
      #         label = "stippellijn = gem. aantal ziekmeldingen in 2018-2021",
      #         x = as.Date("2022-01-01"), y = max(geg_tabel$aantal), 
      #         hjust = 0, vjust = 0, size = rel(3), colour = "black") +
      ylim(0, max(aantal_hoog)*1.01) + 
      labs(x = NULL,
           y = "aantal ziekmeldingen",
           title = "Ontwikkeling aantal ziekmeldingen in 2022",
           subtitle = "(stippellijn = gem. aantal ziekmeldingen in 2019-2021)", 
           colour = NULL) +
      theme_minimal() + 
      theme(axis.title.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            title = element_text(size = 9)) +
      theme(legend.position = "none") +
      scale_colour_manual(values = c("red"))
    
  } else { # --> periode is week of maand
    grafiek <- ggplot() +
      geom_line(data = geg_tabel,
                aes(x = periode,
                    y = aantal,
                    colour = jaar,
                    group = jaar),
                size = rel(1.2)) +
      geom_line(data = geg_tabel_lj,
                aes(x = periode,
                    y = aantal,
                    colour = jaar,
                    group = jaar),
                size = rel(1.5)) +
      geom_hline(yintercept = gemiddelde,
                 linetype="dashed", colour = "black") +
      annotate(geom = "text", 
               label = paste0("aantal aug 2022: ", waarde_aug, " ziekmeldingen"),
               x = 1, y = 0, hjust = 0, vjust = 0, size = rel(3.5), 
               colour = "red", fontface = "bold") +
      #annotate(geom = "text", 
      #         label = paste0("zv% aug 2022: ", waarde_aug, "%"),
      #         x = 1, y = 0, hjust = 0, vjust = 0, size = rel(3.5), 
      #         colour = "red", fontface = "bold") +
      ylim(0, max(aantal_hoog)*1.01) + 
      labs(x = NULL,
           y = "aantal ziekmeldingen",
           title = "Ontwikkeling aantal ziekmeldingen", 
           subtitle = "(stippellijn = gem. aantal ziekmeldingen in 2019-2021)", 
           colour = NULL) +
      theme_minimal() + 
      theme(axis.title.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            title = element_text(size = 9)) +
      theme(legend.position = "bottom") +
      scale_colour_manual(values = c("gray85", "gray50",
                                     "gray10", "red"))
  }
  return(grafiek)
}


g_zieken <- function(afk = "Rijk",
                     periode = "dag") {
  
  
  # gegevens samenstellen
  if (periode == "week") {
    geg_tabel <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_wknr %in% c("2019","2020", "2021")) %>%
      group_by(jaar_wknr, week_nr) %>%
      summarise(aantal = mean(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_wknr,
             periode = week_nr) %>%
      select(jaar, periode, aantal)
    geg_tabel_lj <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_wknr %in% c("2022"),
             week_nr < 35) %>%           # LELIJK !!!!!
      group_by(jaar_wknr, week_nr) %>%
      summarise(aantal = mean(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_wknr,
             periode = week_nr) %>%
      select(jaar, periode, aantal)
    gemiddelde <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_wknr != "2022") 
    gemiddelde <- mean(gemiddelde$aantal)
    aantal_hoog <- bind_rows(geg_tabel, geg_tabel_lj) %>%
      select(aantal)
    aantal_hoog <- max(aantal_hoog$aantal)
    waarde_aug <-geg_tabel_lj %>% filter(periode == max(periode)) %>% select(aantal)
    waarde_aug <- round(waarde_aug$aantal, 0)
    
  } else if (periode == "maand") {
    geg_tabel <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_mndnr %in% c("2019","2020", "2021")) %>%
      group_by(jaar_mndnr, maand_nr) %>%
      summarise(aantal = mean(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_mndnr,
             periode = maand_nr) %>%
      select(jaar, periode, aantal)
    geg_tabel_lj <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_mndnr %in% c("2022")) %>%
      group_by(jaar_mndnr, maand_nr) %>%
      summarise(aantal = mean(aantal),
                .groups = "drop") %>%
      mutate(jaar = jaar_mndnr,
             periode = maand_nr) %>%
      select(jaar, periode, aantal)
    gemiddelde <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_mndnr != "2022") %>%
      group_by(jaar_mndnr, maand_nr) %>%
      summarise(aantal = mean(aantal), .groups = "drop")
    gemiddelde <- mean(gemiddelde$aantal)
    aantal_hoog <- bind_rows(geg_tabel, geg_tabel_lj) %>%
      select(aantal)
    aantal_hoog <- max(aantal_hoog$aantal)
    waarde_aug <-geg_tabel_lj %>% filter(periode == max(periode)) %>% select(aantal)
    waarde_aug <- round(waarde_aug$aantal, 0)
    
  } else if (periode == "dag") { 
    geg_tabel <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_mndnr %in% c("2022")) %>%
      mutate(jaar = jaar_mndnr,
             periode = datum) %>%
      select(jaar, periode, aantal)
    gemiddelde <- zieken_onderdelen %>%
      filter(org_afk %in% afk,
             onderwerp %in% "Aantal zieken",
             jaar_mndnr != "2022")
    gemiddelde <- mean(gemiddelde$aantal)
    aantal_hoog <- max(geg_tabel$aantal)
    
  }
  
  # grafiek samenstellen
  if (periode == "dag") {
    grafiek <- ggplot() +
      geom_line(data = geg_tabel,
                aes(x = periode,
                    y = aantal,
                    colour = jaar,
                    group = jaar),
                size = rel(1.2)) +
      geom_hline(yintercept = gemiddelde,
                 linetype="dashed", colour = "black") +
      ylim(0, max(aantal_hoog)*1.01) + 
      labs(x = NULL,
           y = "aantal ziekmeldingen",
           title = "Ontwikkeling gemiddeld aantal zieken in 2022",
           subtitle = "(stippellijn = gem. aantal zieken in 2019-2021)", 
           colour = NULL) +
      theme_minimal() + 
      theme(axis.title.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            title = element_text(size = 9)) +
      theme(legend.position = "none") +
      scale_colour_manual(values = c("red"))
    
  } else { # --> periode is week of maand
    grafiek <- ggplot() +
      geom_line(data = geg_tabel,
                aes(x = periode,
                    y = aantal,
                    colour = jaar,
                    group = jaar),
                size = rel(1.2)) +
      geom_line(data = geg_tabel_lj,
                aes(x = periode,
                    y = aantal,
                    colour = jaar,
                    group = jaar),
                size = rel(1.5)) +
      geom_hline(yintercept = gemiddelde,
                 linetype="dashed", colour = "black") +
      annotate(geom = "text", 
               label = paste0("aantal aug 2022: ", waarde_aug, " zieken"),
               x = 1, y = 0, hjust = 0, vjust = 0, size = rel(3.5), 
               colour = "red", fontface = "bold") +
      ylim(0, max(aantal_hoog)*1.01) + 
      labs(x = NULL,
           y = "aantal ziekmeldingen",
           title = "Ontwikkeling aantal zieken", 
           subtitle = "(stippellijn = gem. aantal zieken in 2019-2021)", 
           colour = NULL) +
      theme_minimal() + 
      theme(axis.title.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            title = element_text(size = 9)) +
      theme(legend.position = "bottom") +
      scale_colour_manual(values = c("gray85", "gray50", 
                                     "gray10", "red"))
  }
  return(grafiek)
}


g_vergelijken_verzuim_alg <- function(afk_3a = "AZ",
                                      afk_3b = "EZK") {
  
  # gegevens samenstellen
  geg_tabel <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% c(afk_3b, afk_3a),
           jaar %in% c("2021", "2022")) %>%
    mutate(org_afk = str_c(org_afk, jaar, sep = " ")) %>%
    select(org_afk, jaar, maand, maand_nr, aantal)
  max_verzuim <- max(verzuim$aantal)

  # grafiek samenstellen
  grafiek <- ggplot() +
    geom_line(data = geg_tabel, 
              aes(x = maand, 
                  y = aantal, 
                  colour = org_afk, 
                  group = org_afk),
              size = rel(1.2)) +
    scale_colour_manual(values = c("coral", "red", 
                                   "gray50", "black")) +
#    geom_hline(yintercept = 5.3,
#               linetype="dashed", colour = "black") +
#    annotate(geom = "text", label = "gem. zv% 2018-2021",
#             x = 1, y = 5.65, hjust = 0, size = 2.8, colour = "black") +
#    annotate(geom = "text", 
#             label = paste0("zv% aug 2022: ", waarde_aug, "%"),
#             x = 1, y = 0, hjust = 0, vjust = 0, size = rel(3.5), 
#             colour = "red", fontface = "bold") +
    ylim(0, max_verzuim * 1.2) + 
    geom_hline(yintercept = gem_verzuim_rijk,
               linetype="dashed", colour = "black") +
    annotate(geom = "text", label = paste0("gem. zv% 2019-2021 ", gem_verzuim_rijk, "%"),
             x = 1, y = 5.65, hjust = 0, size = 2.8, colour = "black") +
    labs(x = NULL,
         y = "percentage ziekteverzuim",
         title = "Vergelijking van het ziekteverzuimpercentage") +
    theme_minimal() + 
    theme(axis.title.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          title = element_text(size = 9),
          axis.text.x = element_text(size = rel(1), angle = 90)) + 
    guides(colour = guide_legend(reverse = TRUE)) + 
    theme(legend.position = "right",
          legend.text = element_text(size = 9)) +
    theme(strip.text.y = element_text(size = 8)) 
  
  return(grafiek)
  
}


g_voorspellen_a <- function(afk = "Rijk",
                            aantal_maanden = 4) {
  
  geg_tabel <- verzuim %>%
    filter(org_afk == afk, 
           onderwerp == "zv%") %>%
    mutate(jr_mnd = yearmonth(as.Date(paste0(jaar,"-", maand_nr, "-1")))) %>%
    group_by(jr_mnd, jaar, maand_nr) %>%
    summarise(aantal = sum(aantal), .groups = "drop")
  geg_tabel <- tsibble(geg_tabel, index = jr_mnd)
  max_verzuim <- max(verzuim$aantal)
  
  
  
  # ETS of ARIMA model -----------------------------
  #
  # model creatie
  fit_model <- geg_tabel %>%
    #model(ETS(aantal))
    model(ARIMA(aantal))
  
  # prognotisering met model
  fc_model <- fit_model %>%
    forecast(h = paste(aantal_maanden, "months")) %>%
    autoplot(geg_tabel) + 
    geom_hline(yintercept = gem_verzuim_rijk,
               linetype="dashed", colour = "black") +
    annotate(geom = "text", label = paste0("gem. zv% 2019-2021 ", gem_verzuim_rijk, "%"),
             x = 17897, y = 5.65, hjust = 0, size = 2.8, colour = "black") +
    ylim(0, max_verzuim * 1.2) +
    theme_minimal() + 
    labs(x = NULL, y = "ziekteverzuim percentage")
  
  
  return(fc_model)
  
}


g_voorspellen_b <- function(afk = "Rijk",
                            aantal_maanden = 4) {
  
  geg_tabel <- verzuim %>%
    filter(org_afk == afk, 
           onderwerp == "zv%") %>%
    mutate(jr_mnd = yearmonth(as.Date(paste0(jaar,"-", maand_nr, "-1")))) %>%
    group_by(jr_mnd, jaar, maand_nr) %>%
    summarise(aantal = sum(aantal), .groups = "drop")
  geg_tabel <- tsibble(geg_tabel, index = jr_mnd)
  
  # ETS of ARIMA model -----------------------------
  #
  # model creatie
  fit_model <- geg_tabel %>%
    #model(ETS(aantal))
    model(ARIMA(aantal))
  
  yy1 <- fit_model[[1]][[1]]

  x <- report(fit_model)
  
  return(yy1)
  
}

#library(shiny)
#library(tsibble)
#library(tsibbledata)
#library(fabletools)
#library(fable)
#library(feasts)

#food <- tsibbledata::aus_retail %>%
#  filter(Industry == "Food retailing") %>%
#  summarise(Turnover = sum(Turnover))

#fit_model <- food %>%
#  model(ARIMA(Turnover))

#xx <- report(fit_model, coefficients())

#yy <- fit_model[[1]][[1]]
#zz1 <- yy[[1]][[1]]
#zz2 <- yy[[1]][[6]]

