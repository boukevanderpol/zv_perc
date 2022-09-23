# ===============================================================
# Project ziekteverzuim rijksoverheid
# 
# Functies om gegevens te laden en klaar te maken voor gebruik
# 
# 
# 
# ===============================================================

# library(tidyverse)

# verzuimpercentage -----------------------------------
f_laden_verzuim <- function(x) { 
  
  temp <- readr::read_delim(
  file = x,
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_names = TRUE,
  col_types = list(
    organisatie = col_factor(levels = c(
      "Het Rijk",
      "Ministerie van BZK",
      "Ministerie van AZ",
      "Concern Justitie en Veiligheid",
      "Hoge Colleges van Staat",
      "Ministerie van Sociale Zaken en Werkgele",
      "Ministerie van OCW",
      "Ministerie van V.W.S.",
      "Ministerie van Economische Zaken en Klim",
      "Ministerie van Financien",
      "Ministerie van Infrastructuur en Waterst",
      "Eerste Kamer der Staten-Generaal",
      "Tweede Kamer der Staten-Generaal",
      "Ministerie van LNV",
      "Autoriteit Persoonsgegevens",
      "Bureau Financieel Toezicht",
      "Raad van State",
      "Buitenlandse zaken")),
    org_afk = col_factor(levels = c("Rijk", "BZK", "AZ",
                                    "J&V", "HCvS", "SZW",
                                    "OCW", "VWS", "EZK",
                                    "FIN", "I&W", "EK",
                                    "TK", "LNV", "AP",
                                    "BFT", "RvS", "BuZa")),
    soort = col_factor(levels = c("Rijk", "Ministerie",
                                  "Divers", "Diensten")),
    onderwerp = col_factor(levels = c("zv%", "zv% 1-7", "zv% 8-42",
                                      "zv% 43-365", "zv%> 1 jaar"),
                           ordered = TRUE),
    jaar = col_factor(levels = c("2018", "2019", "2020",
                                 "2021", "2022"),
                      ordered = TRUE),
    maand = col_factor(levels = c("jan", "feb", "mrt", "apr", "mei", "jun",
                                  "jul", "aug", "sep", "okt", "nov", "dec"),
                       ordered = TRUE),
    maand_nr = col_factor(levels = c("1", "2", "3", "4",
                                     "5", "6", "7", "8",
                                     "9", "10", "11", "12"),
                          ordered = TRUE),
    aantal = col_double()
  ))
  return(temp)

}

verzuim <- f_laden_verzuim("./gegevens_bewerkt/verzuim.csv")
verzuim <- verzuim %>% filter(jaar != "2018")
verzuim_duurklassen <- f_laden_verzuim("./gegevens_bewerkt/verzuim_duurklassen.csv")
verzuim_duurklassen <- verzuim_duurklassen %>% filter(jaar != "2018")

gem_verzuim_rijk <- verzuim %>%
  filter(org_afk == "Rijk",
         jaar != "2022")
gem_verzuim_rijk <- round(mean(gem_verzuim_rijk$aantal), 1)

#organisatie;org_afk;soort;onderwerp;datum;jaar_mndnr;maand_nr;jaar_wknr;week_nr;aantal
# zieken en ziekmeldingen ---------------------------------
f_laden_zieken_en_ziekmeldingen <- function(x) { 

  temp <- readr::read_delim(
    file = x,
    delim = ";",
    locale = locale(decimal_mark = ","),
    col_names = TRUE,
    col_types = list(
      organisatie = col_factor(levels = c(
        "Het Rijk",
        "Ministerie van BZK",
        "Ministerie van AZ",
        "Concern Justitie en Veiligheid",
        "Hoge Colleges van Staat",
        "Ministerie van Sociale Zaken en Werkgele",
        "Ministerie van OCW",
        "Ministerie van V.W.S.",
        "Ministerie van Economische Zaken en Klim",
        "Ministerie van Financien",
        "Ministerie van Infrastructuur en Waterst",
        "Eerste Kamer der Staten-Generaal",
        "Tweede Kamer der Staten-Generaal",
        "Ministerie van LNV",
        "Autoriteit Persoonsgegevens",
        "Bureau Financieel Toezicht",
        "Raad van State",
        "Buitenlandse zaken")),
      org_afk = col_factor(levels = c("Rijk", "BZK", "AZ",
                                      "J&V", "HCvS", "SZW",
                                      "OCW", "VWS", "EZK",
                                      "FIN", "I&W", "EK",
                                      "TK", "LNV", "AP",
                                      "BFT", "RvS", "BuZa")),
      soort = col_factor(levels = c("Rijk", "Ministerie",
                                    "Divers", "Diensten")),
      onderwerp = col_factor(levels = c("Aantal zieken", "Aantal ziekmeldingen")),
      datum = col_date(format = ""),
      jaar_mndnr = col_factor(levels = c("2018", "2019", "2020",
                                         "2021", "2022"),
                              ordered = TRUE),
      maand_nr = col_factor(levels = c("1", "2", "3", "4",
                                       "5", "6", "7", "8",
                                       "9", "10", "11", "12"),
                            ordered = TRUE),
      jaar_wknr = col_factor(levels = c("2018", "2019", "2020",
                                        "2021", "2022"),
                             ordered = TRUE),
      week_nr = col_factor(levels = c("1", "2", "3", "4",
                                      "5", "6", "7", "8",
                                      "9", "10", "11", "12",
                                      "13", "14", "15", "16",
                                      "17", "18", "19", "20",
                                      "21", "22", "23", "24",
                                      "25", "26", "27", "28",
                                      "29", "30", "31", "32",
                                      "33", "34", "35", "36",
                                      "37", "38", "39", "40",
                                      "41", "42", "43", "44",
                                      "45", "46", "47", "48",
                                      "49", "50", "51", "52",
                                      "53"),
                           ordered = TRUE),
      aantal = col_double()
    ))
  return(temp)
  }

zieken_rijk <- f_laden_zieken_en_ziekmeldingen("./gegevens_bewerkt/zieken_rijk.csv")
zieken_rijk <- zieken_rijk %>% filter(jaar_mndnr != "2018")  # LELIJK
zieken_onderdelen <- f_laden_zieken_en_ziekmeldingen("./gegevens_bewerkt/zieken_onderdelen.csv")
zieken_onderdelen <- zieken_onderdelen %>% filter(jaar_mndnr != "2018")  # LELIJK
zieken_onderdelen <- bind_rows(zieken_onderdelen, zieken_rijk)

ziekmeldingen_rijk <- f_laden_zieken_en_ziekmeldingen("./gegevens_bewerkt/ziekmeldingen_rijk.csv")
ziekmeldingen_rijk <- ziekmeldingen_rijk %>% filter(jaar_mndnr != "2018")  # LELIJK
ziekmeldingen_onderdelen <- f_laden_zieken_en_ziekmeldingen("./gegevens_bewerkt/ziekmeldingen_onderdelen.csv")
ziekmeldingen_onderdelen <- ziekmeldingen_onderdelen %>% filter(jaar_mndnr != "2018")  # LELIJK
ziekmeldingen_onderdelen <- bind_rows(ziekmeldingen_onderdelen, 
                                      ziekmeldingen_rijk)






