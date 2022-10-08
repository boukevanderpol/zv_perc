# ===============================================================
# Project ziekteverzuim rijksoverheid
# 
# functies waaarmee waarden voor de valueboxen worden gevuld
# 
# 
# 
# ===============================================================


vb_zv_perc <- function(afk = "AZ") {
  
  geg_tabel_lj <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2022"))
  geg_tabel_lj$maand_nr <- as.integer(geg_tabel_lj$maand_nr)
  zv_perc <- geg_tabel_lj$aantal[max(geg_tabel_lj$maand_nr)]
  zv_perc <- round(zv_perc, 2)
  
  return(zv_perc)

}



vb_zv_kleur <- function(afk = "AZ") {
  
  geg_tabel_lj <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2022"))
  geg_tabel_lj$maand_nr <- as.integer(geg_tabel_lj$maand_nr)
  zv_perc <- geg_tabel_lj$aantal[max(geg_tabel_lj$maand_nr)]
  kleur <- ifelse(gem_verzuim_rijk > zv_perc, "green", "maroon") 
  
  return(kleur)
  
}



vb_zv_icoon <- function(afk = "AZ") {
  
  geg_tabel_lj <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2022"))
  geg_tabel_lj$maand_nr <- as.integer(geg_tabel_lj$maand_nr)
  zv_perc <- geg_tabel_lj$aantal[max(geg_tabel_lj$maand_nr)]
  icoon <- ifelse(gem_verzuim_rijk > zv_perc, "arrow-up", "arrow-down") 
  
  return(icoon)
  
}



vb_aantal_zieken <- function(afk = "AZ") {
  
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
  geg_tabel_lj$periode <- as.integer(geg_tabel_lj$periode)
  aantal_z <- geg_tabel_lj$aantal[max(geg_tabel_lj$periode)]
  aantal_z <- round(aantal_z, 0)
  
  return(aantal_z)
  
}



vb_aantal_ziekmeldingen <- function(afk = "BZK") {
  
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
  geg_tabel_lj$periode <- as.integer(geg_tabel_lj$periode)
  aantal_zm <- geg_tabel_lj$aantal[max(geg_tabel_lj$periode)]

  return(aantal_zm)
  
}



