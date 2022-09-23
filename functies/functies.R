# ===============================================================
# Project ziekteverzuim rijksoverheid
# 
# algemene functies
# 
# 
# 
# ===============================================================




# wijzig een NA in een nul waarde ------------------------
f_NA_naar_nul <- function (x) {
  x[is.na(x)] <- "0"
  return(x)
}




# logische operator toevoegen:
`%notin%` <- Negate(`%in%`)



