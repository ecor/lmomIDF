##https://omirl.regione.liguria.it/#/map

library(magrittr)
years <- 1980:2022


##https://ambientepub.regione.liguria.it/SiraQualMeteo/script/PubAccessoDatiMeteoPost.asp?CodParam=PRECPBIWC1&CodTema=STAZIONE&IdEstraz=DE&Frequenza=HH&TipoOutput=HTML&Separatore=TAB&IdRichiesta=52052629139451&IdRichiestaCarto=&DataIniz=01/01/%04d&InizOra=00:00&DataFine=31/12/%04d&FineOra=23:59
urls <- "https://ambientepub.regione.liguria.it/SiraQualMeteo/script/PubAccessoDatiMeteoPost.asp?CodParam=PRECPBIWC1&CodTema=STAZIONE&IdEstraz=DE&Frequenza=HH&TipoOutput=HTML&Separatore=,&IdRichiesta=52052629139451&IdRichiestaCarto=&DataIniz=01/01/%04d&InizOra=00:00&DataFine=31/12/%04d&FineOra=23:59"
files <- "/home/ecor/local/rpackages/jrc/lmomIDF/inst/ext_data/liguria/html/chiavari_precipitation_%04d.html"
for (year in years) {

  file <- files %>% sprintf(year)
  url <- urls %>% sprintf(year,year)
  o <- readLines(url) %>% writeLines(con=file) 
  
  
  
  
}  
