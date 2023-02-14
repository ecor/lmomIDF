###

###
years <- 1985:2015
uu <- list()

y <- rast(ymax=29,ymin=27,xmin=33,xmax=37)

##years <- 1985:1986
for (year in years) {
  
  rr <- "/home/ecor/local/data/climate/jrc/lmomIDF_example_ext_data/chirps_test_area/chirps_%d.grd" %>% sprintf(year) %>% rast()
  uu[[as.character(year)]] <- crop(x=rr,y=y)

}

