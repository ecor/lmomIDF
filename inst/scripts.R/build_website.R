library(pkgdown)
####
destination_site="/home/ecor/local/rpackages/jrc/lmomIDF_site"
pkg ="/home/ecor/local/rpackages/jrc/lmomIDF"
build_site(pkg=pkg,override = list(destination = destination_site))
