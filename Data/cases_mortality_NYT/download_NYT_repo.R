#---------------------------------------------------------------------------------------------------------------
#---------- This code loads NYT cases and mortality data from their github repo
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) 
getwd()

#-------------------------------------------- download ----------------------------------------------------------------

download.file("https://github.com/nytimes/covid-19-data/archive/master.zip",
              destfile = "Data/cases_mortality_NYT/NYT.zip")

unzip(zipfile = "Data/cases_mortality_NYT/NYT.zip", exdir = "Data/cases_mortality_NYT/")

fn <- "Data/cases_mortality_NYT/NYT.zip"
if (file.exists(fn)) { file.remove(fn) }






