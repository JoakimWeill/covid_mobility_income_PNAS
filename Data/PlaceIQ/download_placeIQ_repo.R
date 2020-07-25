#---------------------------------------------------------------------------------------------------------------
#---------- This code loads the PlaceIQ exposure measures
#---------------------------------------------------------------------------------------------------------------

#-------------------------------------------- download ----------------------------------------------------------------

download.file("https://github.com/COVIDExposureIndices/COVIDExposureIndices/archive/master.zip",
              destfile = "Data/PlaceIQ/COVIDExposureIndice.zip")

unzip(zipfile = "Data/PlaceIQ/COVIDExposureIndice.zip", exdir = "Data/PlaceIQ/")

fn <- "Data/PlaceIQ/COVIDExposureIndice.zip"
if (file.exists(fn)) { file.remove(fn) }

