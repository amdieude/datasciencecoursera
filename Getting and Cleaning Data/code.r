
setwd("C:\\DATA SCIENCE SPECIALIZATION\\Getting Data")
if (!file.exists("data")) {
  dir.create("data")
}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD&bom=true"
download.file(fileUrl, destfile = "./data/cameras.csv")
list.files("./data")