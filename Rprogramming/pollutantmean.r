#=====================================================================
# Pollutantmean function begining
#=====================================================================
pollutantmean<-function(directory, pollutant, id=1:332){ 
#Creation of a local variable to holp pollutant mean
data<-c()
#Listing all files in "directory"
listfiles<-as.character(list.files(directory))
#Files paths creation by concatening directory and files names
filespaths<-paste0(directory,"/",listfiles)
#Loop over the selected range of data files and data importation
for (i in id) { 
selected<-read.csv(filespaths[i], header=TRUE, sep=",")
pollutant
#Deleting rows with NA values for th e selected pollutant
data_without_na <- selected[!is.na(selected[, pollutant]), pollutant]
#Updating the local variable for mean calculation
data <- c(data, data_without_na)
}
#Creating a results object containing the calculated mean
output<-round(mean(data),3)
#return mean of the choosen pollutant according to the rang of data
return(output)
}
#=====================================================================
# Pollutantmean function end
#=====================================================================

#Verification of resuts
pollutantmean("specdata", "sulfate", 1:10) == 4.064
pollutantmean("specdata", "nitrate", 70:72) == 1.706
pollutantmean("specdata", "nitrate", 23) == 1.281

