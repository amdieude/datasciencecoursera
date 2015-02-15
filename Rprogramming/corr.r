
#=====================================================================
# corr function begining
#=====================================================================
corr<-function(directory, threshold = 0) {
  count_complete<-integer()
  correlation<-c()
  
  for (i in 1:332) {
    files<-paste0(directory, "/", sprintf("%03d", as.integer(i)),
                  ".csv")
    data<-read.csv(files,header=TRUE,sep=",")
    data_without_na<-data[complete.cases(data),]
    if (nrow(data_without_na) > threshold) {
      correlation<-c(correlation, cor(data_without_na$sulfate, data_without_na$nitrate))
    }
  } 
  return(correlation)
}

#=====================================================================
# corr function end
#=====================================================================




