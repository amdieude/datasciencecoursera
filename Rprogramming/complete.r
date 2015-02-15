#=====================================================================
# complete function begining
#=====================================================================
complete<-function(directory, range = 1:332) {
#
data_frame<-data.frame(id = integer(), nobs = integer())   
#
for (i in range) {
    files<-paste0(directory, "/", sprintf("%03d", as.integer(i)),
         ".csv")
    data<-read.csv(files, header=TRUE, sep=",")
    data_f<-data.frame(id = i, nobs = sum(complete.cases(data)))
    data_frame<-rbind(data_frame, data_f)
     }
   data_frame
}

#=====================================================================
# complete function end
#=====================================================================
