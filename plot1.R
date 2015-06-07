plot1 <- function(directory, threshold = 0) {
  cr<-numeric()
  completedata<-complete(directory)
  for (ids in 1:332) {
    if (completedata[ids,2]>threshold) {
      csvfile <- if (ids < 10) { 
        paste0(0,0,ids,".csv")
      } else if (ids < 100) {
        paste0(0,ids,".csv")
      } else {
        paste0(ids,".csv")
      }
      filepath<-file.path(directory,csvfile)
      datacsv<-read.csv(filepath)
      corrids<-cor(datacsv[,2],datacsv[,3], use="pairwise.complete.obs")
      cr<-append(cr,corrids)
    }
  }
  cr
}