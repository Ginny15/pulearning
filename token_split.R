for ( i in 1:length(cachedInfo)){
  rrr = strsplit(cachedInfo[[2629]]$`cm:content`, split = "[-(),:.!?\\-\\|]|\n|\r| |(\\d+)|\t|/|_")[[1]]
  wordStem(rrr[rrr != ""])
}

write(wordStem(rrr[rrr != ""]),paste0('ccccc','.txt'))
 
library(Snowball) 

[6] "5/Company Home/Sites/sfdccases/documentLibrary/001/353"              
[7] "6/Company Home/Sites/sfdccases/documentLibrary/001/353/00135322.html"
 
data.1 = data[which(data$label == 'N'),c(2:10)]
data.1 = dummy.data.frame(data.1)
cl = kmeans(data.1,5)
newdf=data.frame(cbind(data.1,cl$cluster))
table(newdf$cl.cluster,newdf$label)
which(newdf$cl.cluster==0)

dbid.0809 = numeric(0)
for (i in 1: length(cachedInfo)){
  dbid.i = cachedInfo[[i]]$DBID
  if(dbid.i %in% DBID.1548.new) {
    dbid.0809[i] = dbid.i}
  
