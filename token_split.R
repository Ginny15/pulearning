timeeee = solr_search(q='FINGERPRINT:1548 AND TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case"
', base=url, fl='*,DBID,score', verbose=FALSE,rows = 9999)
DBID.0808 = timeeee$DBID
intersect(DBID.0808,test[which(pred.test=='Y'),]$DBID)
intersect(DBID.0808,DBID.1548.new)
cbind(DBID.1548.new
score.0808 = timeeee$score
dataframe.0808 = data.frame(DBID=DBID.0808,score = score.0808)

fit.xgb.0808  <- train(label~., data = data.step.2[,-1], method='xgbTree',trControl=control)

DBID.unclassified = DBID[!DBID %in% DBID.1548.new]
DBID.unclassified.sample = DBID.unclassified[sample(length(DBID.unclassified),10000)]
setwd('/Users/xhu/Documents/solr_data/')
for (i in 1: length(cachedInfo)){
  dbid.i = cachedInfo[[i]]$DBID
  if(dbid.i %in% DBID.1548.new) {
    write(cachedInfo[[i]]$`cm:content`,paste0('positive/',dbid.i,'.txt'))}
  else {
    write(cachedInfo[[i]]$`cm:content`,paste0('unclassified/',dbid.i,'.txt'))}
}

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
  