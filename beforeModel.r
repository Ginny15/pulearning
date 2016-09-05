## preparations ####
LID.1548 = read.table("/Users/xhu/Documents/1548-links.txt")[,2]
library(solr);library(pracma);library(permute);library(data.table);library(caret);library(dummies);library(BBmisc)
url = 'http://localhost:8987/solr/LSH/afts'

#  searching with LID, 15 of them cannot be found. The rest are all cases.
L= length(LID.1548); DBID.1548 = rep(NA,L)
for(i in 1:L){
  Q1 = paste("LID:",shQuote(LID.1548[i]))
  search.i = solr_search(q=Q1, base=url, fl='*,DBID', verbose=FALSE)$DBID
  #DBID.1548[i] = ifelse(, NA, search.i) 
  #if(!isempty(search.i)) 
    DBID.1548[i]=search.i
}
summary(DBID.1548)
#  omit NA's
DBID.1548.new = DBID.1548[!is.na(DBID.1548)]
length(DBID.1548.new) #  3577
write.csv(DBID.1548.new,'dbid.1548.csv')

query = 'TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case"'
dbid.list = solr_all(q=query, base = url,fl = '*',verbose = FALSE,rows = 99999)$DBID
# dbid.list.1 = dbid.list[1:5]
withcached.cases = sapply(dbid.list,function(x) ifelse(
  is.error(try(solr_all(q=paste('DBID:',x), base = url,fl = '*,[cached]',verbose = FALSE,raw = TRUE),TRUE)),
  0, 1))
dbid.list[which(withcached.cases==0)]
DBID.list.missCache = dbid.list[which(withcached.cases==0)]
DBID.list.missCache = DBID.list.missCache[!DBID.list.missCache %in% c(200000,600000)]
#c(244787,1442699,1958389,41817,43615,34584,53845,54060,1185590,2332739)
query.need = NULL
for (x in DBID.list.missCache){
  query.need =  paste(query.need,toString(x))
}
query.need = " 244787 1442699 1958389 41817 43615 34584 53845 54060 1185590 2332739"
query = paste('TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case" AND -DBID:(',query.need,")")
result.search = solr_all(q= query, base=url, fl='*,[cached]', raw=TRUE, verbose=FALSE, rows=99999)
cachedInfo = solr_parse(result.search,'list')$response$docs
rm(result.search);gc()
timeeee = character(93434)
# test if all NER is empty ####
temp = logical()
for(i in 1:length(cachedInfo)){
  cachedInfo.i = cachedInfo[[i]]
  cached.i.ner = cachedInfo.i$ner   # several items in it
  temp[i]=isempty(cached.i.ner)}
summary(temp)  # all FALSE
rm(temp)

###  extracting features ####
#################################################################

DBID=numeric(93434);  PNAME.2=character(93434); PNAME.3=character(93434); sf.issueType=character(93434);
cm.createdTime=character(93434); sf.productType=character(93434); sf.contactName = character(93434);sf.issueSubType = character(93434);
sf.accountName = character(93434); sf.datetimeOpened=character(93434); Size=numeric(93434); 
nullProperties = character(93434); cm.content.locale=character (93434);  validationStatus=character (93434); sys.locale=character (93434);
cm.modifier=character (93434); cm.modified=character(93434); sf.type=character (93434); content.tr_time=numeric(93434); 
content.tr_ex=character (); publishingStatus=character(93434); sf.language =character(93434);
sf.isRFA = character(93434);NER.LIST = list();

for(i in 1:length(cachedInfo)){
  cachedInfo.i = cachedInfo[[i]]
  timeeee[i] = cachedInfo.i$`sf:datetimeOpened`
  cached.i.ner = cachedInfo.i$ner   # several items in it
  for (j in 1:length(cached.i.ner)) {
    if(strsplit(cached.i.ner[j],split = ":")[[1]][1] %in% c('CARDINAL','DATE','TIME')) cached.i.ner[j] <- NA
  }
  cached.i.ner = cached.i.ner[!is.na(cached.i.ner)]
  NER.LIST[[i]]=cached.i.ner
  DBID[i] = cachedInfo.i$DBID  #     1548
  splitInfo = strsplit(cachedInfo.i$PNAME[7],split='/')[[1]]
  PNAME.2[i] = splitInfo[6]
  #PNAME.3[i] = splitInfo[7]
  sf.issueType[i] = ifelse(!isempty(cachedInfo.i$`sf:issueType`),cachedInfo.i$`sf:issueType`,NA)
  cm.createdTime[i] = cachedInfo.i$`cm:created` #   "20i5-i2-iiTi8:26:25.308Z"
  sf.productType[i] = ifelse(!isempty(cachedInfo.i$`sf:productType`),cachedInfo.i$`sf:productType`,NA)
  #sf.contactName[i] = ifelse(!isempty(cachedInfo.i$`sf:contactName`),cachedInfo.i$`sf:contactName`,NA)
  Size[i] = cachedInfo.i$`cm:content.size` #   i8497
  cm.content.locale[i] = cachedInfo.i$`cm:content.locale` #   "en_US"
  #nullProperties[i] = ifelse(!isempty(cachedInfo.i$NULLPROPERTIES),cachedInfo.i$NULLPROPERTIES,NA)
  sf.isRFA[i] = cachedInfo.i$`sf:isRFA`
  sf.issueSubType[i] = ifelse(!isempty(cachedInfo.i$`sf:issueSubtype`),cachedInfo.i$`sf:issueSubtype`,NA)
  sys.locale[i] = cachedInfo.i$`sys:locale` #   "en_US_"
  cm.modifier[i] = cachedInfo.i$`cm:modifier` #   "admin"
  cm.modified[i] = cachedInfo.i$`cm:modified` #   "20i6-03-i4Ti7:00:i2.23iZ"
  sf.datetimeOpened[i] = cachedInfo.i$`sf:datetimeOpened` #   "20i6-03-i4Ti6:52:48.000Z"
  #sf.accountName[i] = ifelse(!isempty(cachedInfo.i$`sf:accountName`),cachedInfo.i$`sf:accountName`,NA)
  content.tr_time[i] = cachedInfo.i$`cm:content.tr_time` #   i7
}
rm(cachedInfo);gc()

###  TF*IDF mechanism ####
#################################################################
# facet: get the count of all Enitities in content
query = 'TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case"'
NER.DF = solr_facet(q=query,facet.field  = "ner",base=url,verbose = FALSE,facet.limit  = 9999)$facet_fields$ner   
# CARDINALs are non-sense, remove them
NER.DF = data.frame(NER = unlist(NER.DF$X1),COUNT = unlist(NER.DF$X2))
NER.DF$NER=as.character(NER.DF$NER)
for (i in 1:nrow(NER.DF)) {
  if(strsplit(NER.DF[i,1],split = ":")[[1]][1] %in% c('CARDINAL','DATE','TIME')) NER.DF[i,2] <- NA
}
NER.DF.NEW = NER.DF[!is.na(NER.DF$COUNT),]
# = 7102
NER.DF.NEW.1 = NER.DF.NEW[which(NER.DF.NEW$COUNT>500),]
NER.ds = data.frame(dummy(NER.DF.NEW.1$NER))
NER.ds.new = as.data.frame(matrix(0, ncol = ncol(NER.ds), nrow = length(NER.LIST)))
colnames(NER.ds.new) = colnames(NER.ds)  
colnames(NER.ds.new) <- gsub("NER.","",colnames(NER.ds.new))
rm(NER.ds)
X = which(NER.ds.new==0,arr.ind = TRUE)

ENTITIES = unlist(NER.LIST)

df = data.frame(docID = rep(1:length(NER.LIST), sapply(NER.LIST, function(x) length(x))), entityName = unlist(NER.LIST))

document.frequency = rep(0,length(usefulEntity))
i=1
for (ner in usefulEntity) {
  document.frequency[i]=length(unique(df$docID[which(df$entityName==ner)]))
  i=i+1
}

temp = df %>%  group_by(entityName,docID) %>%  mutate(count = n()) 
df$count = temp$count
df = df[which(df$count > 500),]
df$entityName <- as.factor(as.character(df$entityName))
df.2 = dummy(df$entityName)
df.2 = cbind(df$docID,df.2)
temp = df %>%  group_by(entityName) %>%  mutate(count = n()) 
df$count = temp$count
rownames(df.2) = df.2$docID

NERdfwithref = data.frame(NERref = 1:nrow(NER.DF.NEW.1),NER =  NER.DF.NEW.1$NER,count =NER.DF.NEW.1$COUNT  )

entitycounttable = data.frame(table(ENTITIES))
entitycounttable = entitycounttable[which(entitycounttable$Freq > 100),]
for(i in 1:length(NER.LIST)){
  cached.i.ner = NER.LIST[[i]]   # several items in it
  for (j in cached.i.ner) {
    if(!j %in% entitycounttable$ENTITIES) cached.i.ner[j] <- NA
  }
  cached.i.ner = cached.i.ner[!is.na(cached.i.ner)]
  NER.LIST[[i]]=cached.i.ner
}
df = data.frame(docID = rep(1:length(NER.LIST), sapply(NER.LIST, function(x) length(x))), entityName = unlist(NER.LIST))

NER.count = nrow(NER.DF.NEW.1)
size = length(NER.LIST)

term.frequency = data.frame(matrix(0,length(NER.LIST),length(usefulEntity)))
for (k in 1:length(NER.LIST)){
  ner = NER.LIST[[k]]
  termfreq = data.frame(table(ner))
  for (j in 1:length(usefulEntity)){
      term.frequency[k,j] = ifelse(usefulEntity[j] %in% ner,termfreq$Freq[which(termfreq$ner==usefulEntity[j])],0)
    }
}

tf.idf = term.frequency
size = length(NER.LIST)
  for (j in 1:98){
    tf.idf[,j] = term.frequency[,j] * log(size/document.frequency[j])
  }

###  splitting time sequances into year, month, day and hour ####
data = cbind(DBID,PNAME.2,sf.issueType,cm.createdTime,sf.productType,Size,cm.content.locale,
             sf.isRFA,sf.issueSubType,sys.locale,cm.modified,cm.modifier,sf.datetimeOpened,
             content.tr_time)
data = data.frame(data[-dbid.no.entity.idx,])
for(i in 1:ncol(data)){
  data[[i]]=as.character(data[[i]])
}
TimeSplit = sapply(data[['cm.createdTime']][1:nrow(data)], function(x) strsplit(x, split = 'T|:|-')[[1]])
data$createdYear = TimeSplit[1,]
data$createdMonth = TimeSplit[2,]
data$createdDay = TimeSplit[3,]
data$createdHour = TimeSplit[4,]

TimeSplit = sapply(data[['cm.modified']][1:nrow(data)], function(x) strsplit(x, split = 'T|:|-')[[1]])
data$modifiedYear = TimeSplit[1,]
data$modifiedMonth = TimeSplit[2,]
data$modifiedDay = TimeSplit[3,]
data$modifiedHour = TimeSplit[4,]

TimeSplit = sapply(data[['sf.datetimeOpened']][1:nrow(data)], function(x) strsplit(x, split = 'T|:|-')[[1]])
data$openedYear = TimeSplit[1,]
data$openedMonth = TimeSplit[2,]
data$openedDay = TimeSplit[3,]
data$openedHour = TimeSplit[4,]


###  shuffle, split data ####
#################################################################
data$sf.datetimeOpened<-NULL;data$cm.createdTime<-NULL;data$cm.modified<-NULL
data = data[shuffle(nrow(data)),]
data[['Size']]=as.numeric(data[['Size']])
data$label =  ifelse(data$DBID %in% DBID.1548.new, 'Y','N'); data$label=as.factor(data$label)
for (i in 1:24) {
  if(typeof(data[[i]]) == 'character')
    data[[i]] = as.factor(data[[i]])
}

# sf.issueType  NA  838
# sf.contactName NA 753
# sf.issueSubType NA 2655
# sf.accountName NA 683
for (col in c('sf.issueType','sf.issueSubType','sf.productType')){
  modeName = names(which.max(table(data[[col]])))
  for (i in 1: nrow(data)){
    if(is.na(data[[col]][i])) data[[col]][i] = modeName
  }
}
summary(data)

data = cbind(data,tf.idf)



data.2=data;data.1=data[,1:24];data=NULL

data=data.1[,c(1:10,24)]



score.search = solr_all(q= 'FINGERPRINT:1548', base=url, fl='*,DBID,score', verbose=FALSE, rows=99999)
score.df = data.frame(DBID = score.search$DBID, score = score.search$score)
data = merge(x = data.1[,c(1:10,24)], y = score.df, by = "DBID", all.x = TRUE)
data$score[which(is.na(data$score))] = 0
tf.idf.sum = rowSums(tf.idf)
data = cbind(data,tf.idf.sum)

data.1$tf.idf.sum=NULL;

data = cbind(data.1,tf.idf) 

trainIDX = sample(nrow(data),nrow(data)*0.75,replace = FALSE)
train = data[trainIDX,]
test = data[-trainIDX,]

P = train[train$label=='Y',]
U = train[train$label=='N',]

###   spy technique  ######
#########################
control = trainControl(method="cv", number=3, classProbs = TRUE)
RNlist = NULL
for(itr in 1:10){
  RN = NULL
  idx.S = sample(nrow(P),0.15*nrow(P),replace = FALSE)
  S = P[idx.S,]
  Us = rbind(U,S);Us = Us[shuffle(nrow(Us)),];Ps = P[-idx.S,]
  Ps$label = 'Y';Us$label = 'N'
  PsUs = rbind(Ps,Us)
  PsUs = PsUs[shuffle(nrow(PsUs)),];PsUs$label =as.factor(PsUs$label)
  fit.xgb  <- train(label~., data = PsUs[,-c(1:10)], method='xgbTree',trControl=control)
  pred.Us = predict(fit.xgb,Us[,-c(1:10)],type = 'prob')$Y
  pred.S = predict(fit.xgb,S[,-c(1:10)],type='prob')$Y
  t = min(pred.S);j=1
  for (i in 1: length(pred.Us)){
    if (pred.Us[i]<t){
      RN[j] = Us[i,1];j=j+1
    }
  }
  RNlist[[itr]] = RN
}
insecRN = RNlist[[1]]
for (i in 2:10){
  insecRN = intersect(insecRN,RNlist[[i]])
}
# insecRN +P

Us[,1] =as.numeric(Us[,1])
RN = Us[which(Us[,1] %in% insecRN),]
P.sample=P[sample(nrow(P),500,replace = FALSE),]
data.step.2 = rbind(RN,P.sample); data.step.2=data.step.2[shuffle(nrow(data.step.2)),]
fit.xgb  <- train(label~., data = data.step.2[,-c(1:10)], method='xgbTree',trControl=control)
pred.test = predict(fit.xgb,test[,-c(1:10)])
confusionMatrix(pred.test,test$label)$table

# Reference
# Prediction     N     Y
# N 18827     4
# Y  3631   894
prob.r = 894/898
prob.fx.1 = length(pred.test[which(pred.test=='Y')])/length(pred.test)
metric = prob.r^2/prob.fx.1 #5.11


kmeans()
write.csv(cbind(data,data.frame(DBID=data$DBID,original=data$label,pred=predict(fit.xgb,data[,11:13]),prob = predict(fit.xgb,data[,11;13],type = 'prob'))),'results_all_catego.csv')
data1



newdf = data.frame(DBID=test$DBID,original=test$label,pred=pred.test,prob = predict(fit.xgb,test[,-1],type = 'prob'))

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     N     Y
# N 19067     3
# Y  3382   904
prob.r = 904/907
prob.fx.1 = length(pred.test[which(pred.test=='Y')])/length(pred.test)
prob.r^2/prob.fx.1   #5.4


###  Cosine-Rocchio (CR) Technique  ####
########################################
PN = NULL; RN= NULL;
# NER.ds is the data frame for TF_IDF scheme
norm(NER.ds[1,],'2')
V.p = apply(P[,23:77], 1,function(x) sum(x/norm(x,'2')))/nrow(P)
normVp = norm(V.p,'2')
cos.Vp.d = apply(U[,23:77],1,function(x) x %*% V.p/(norm(x,'2')*normVp))
cos.DF = data.frame(DBID=P$DBID, cosV = cos.Vp.d)
cos.DF = cos.DF[with(cos.DF, order(-cos.Vp.d)), ]
defaultPosition = floor((1-0.05)*nrow(P))
omiga = cos.DF$cosV[defaultPosition]
for (i in 1: nrow(U)){
  if(cos.DF$cosV < omiga) PN=append(PN,U[i,])
}
alpha = 16; beta =4;
c.p.1 = apply(P[,23:77], 1,function(x) sum(x/norm(x,'2')))/nrow(P) 
c.p.2 = apply(PN[,23:77], 1,function(x) sum(x/norm(x,'2')))/nrow(PN)
c.p = alpha*c.p.1- beta*c.p.2
c.pn = alpha*c.p.2 - beta*c.p.1
normCP = norm(c.p, '2'); normCPN= norm(c.pn,'2');
cos.pn.d = apply(U[,23:77],1,function(x) x %*% c.pn/(norm(x,'2')*normCPN))
cos.p.d = apply(U[,23:77],1,function(x) x %*% c.p/(norm(x,'2')*normCP))

for (i in 1: nrow(U)){
  if (cos.pn.d>cos.p.d) RN = append(RN,U[i,])
}
#  term token table ####
url.json= "http://localhost:8986/solr/LSH/terms?wt=json&terms.regex=\\{en\\}[0-9]%2B&terms.fl=content@s__lt@{http://www.alfresco.org/model/content/1.0}content&terms.limit=100"
newcache = fromJSON(readLines(url.json))$terms$`content@s__lt@{http://www.alfresco.org/model/content/1.0}content`


url.json= "http://localhost:8986/solr/LSH/terms?wt=json&terms.fl=TYPE&terms.limit=-1"
newcache = fromJSON(readLines(url.json))$terms$TYPE
names=character();j=1
for(i in seq(1,length(newcache),2)){
  names[j]=newcache[[i]];  j=j+1
}
names = sapply(names, function(x) substr(x,5,1000))
counts=numeric(93434);j=1
for(i in seq(2,length(newcache),2)){
  counts[j]=newcache[[i]];  j=j+1
}

TermTable=data.frame(term = names,termCount = counts)


contentTokens=readLines('http://localhost:8986/solr/LSH/terms?terms.fl=content@s__lt@{http://www.alfresco.org/model/content/1.0}content&wt=JSON&terms.limit=100')

library(XML)
xmlfile = 
  xmlTreeParse("http://localhost:8986/solr/LSH/terms?terms.regex=\\{en\\}[0-9]%2B&terms.fl=content@s__lt@{http://www.alfresco.org/model/content/1.0}content&terms.limit=100") 
xmltop = xmlRoot(xmlfile)
xmlfile
readfiles = readLines("http://localhost:8986/solr/LSH/terms?terms.regex=\\{en\\}[0-9]%2B&terms.fl=content@s__lt@{http://www.alfresco.org/model/content/1.0}content&terms.limit=100")
cat(readfiles[3])
readfiles[grep("</lst>", readfiles)]

plantcat <- 
  xmlApply(xmlfile$doc$children$response, function(x) xmlValue(x))
xmlfile2 = xmlParse('http://localhost:8986/solr/LSH/terms?terms.fl=content@s__lt@{http://www.alfresco.org/model/content/1.0}content&terms.limit=100')
xmlRoot(xmlfile2)
# HIDING CODES ####
# url.2 = 'http://localhost:8986/solr/LSH/afts'
# 
# url.3 = 'http://localhost:8986/solr/LSH'
# 
# solr_mlt(q = "FINGERPRINT:2289",mlt.fl = "TYPE", base = url,rows = 100)
# solr_all(q='*:*',base=url.2,verbose=TRUE,qt = '/terms',terms.fl='the')
# 
# http://localhost:8983/solr/collection1/select?qt=/terms&terms.fl=INDUSTRY&terms.prefix=P&terms=true

# termLists <- vector(mode = "list", length = length(content))
# for (i in 1:length(content)){
#   elementsList = strsplit(content[i], ' ')
#   j = 1
#   for(elements in elementsList){
#     if (elements !="" & elements !=""){
#       termLists[[i]][j] = elements; j=j+1
#     }}
#   table(elementsList.new)
# }
content.split = strsplit(content[1],split = "[-(),:.!?\\-\\|]|\n|\r| |(\\d+)|\t|/")[[1]]
content.split[content.split != ""]
content.split = tolower(content.split[content.split != ""])
table(content.split) 

library(rjson)
fromJSON(readLines('http://localhost:8986/solr/LSH/query?q=FINGERPRINT'))

library(XML)
xmlfile = xmlTreeParse('http://localhost:8986/solr/LSH/query?q=FINGERPRINT&wt=JSON' ) 
xmltop = xmlRoot(xmlfile)
xmlApply(xmlfile$doc$children$response, function(x) xmlValue(x))

readL = readLines('http://localhost:8986/solr/LSH/query?q=fingerprint&rows=100')
dbid.3 = character();j=1
for (i in seq(12,length(readL),4)){
  dbid.3[j]=readL[i];j=j+1
}
dbid.3 = data.frame(dbid = as.numeric(sapply(dbid.3,function(x) strsplit(x, ':|}')[[1]][2])))
intersect(dbid.3$sapply.dbid.3..function.x..strsplit.x..........1...2..,dbid.1$DBID)
