## preparations ####
library(solr);library(pracma);library(permute);library(data.table);library(caret);library(dummies);library(BBmisc)
LID.1548 = read.table("/Users/xhu/Documents/1548-links.txt")[,2]
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
summary(DBID.1548);
#  omit NA's
DBID.1548.new = DBID.1548[!is.na(DBID.1548)]
#write.csv(DBID.1548.new,'dbid.1548.csv')

# query = 'TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case"'
# dbid.list = solr_all(q=query, base = url,fl = '*',verbose = FALSE,rows = 99999)$DBID
# # dbid.list.1 = dbid.list[1:5]
# withcached.cases = sapply(dbid.list,function(x) ifelse(
#   is.error(try(solr_all(q=paste('DBID:',x), base = url,fl = '*,[cached]',verbose = FALSE,raw = TRUE),TRUE)),
#   0, 1))
# dbid.list[which(withcached.cases==0)]
# DBID.list.missCache = dbid.list[which(withcached.cases==0)]
# DBID.list.missCache = DBID.list.missCache[!DBID.list.missCache %in% c(200000,600000)]
# #c(244787,1442699,1958389,41817,43615,34584,53845,54060,1185590,2332739)
# query.need = NULL
# for (x in DBID.list.missCache){
#   query.need =  paste(query.need,toString(x))
# }
query.need = " 244787 1442699 1958389 41817 43615 34584 53845 54060 1185590 2332739"
query = paste('TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case" AND -DBID:(',query.need,")")
rm(query.need);rm(i,L,Q1,search.i)
# result.search = solr_all(q= query, base=url, fl='*,[cached]', raw=TRUE, verbose=FALSE, rows=99999)
cachedInfo = solr_parse(solr_all(q= query, base=url, fl='*,[cached]', raw=TRUE, verbose=FALSE, rows=99999),'list')$response$docs
featureNames = names(cachedInfo[[1]])

solr_facet(base=url,facet.limit=100,verbose=FALSE, facet.field = featureNames[12],q='TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case"')$facet_field

###  extracting features ####
#################################################################

DBID=numeric(93434);  PATH = character(93434);
PNAME.2=character(93434); PNAME.3=character(93434); issueType=character(93434);
createdTime=character(93434); productType=character(93434); contactName = character(93434);
caseNUM = character(93434);  identifier = character(93434); contentID =character(93434); 
Size=numeric(93434); language =character(93434); NER.LIST = list();  
tr_time=numeric(93434); caseID=character(93434);title=character(93434);issueSubType = character(93434);
accountName = character(93434); datetimeOpened=character(93434); modifiedTime=character(93434);  

for(i in 1:length(cachedInfo)){
  cachedInfo.i = cachedInfo[[i]]
  # cached.i.ner = cachedInfo.i$ner   # several items in it
  # for (j in 1:length(cached.i.ner)) {
  #   if(strsplit(cached.i.ner[j],split = ":")[[1]][1] %in% c('CARDINAL','DATE','TIME')) cached.i.ner[j] <- NA}
  # NER.LIST[[i]]= cached.i.ner[!is.na(cached.i.ner)]
  DBID[i] = cachedInfo.i$DBID;PATH[i]=cachedInfo.i$PATH
  splitInfo = strsplit(cachedInfo.i$PNAME[7],split='/')[[1]]
  PNAME.2[i] = splitInfo[6];PNAME.3[i] = splitInfo[7]
  # issueType[i] = ifelse(!isempty(cachedInfo.i$`sf:issueType`),cachedInfo.i$`sf:issueType`,NA)
  #createdTime[i] = cachedInfo.i$`cm:created` #   "20i5-i2-iiTi8:26:25.308Z"
  productType[i] = ifelse(!isempty(cachedInfo.i$`sf:productType`),cachedInfo.i$`sf:productType`,NA)
  contactName[i] = ifelse(!isempty(cachedInfo.i$`sf:contactName`),cachedInfo.i$`sf:contactName`,NA)
  # caseNUM[i]=strsplit(cachedInfo.i$`cm:description`,' ')[[1]][3] 
  # contentID[i]=cachedInfo.i$`cm:content.docid`;Size[i] = cachedInfo.i$`cm:content.size` #   i8497
  language[i] = cachedInfo.i$`cm:content.locale`; 
 tr_time[i] = cachedInfo.i$`cm:content.tr_time` #   i7
  #  title[i]=cachedInfo.i$`cm:title`;
  # issueSubType[i] = ifelse(!isempty(cachedInfo.i$`sf:issueSubtype`),cachedInfo.i$`sf:issueSubtype`,NA)
  # accountName[i] = ifelse(!isempty(cachedInfo.i$`sf:accountName`),cachedInfo.i$`sf:accountName`,NA)
  #datetimeOpened[i] = cachedInfo.i$`sf:datetimeOpened` #   "20i6-03-i4Ti6:52:48.000Z"
  #modifiedTime[i] = cachedInfo.i$`cm:modified` #   "20i6-03-i4Ti7:00:i2.23iZ"
}

mydata=data.frame(cbind(DBID,PNAME.2,productType,contactName,Size,language,
             tr_time,accountName))

mydata$Size=as.numeric(mydata$Size);mydata$tr_time=as.numeric(mydata$tr_time);
mydata$label =  ifelse(mydata$DBID %in% DBID.1548, 'Y','N'); mydata$label=as.factor(mydata$label)
summary(mydata$label)

TimeSplit = sapply(as.character(mydata[['createdTime']]), function(x) strsplit(x, split = 'T')[[1]])
mydata$createdDate = as.Date(TimeSplit[1,])
TimeSplit = sapply(as.character(mydata[['modifiedTime']]), function(x) strsplit(x, split = 'T')[[1]])
mydata$modifiedDate = as.Date(TimeSplit[1,])
TimeSplit = sapply(as.character(mydata[['datetimeOpened']]), function(x) strsplit(x, split = 'T')[[1]])
mydata$openedDate =as.Date(TimeSplit[1,])
mydata$datetimeOpened=NULL;mydata$modifiedTime=NULL;mydata$createdTime=NULL
summary(mydata)

# empty: issueType 844;issueSubType: 2773;accountName: 688
#         productType 839;contactName:758;partnerCollaborator:76704

modeName = names(which.max(table(mydata$productType)))
for (i in 1: nrow(mydata)){
  if(is.na(mydata$productType[i])) mydata$productType[i] = modeName
}
summary(mydata)

for (col in c('accountName','contactName')){
  mydata[[col]]=as.character(mydata[[col]])
  for (i in 1: nrow(mydata)){
    if(is.na(mydata[[col]][i])) mydata[[col]][i] = 'Missing Value'
  }
}

t2 = as.data.frame.matrix(table(mydata$accountName,mydata$productType)[,])
colnames(t2) <- c("account_1","account_2","account_3","account_4","account_5","account_6")
t2$accountName <- row.names(t2);
mydata <- merge(x = mydata, y = t2, by = "accountName", all.x = TRUE)
rm(t2)

t2 = as.data.frame.matrix(table(mydata$contactName,mydata$productType)[,])
colnames(t2) <- c("contact_1","contact_2","contact_3","contact_4","contact_5","contact_6")
t2$contactName <- row.names(t2)
mydata <- merge(x = mydata, y = t2, by = "contactName", all.x = TRUE)
rm(t2)

t3 = data.frame(prop.table(table(mydata$contactName)))
colnames(t3) <- c("contactName", "contactName_Freq")
mydata <- merge(x = mydata, y = t3, by = "contactName", all.x = TRUE)
mydata$contactName_logodds <- log(mydata$contactName_Freq)-log(1-mydata$contactName_Freq)
rm(t3)

t3 = data.frame(prop.table(table(mydata$accountName)))
colnames(t3) <- c("accountName", "accountName_Freq")
mydata <- merge(x = mydata, y = t3, by = "accountName", all.x = TRUE)
mydata$accountName_logodds <- log(mydata$accountName_Freq)-log(1-mydata$accountName_Freq)
rm(t3)



rm(list=ls()[c(1,2,4:11,13:21,23,26,28:43)])

Iwanttosee = data.frame(table(mydata$partnerCollaborator,mydata$label))
write.csv(Iwanttosee,'Iwanttosee.csv')


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

#imuting NA values with mode value
for (col in c('sf.issueType','sf.issueSubType','sf.productType')){
  modeName = names(which.max(table(data[[col]])))
  for (i in 1: nrow(data)){
    if(is.na(data[[col]][i])) data[[col]][i] = modeName
  }
}
summary(data)

data = cbind(data,tf.idf)

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


