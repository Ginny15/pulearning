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

query.need = " 244787 1442699 1958389 41817 43615 34584 53845 54060 1185590 2332739"
query = paste('TYPE:"{http://www.alfresco.com/model/actsearch2/salesforce/1.0}Case" AND -DBID:(',query.need,")")
result.search = solr_all(q= query, base=url, fl='*,[cached]', raw=TRUE, verbose=FALSE, rows=99999)
cachedInfo = solr_parse(result.search,'list')$response$docs
rm(result.search);gc()

###  extracting features ####
#################################################################
rm(NER.LIST,DBID)
NER.LIST = vector('list',93434);DBID=rep(0,length(cachedInfo)) 
for(i in 1:length(cachedInfo)){
  cachedInfo.i = cachedInfo[[i]]
  cached.i.ner = cachedInfo.i$ner   # several items in it
  for (j in 1:length(cached.i.ner)) {
    # originally 93434 cases, after removing cardinal, date and time, 13 cases have no entities, drop them.
    if(strsplit(cached.i.ner[j],split = ":")[[1]][1] %in% c('CARDINAL','DATE','TIME')) cached.i.ner[j] <- NA
  }
  cached.i.ner = cached.i.ner[!is.na(cached.i.ner)]
  NER.LIST[[i]]=cached.i.ner
  DBID[i] = cachedInfo.i$DBID}
rm(cachedInfo);gc()
#NER.LIST.1 = NER.LIST[which(DBID %in% DBID.1548)] 
NERwithDDBID = data.frame(DBID = rep(DBID,sapply(NER.LIST,function(x) length(x))),
                                     entity = unlist(NER.LIST))
library(dplyr)
temp = NERwithDDBID %>%  group_by(entity) %>%  mutate(count.entity = n()) 
temp = temp %>%  group_by(entity,DBID) %>%  mutate(count.entity.dbid= n()) 
temp = temp %>%  group_by(DBID) %>%  mutate(count.dbid= n()) 
dbid.count = unique(temp[,c(1,5)])

# # ####
# nerSizeDistribution = data.frame(nerSize = 1: max(dbid.count$count.dbid),
#                                  count = sapply(1: max(dbid.count$count.dbid),
#                                                 function(i) nrow(dbid.count[which(dbid.count$count.dbid == i),])))
# cachedInfo.i = solr_all(q = "DBID:136093",base=url, fl='*,[cached]', raw=TRUE, verbose=FALSE, rows=99999)
# cachedInfo.i = solr_parse(cachedInfo.i,'list')$response$docs
# ner.136093  = cachedInfo.i[[1]]$ner
# length(ner.136093[which(sapply(ner.136093,function(x) !strsplit(x,':')[[1]][1] %in% c('CARDINAL','DATE','TIME')))])
# content.136093  = cachedInfo.i[[1]]$`cm:content`
# write(content.136093)
# temp = unique(temp)

# ####
NER.LIST = NER.LIST[sapply(NER.LIST, function(x) !isempty(x))]
 
# cases with no entity
dbid.no.entity.idx = which(sapply(NER.LIST, function(x) isempty(x)))  # 13 is empty
dbid.notempty.entity = DBID[-dbid.no.entity.idx ]

entropy.LIST = sapply(NER.LIST, function(x) rep(0,length(x)))
ner.LIST = sapply(NER.LIST, function(x) character(length(x)))
for (i in 1: length(NER.LIST)){
  NER_i = NER.LIST[[i]]
  tf.table = data.frame(table(NER_i))
  Nij = length(NER_i)
  V_f_ij = tf.table$Freq / (Nij ^(tf.table$Freq))
  entropy.LIST[[i]] = V_f_ij / sum(V_f_ij)
  ner.LIST[[i]] =  tf.table$NER_i
}

ner.term.frequency= sapply(NER.LIST, function(x) rep(0,length(x)))
for (i in 1: length(NER.LIST)){
  NER_i = NER.LIST[[i]]
  tf.table = data.frame(table(NER_i))
  ner.term.frequency[[i]] = tf.table$Freq
}

ner.LIST = sapply(ner.LIST, function(x) as.character(x))
entities = unique(temp$entity)
entity = rep(0,length(entities))
for(j in 1:length(entities)){
  w_j = entities[j]
  for (i in 1: length(ner.LIST)){
    if(w_j %in% ner.LIST[[i]]){
      entity[j] = entity[j] - entropy.LIST[[i]][which(ner.LIST[[i]] == w_j)]/log(entropy.LIST[[i]][which(ner.LIST[[i]]==w_j)])
    }
  }
}
max(entity)*0.5

entity[which(entity>10)]  # count = 98

usefulEntity = as.character(entities[which(entity > 10)])  # top 98 entities

NER.DF.NEW

newdf = data.frame(entity = entities,entropy = entity,count = unique(temp[,2:3]))

NERwithDDBID$count = temp$count
nrow(NERwithDDBID[which(NERwithDDBID$count<500),])
temp = NERwithDDBID[,2:3]
temp = unique(temp)
NER.LIST.1.unlist = unlist(NER.LIST.1)
length(unique(NER.LIST.1.unlist))
df = data.frame(docID = rep(1:length(NER.LIST.1), sapply(NER.LIST, function(x) length(x))), entityName = unlist(NER.LIST))

library(dplyr)
temp = NERwithDDBID %>%  group_by(entity) %>%  mutate(count = n()) 
NERwithDDBID$count = temp$count
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


df.2 = dummy(df$entityName)

NER.LIST.1

rm(NER.LIST.REP)
 
rownames(NER.LIST.REP)
