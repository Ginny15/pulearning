links.1548 <- read.table("/Users/xhu/Documents/1548-links.txt")[,2]
library(solr)
library(pracma)
url <- 'http://localhost:8986/solr/LSH/afts'

# searching with LID, 71 of them cannot be found. The rest are all cases.
typeList = character()
for (i in 1:length(links.1548)){
  Q1= paste("LID:",shQuote(links.1548[i]))
  search.i = solr_search(q=Q1,base=url,fl = '*,TYPE',verbose=FALSE)$TYPE
  if(isempty(search.i)) typeList[i]= NA
  else typeList[i] = search.i
}
typeList = data.frame(typeList)
typeList$typeList = as.factor(typeList$typeList)
typeList = as.factor(typeList)
summary(typeList)

# SOLVED: NOT UNDATED. write to xlsx the 71 missing cases   
rowNum = numeric()
LID=character()
j=1
for (i in 1:length(links.1548)){
  Q1= paste("LID:",shQuote(links.1548[i]))
  search.i = solr_search(q=Q1,base=url,fl = '*,TYPE',verbose=FALSE)$TYPE
  if(isempty(search.i)) {
    LID[j] = shQuote(links.1548[i])
    rowNum[j] = i
    j=j+1
  }
}
na.List = data.frame(as.numeric(rowNum),as.character(LID))
library(WriteXLS)
WriteXLS(na.List,'naList.xlsx',row.names = FALSE)
 
# DBID of those existing cases wrt DBID-1548
rowNum = numeric()
DBID=numeric()
j=1
for (i in 1:length(links.1548)){
  Q1= paste("LID:",shQuote(links.1548[i]))
  search.i = solr_search(q=Q1,base=url,fl = '*,DBID',verbose=FALSE)$DBID
  if(isempty(search.i)==FALSE) {
    DBID[j] = search.i
    rowNum[j] = i
    j=j+1
  }
}
List.dbid = data.frame(as.numeric(rowNum),as.numeric(DBID))
 

search.1 = solr_search(q='DBID:2192871',base=url,fl = '*,[cached]',raw=TRUE,verbose=FALSE)
solr_parse(search.1,'list')$response$docs[[1]]$`cm:title` # "Installing an Alfresco license key"
cat(solr_parse(search.1,'list')$response$docs[[1]]$`cm:content`)


dbidList = numeric(0)
j=1
FP.q = paste('FINGERPRINT:',1548)
for (i in 1:length(links.1548)){
  LID.q= paste("LID:",shQuote(links.1548[i]))
  search.i = solr_search(q=paste(FP.q,'AND',LID.q), base=url,fl = '*',verbose=FALSE)
  if(isempty(search.i) == FALSE){
    dbidList[j] = search.i[3]
    j = j+1
  }
}

# search with:
#  q='FINGERPRINT:1548',fq = 'LID:"workspace://SpacesStore/00156515-080f-40dc-a17c-374190cbd026"'
# each time change fq, keep q unchanged
FP.q = paste('FINGERPRINT:',1548)
for (i in 1:length(links.1548)){
  LID.q= paste("LID:",shQuote(links.1548[i]))
  search.i = solr_search(q=FP.q,fq = LID.q, base=url,fl = '*',verbose=FALSE)
  if(isempty(search.i) == FALSE){
    dbidList[j] = search.i[3]
    j = j+1
  }
}
dbidDF = data.frame(DBID = as.numeric(dbidList)) 








result.search = solr_all(q='DBID:1548',base=url,fl = '*',raw = TRUE,verbose=FALSE)
solr_parse(result.search,'df')

result.search
cat(result.search[1])
qrl = paste('DBID:',dbidDF[1,])
for (i in 2:nrow(dbidDF)){
  qrl = paste(qrl,'OR',paste('DBID:',dbidDF[i,]))  
}
suffix.qrl = paste('FINGERPRINT:',1548)
solr_search(q=paste(qrl,'AND',suffix.qrl), base=url,fl = 'DBID,score,TYPE',verbose=FALSE,rows = 100)
 

solr_search(q=qrl, base=url,fl = 'DBID,score,TYPE',verbose=FALSE,rows = 100)

# workspace://SpacesStore/0049d344-3916-4ee4-867f-def7d2a742cd
solr_search(q='LID:"workspace://SpacesStore/0049d344-3916-4ee4-867f-def7d2a742cd"', base=url,fl = '*')


 


solr_all(q='content: solr',base=url,fl='*')

OWNER.FACET.2289 <- solr_facet(q='DBID:2289',base=url,fl='title',facet.field = 'OWNER',facet.limit = -1)$facet_fields$OWNER
colnames(OWNER.FACET.2289) <- c('owner_name','owner_count')
OWNER.FACET.2289

solr_all(q='DBID:2289',base=url,fl='*,[dv]',facet.field = 'SITE')

ccc = solr_search(q='DBID:2289',base=url,fl='*,ASPECT')$ASPECT
splitted = strsplit(ccc,',')[[1]]
length(splitted)  #6
splitted[6]
splitted.new = 
  for (item in splitted){
    if (substring(item,1,1) == '{') item = NA
  }

try1 = splitted[1]
try1[1]
substring(try1,1,1)
ccc = solr_search(q='DBID:2289',base=url,fl='*,PROPERTIES')$PROPERTIES
splitted = strsplit(ccc,',')[[1]]
length(splitted)  #6
splitted[6]






as.array(solr_search(q='DBID:2289',base=url,fl='*,ASPECT')$ASPECT[1])[1]

# 554903
# 1320357

# PNAME ????
# OWNER
# PROPERTY
# cm:created
# cm:modified
# cm:content.size
# cm:content.locale
# cm:content.mimetype
# sf:createdBy
# sf:type

solr_search(q='DBID:554903',base=url,fl='*,sf:type,sf:createdBy,cm:content.mimetype,cm:content.locale')


# default
# solr_all(q = "*:*", sort = NULL, start = 0, rows = NULL,
#          pageDoc = NULL, pageScore = NULL, fq = NULL, fl = NULL,
#          defType = NULL, timeAllowed = NULL, qt = NULL, wt = "json",
#          NOW = NULL, TZ = NULL, echoHandler = NULL, echoParams = NULL,
#          key = NULL, base = NULL, callopts = list(), raw = FALSE,
#          parsetype = "df", concat = ",", ..., verbose = TRUE)


nrow(solr_all(q = "FINGERPRINT:2289", sort = NULL, start = 0, rows = 10000,
              pageDoc = NULL, pageScore = NULL, fq = NULL, fl = '*,TYPE,score',
              defType = NULL, timeAllowed = NULL, qt = NULL, wt = "json",
              base = url))


solr_facet(q='*',base=url,fl='title',facet.field = 'sf:type',facet.limit = -1)$facet_fields

solr_facet(q='*',base=url,fl='title',facet.field = 'sf:issueType',facet.limit = -1)$facet_fields


#  count of documents with sf:issueType or sf:type
sum(sum(as.numeric(
  solr_facet(q='*',base=url,fl='title',facet.field = 'sf:issueType',facet.limit = -1)$facet_fields$'sf:issueType'$X2
)),    
sum(as.numeric(
  solr_facet(q='*',base=url,fl='title',facet.field = 'sf:type',facet.limit = -1)$facet_fields$'sf:type'$X2
))  ) # 96522


#  facet query by multiple facets
solr_facet(q='*',base=url,fl='title',facet.field = c('sf:type','sf:issueType'),facet.limit = -1)$facet_fields

nrow(solr_search(q = "sf:issueType:*", sort = NULL, start = 0, rows = 100000-1,
                 pageDoc = NULL, pageScore = NULL, fq = NULL, fl = '*',
                 defType = NULL, timeAllowed = NULL, qt = NULL, wt = "json",
                 base = url))


solr_facet(q='DBID:554903',base=url,fl='title',facet.field = 'SITE',facet.limit = -1)$facet_fields



solr_facet(q='DBID:554903',base=url,facet.field = 'OWNER',facet.limit = -1)$facet_fields



# 322579 ITEMS IN ALL

# TENANT  _DEFAULT_  342579
type.count <- solr_facet(q='*',base=url,fl='*',facet.field = 'TYPE')$facet_fields$TYPE
colnames(type.count) <- c('TYPE','TYPE.COUNT')

aspect.count <- solr_facet(q='*',base=url,fl='*',facet.field = 'ASPECT')$facet_fields$ASPECT
colnames(aspect.count) <- c('ASPECT','ASPECT.COUNT')

# ACLTXID is all 0, nonsense!

aclid.count = solr_facet(q='*',base=url,fl='*',facet.field = 'ACLID',facet.limit = -1)$facet_fields$ACLID
aclid.count = data.frame(aclid.count)
aclid.count$X2 = as.numeric(aclid.count$X2)
aclid.count = aclid.count[order(-aclid.count$X2),]
solr_all(q='*',base=url,fl='*',rows = 500000)
#       aclid   count
# 27     27      3
# 11     11      4
# 41     41      4
# 151   165     83
# 53     55    103
# 39     39    189
# 13     13    331
# 9       9    334
# 66     78    719
# 56     60   1308
# 60     68   4770
# 58     64 116763
# 62     72 216546

solr_facet(q='*',base=url,fl='*',facet.field = 'SITE',facet.limit = -1)$facet_fields$SITE


#            SITE counts
# 1 _REPOSITORY_   2369
# 2   jiraissues 116763
# 3 sfdcarticles   4770
# 4    sfdccases 216546
# 5        skype    720
# 6        swsdp    103
# 7       upload   1308

type.count <- as.data.frame(solr_facet(q='*',base=url,fl='*',facet.field = 'TYPE')$facet_fields$TYPE)
df <- NULL 
type.count$X1 <- as.character(type.count$X1)
type.count$X2 <- as.numeric(type.count$X2)

write.csv(type.count,'type.count.1.csv',row.names = FALSE)


links.1548[100,3]
