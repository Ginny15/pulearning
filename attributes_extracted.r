# attributes extracted #
result.search = solr_all(q='DBID:1548',base=url,fl = '*,[cached]',raw = TRUE,verbose=FALSE)
solr_parse(result.search,'list')$response$docs[[1]]$DBID     #1548
solr_parse(result.search,'list')$response$docs[[1]]$LID      # "workspace://SpacesStore/7df1ddcc-328c-4ea4-bea0-277d11526718"
solr_parse(result.search,'list')$response$docs[[1]]$INTXID  # 1660410
solr_parse(result.search,'list')$response$docs[[1]]$DOC_TYPE  # "Node"
solr_parse(result.search,'list')$response$docs[[1]]$ACLID # 68
solr_parse(result.search,'list')$response$docs[[1]]$TYPE # "{http://www.alfresco.com/model/actsearch2/salesforce/1.0}ArticleFAQ"
solr_parse(result.search,'list')$response$docs[[1]]$ASPECT   # 6 in all
# [1] "{http://www.alfresco.org/model/content/1.0}auditable"                     
# [2] "{http://www.alfresco.org/model/system/1.0}referenceable"                  
# [3] "{http://www.alfresco.org/model/system/1.0}localized"                      
# [4] "{http://www.alfresco.org/model/content/1.0}versionable"                   
# [5] "{http://www.alfresco.com/model/actsearch2/salesforce/1.0}hasVideoTutorial"
# [6] "{http://www.alfresco.org/model/content/1.0}titled"   
solr_parse(result.search,'list')$response$docs[[1]]$TENANT  # "_DEFAULT_"
solr_parse(result.search,'list')$response$docs[[1]]$SITE  # "sfdcarticles"
solr_parse(result.search,'list')$response$docs[[1]]$PNAME 
# [1] "0/006"                                                     "1/000/006"                                                
# [3] "2/documentLibrary/000/006"                                 "3/sfdcarticles/documentLibrary/000/006"                   
# [5] "4/Sites/sfdcarticles/documentLibrary/000/006"              "5/Company Home/Sites/sfdcarticles/documentLibrary/000/006"
# [7] "F/Company Home/Sites/sfdcarticles/documentLibrary/000/006"
solr_parse(result.search,'list')$response$docs[[1]]$NPATH[3]    #"2/Company Home/Sites/sfdcarticles"
solr_parse(result.search,'list')$response$docs[[1]]$OWNER     # "admin"
solr_parse(result.search,'list')$response$docs[[1]]$PRIMARYPARENT  # "workspace://SpacesStore/c3632060-12ab-4955-aab3-e1d99864fe31"
solr_parse(result.search,'list')$response$docs[[1]]$PRIMARYASSOCTYPEQNAME  # "{http://www.alfresco.org/model/content/1.0}contains"

solr_parse(result.search,'list')$response$docs[[1]]$`sf:createdBy`   # "Claudia Belardo"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:created`  # "2015-12-11T18:26:25.308Z"
solr_parse(result.search,'list')$response$docs[[1]]$`sf:lastModifiedBy` # "Kimberly Holyszko"
solr_parse(result.search,'list')$response$docs[[1]]$`sf:dateTimeArticleOpened`  # "2016-03-14T16:52:48.000Z"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:description`
# "This article provides information about how to install an Alfresco One or Enterprise license"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:creator` # "admin"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:name` # "000006082.html"
solr_parse(result.search,'list')$response$docs[[1]]$`sys:store-identifier` # "SpacesStore"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.docid` # 1064790
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.size` # 18497

solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.locale` # "en_US"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.mimetype` # "text/html"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.encoding` # "UTF-8"
solr_parse(result.search,'list')$response$docs[[1]]$`sf:publishingStatus` # "Online"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:versionLabel` # "1.0"
solr_parse(result.search,'list')$response$docs[[1]]$`sf:language`  # "en_US"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:autoVersion` # "false"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:initialVersion` # "true"

solr_parse(result.search,'list')$response$docs[[1]]$`act2:type` # "SalesForce Article"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:autoVersionOnUpdateProps` # "false"
solr_parse(result.search,'list')$response$docs[[1]]$`sf:validationStatus` # "Validated Externally"
solr_parse(result.search,'list')$response$docs[[1]]$`sys:store-protocol` # "workspace"
solr_parse(result.search,'list')$response$docs[[1]]$`sys:locale` # "en_US_"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:modifier`  # "admin"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:modified` #"2016-03-14T17:00:12.231Z"
solr_parse(result.search,'list')$response$docs[[1]]$`sf:type`  # "Customer_Care"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.tr_status`  #"org.alfresco.solr.client.SOLRAPIClient$SolrApiContentStatus:OK"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.tr_time`   #17
solr_parse(result.search,'list')$response$docs[[1]]$`cm:content.tr_ex`   #NULL
solr_parse(result.search,'list')$response$docs[[1]]$FTSSTATUS  # "Clean"
solr_parse(result.search,'list')$response$docs[[1]]$`cm:title` # "Installing an Alfresco license key"
cat(solr_parse(result.search,'list')$response$docs[[1]]$`cm:content`)