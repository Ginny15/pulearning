## preparations ####
LID.1548 = read.table("/Users/xhu/Documents/1548-links.txt")[,2]
library(solr);library(pracma);library(permute);library(data.table);library(caret);library(dummies);library(BBmisc)
url = 'http://localhost:8987/solr/LSH/afts'

score.search = solr_all(q= 'FINGERPRINT:1548', base=url, fl='*,DBID,score', verbose=FALSE, rows=99999)
score.df = data.frame(DBID = score.search$DBID, score = score.search$score)
data = merge(x = data.1[,c(1:10,24)], y = score.df, by = "DBID", all.x = TRUE)
data$score[which(is.na(data$score))] = 0
data$Size <- NULL;
tf.idf.sum = rowSums(tf.idf)
data = cbind(data,tf.idf.sum)
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
  fit.xgb  <- train(label~., data = PsUs[,-1], method='xgbTree',trControl=control)
  pred.Us = predict(fit.xgb,Us[,-1],type = 'prob')$Y
  pred.S = predict(fit.xgb,S[,-1],type='prob')$Y
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
data.step.2 = rbind(RN,P); data.step.2=data.step.2[shuffle(nrow(data.step.2)),]
fit.xgb  <- train(label~., data = data.step.2[,-1], method='xgbTree',trControl=control)
confusionMatrix(predict(fit.xgb,test[,-1]),test$label)
# Reference
# Prediction     N     Y
# N 17758     0
# Y  4705   893
confusionMatrix(predict(fit.xgb,train[,-1]),train$label)
# Reference
# Prediction     N     Y
# N 53079     4
# Y 14308  2674

prob.r = 892/893
prob.fx.1 = length(pred.test[which(pred.test=='Y')])/length(pred.test)
metric = prob.r^2/prob.fx.1 #7.04


kmeans()
write.csv(cbind(test,data.frame(test,pred=predict(fit.xgb,test[,-1]),prob = predict(fit.xgb,test[,-1],type = 'prob'))),'results_all_catego.0817.testset.csv')


 
