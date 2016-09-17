
#### one class svm
suppressWarnings(suppressMessages(library(oneClass)))
suppressWarnings(suppressMessages(library(doParallel)))
suppressWarnings(suppressMessages(library(raster)))
suppressWarnings(suppressMessages(library(RColorBrewer)))

cl <- makeCluster(detectCores()-1)
doParallel:::registerDoParallel(cl)

data.x = dummy.data.frame(data[,c(2:9,11)])
data.y = data$label
data = cbind(data.x,data.y)
trainIDX = sample(nrow(data),nrow(data)*0.75,replace = FALSE)
train = data[trainIDX,];test = data[-trainIDX,]
train.pos = train[which(train$data.y=='Y'),] 
train.un = train[-which(train$data.y=='Y'),]  
train.un.subset = train.un[sample(nrow(train.un),5000,replace = FALSE),]
train = rbind(train.pos,train.un.subset)
train = train[shuffle(nrow(train)),]
library(oneClass)
seed <- 123456
train.x <- train[,-215];
train.y <- puFactor(train[,215], positive='Y')
set.seed(seed)
train.index <- createFolds(train.y, k=3, returnTrain=TRUE)
set.seed(seed)
test.x <- test[,-215]
test.y <- puFactor(test[,215], positive='Y')
sigma = 0.1, cNeg = 4 and cMultiplier = 16384
tuneGrid <- expand.grid( sigma =seq(0.01, 0.1), cNeg = seq(1, 2, 4) ,cMultiplier=2056)
bsvm.fit <- trainOcc(x=train.x, y=train.y, index=train.index,tuneGrid=tuneGrid)
bsvm.fit.def <- bsvm.fit # store for comparison
bsvm.pred <- predict(bsvm.fit, test.x)
bsvm.bin <- bsvm.pred>0
bsvm.bin = puFactor(bsvm.bin, positive='TRUE')
confusionMatrix(bsvm.bin,test.y)$table
