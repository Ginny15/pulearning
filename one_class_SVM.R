suppressWarnings(suppressMessages(library(oneClass)))
suppressWarnings(suppressMessages(library(doParallel)))
suppressWarnings(suppressMessages(library(raster)))
suppressWarnings(suppressMessages(library(RColorBrewer)))

cl <- makeCluster(detectCores()-1)
doParallel:::registerDoParallel(cl)

data.x = dummy.data.frame(data.1[,c(2:10)])
data.y = data.1$label
trainIDX = sample(nrow(data.x),nrow(data.x)*0.75,replace = FALSE)
library(oneClass)
seed <- 123456
train.x <- data.x[trainIDX,]
train.y <- puFactor(data.y[trainIDX], positive='Y')
set.seed(seed)
train.index <- createFolds(train.y, k=3, returnTrain=TRUE)
set.seed(seed)
test.x <- data.x[-trainIDX,]
test.y <- puFactor(data.y[-trainIDX], positive='Y')
 
ocsvm.fit <- trainOcc(x=train.x.bsvm, y=train.y.bsvm, method="ocsvm", index=train.index)ocsvm.fit.def <- ocsvm.fit # store for comparison
ocsvm.pred <- predict(ocsvm.fit, test.x)
ocsvm.bin <- ocsvm.pred>0
ocsvm.bin = puFactor(ocsvm.bin, positive='TRUE')
confusionMatrix(ocsvm.bin,test.y)$table
# Prediction    un   pos
# un  21953   426
# pos   524   453

options(repr.plot.width=14, repr.plot.height=3.5)
par(mfrow=c(1, 3))
hist(ocsvm.fit, ocsvm.pred, th=0, noWarnRasHist=TRUE)
plot(ocsvm.pred, col=brewer.pal(9, "BuPu"))
plot(ocsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])
options(repr.plot.width=6, repr.plot.height=5)
featurespace(bsvm.fit, th=0)

options(repr.plot.width=4, repr.plot.height=3)
trellis.par.set(caretTheme()) # nice colors from caret
plot(ocsvm.fit, plotType="level") # see ?plot.train for other plot types

length(train.y)
newtrainIDX = sample(nrow(train.x),7000,replace = FALSE)
train.x.bsvm=train.x[newtrainIDX,];train.y.bsvm=train.y[newtrainIDX]
bsvm.fit <- trainOcc(x=train.x.bsvm, y=train.y.bsvm, index=train.index)
bsvm.fit.def <- bsvm.fit # store for comparison
bsvm.pred <- predict(bsvm.fit, test.x)
bsvm.bin <- bsvm.pred>0
bsvm.bin = puFactor(bsvm.bin, positive='TRUE')
confusionMatrix(bsvm.bin,test.y)$table
# Prediction    un   pos
# un  22323   447
# pos   154   432

head(ocsvm.fit$results)

options(repr.plot.width=14, repr.plot.height=3.5)
par(mfrow=c(1, 3))
hist(ocsvm.fit, ocsvm.pred, th=0, noWarnRasHist=TRUE)
plot(ocsvm.pred, col=brewer.pal(9, "RdBu"))
plot(ocsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])


options(repr.plot.width=6, repr.plot.height=5)
featurespace(ocsvm.fit, th=0)

tuneGrid <- expand.grid( sigma = seq(.1, 2, .1), nu = seq(.05, .5, .05) )
ocsvm.fit <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", tuneGrid=tuneGrid, index=train.index)
ocsvm.pred <- predict(ocsvm.fit, bananas$x)

options(repr.plot.width=4, repr.plot.height=3)
trellis.par.set(caretTheme()) # nice colors from caret
plot(ocsvm.fit.def, plotType="level") # see ?plot.train for other plot types
plot(ocsvm.fit, plotType="level")
