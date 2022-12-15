library(glmnet)
library(pROC)

train<-read.table("train.txt",header=T)
test<-read.table("test.txt",header=T)
train$Sample<-factor(train$Sample)
test$Sample<-factor(test$Sample)
x <- as.matrix(train[, 2:10])
y <- train[, 1]
fitCV <- cv.glmnet(x, y, family = "binomial",type.measure = "deviance")
plot(fitCV) 
coef.min = coef(fitCV, s = "lambda.min")
coef.min.out = coef.min[which(coef.min!=0),]
coef.min.out<-round(coef.min.out,4)
coef.min.out2<-matrix(coef.min.out,length(coef.min.out),1)
rownames(coef.min.out2)<-names(coef.min.out)
colnames(coef.min.out2)<-c('coef')
coef.min.out3<- coef.min.out2[-1,]
coef.min.out3[order(abs(coef.min.out3),decreasing=TRUE)]
fit4<-glmnet(x,y,family='binomial')
plot(fit4,xvar='lambda', label = TRUE)