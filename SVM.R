#import data
data=read.table('Documents/uspsdata/uspsdata.txt',header=T)
y=read.table("Documents/uspsdata/uspscl.txt",header=T)
names(data)
length(data)
nrow(data)

set.seed(1)

#randomly select test set and training set
s=sort(sample(nrow(y),0.8*nrow(y)));s
train.x=as.matrix(data.frame(data[c(s),],row.names=c(1:(0.8*nrow(y)))))
train.y=as.numeric(as.matrix(data.frame(y[c(s),],row.names=c(1:(0.8*nrow(y))))))
test.x=as.matrix(data.frame(data[-c(s),],row.names=c(1:(round(nrow(y)-0.8*nrow(y))))))
test.y=as.numeric(as.matrix(data.frame(y[-c(s),],row.names=c(1:(round(nrow(y)-0.8*nrow(y)))))))
length(c(s))
round(39.8)
traindata=data.frame(x=train.x,y=as.factor(train.y))
testdata=data.frame(x=test.x,y=as.factor(test.y))

#train linear SVM
library(e1071)
library(ggplot2)
svmlinear=svm(y~.,data=traindata,kernel="linear",cost=10,scale=T)
svmlinear$index
summary(svmlinear)

#cross-validate the margin parameter
tune.out1=tune(svm,y~.,data=traindata,kernel="linear",ranges=list(cost=c(0.0001,0.0005,0.001,0.005,0.01,0.1)))
summary(tune.out1)
tune.out1$performance$error

#a function of the margin parameter in the linear case
error.l=tune.out1$performance$error
cost.l=tune.out1$performance$cost
qplot(cost.l,error.l,log='x',geom="line",main="Misclassification Rate of the linear SVM")

#the misclassification rate of the linear SVM
bestmodel1=tune.out1$best.model
summary(bestmodel1)
predict(bestmodel1,testdata)
mean(test.y!=predict(bestmodel1,testdata))

#train a SVM with soft margin and RBF kernel
svmradial=svm(y~.,data=traindata,kernel="radial",cost=1)
summary(svmradial)

#cross-validate both the soft-margin parameter and the kernel #bandwidth
tune.out2=tune(svm,y~.,data=traindata,kernel="radial",ranges=list(cost=c(0.01,0.05,0.1,0.5,1,10,100,1000,10000),gamma=c(0.0001,0.0005,0.001,0.005,0.01,0.1,0.5,1)))
summary(tune.out2)

#a function of the margin parameter and the kernel bandwidth in th e non-linear case

cost.r=tune.out2$performance$cost
gamma.r=tune.out2$performance$gamma
error.r=tune.out2$performance$error
dat=data.frame(cbind(cost.r,gamma.r,error.r))
dat[1:12,]
ggplot(dat,aes(x=log(cost.r),y=error.r,group=gamma.r,col=gamma.r))+geom_line()

#3D 
library(rgl)
plot3d(cost.r,gamma.r,error.r,log="xy")

#misclassification rate of non-linear case
bestmodel2=tune.out2$best.model
summary(bestmodel2)
mean(test.y!=predict(bestmodel2,testdata))
