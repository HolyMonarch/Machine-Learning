
library(MASS)
library(mvtnorm)
source('Documents/fakedata.R',chdir=T) #import fakedata
z=c(1,2,-4)
set.seed(1)
data=fakedata(z,100) #generate matrix S and class lable vector y
S=data.frame(data[1])
y=data.frame(data[2])

#a function classify
classify=function(x,y){
	y=sign(as.matrix(x)%*%y)
	print(y)
}
classify(S,z)
sgn=classify(S,z)
I=(sgn!=y);as.numeric(I)
as.matrix(S)%*%z

#b function perceptron
perceptron=function(S,y,z=c(1,1,-1)){
	sgn=sign(as.matrix(S)%*%z)
	I=as.numeric(sgn!=y)
	c=sum(I*abs(as.matrix(S)%*%z))
	g.c=c(sum((I*(sgn)*S[,1])),sum((I*(sgn)*S[,2])),sum((I*(sgn)*S[,3])))
	i=1
	z.history=c()
	while(c>0.000001){
		z.history=rbind(z.history,z)
		z=z-(1/i)*g.c
		sgn=sign(as.matrix(S)%*%z)
		I=as.numeric(sgn!=y)
		c=sum(I*abs(as.matrix(S)%*%z))
		g.c=c(sum((I*(sgn)*S[,1])),sum((I*(sgn)*S[,2])),sum((I*(sgn)*S[,3])))
		i=i+1
	}
	z.history
}
z.history=perceptron(S,y,z=c(1,2,-9));z.history
z.history[dim(z.history)[1],]


#c generate traning set
z2=c(2,3,-4)
set.seed(9)
data1=fakedata(z2,100)
S2=data.frame(data1[1])
y2=data.frame(data1[2])
z.history2=perceptron(S2,y2,z=c(2,3,-8));z.history2
z.new=z.history2[dim(z.history2)[1],] #perceptron

#generate testing set
set.seed(12345)
data2=fakedata(z2,100)
S3=data.frame(data2[1])
y3=data.frame(data2[2])
sgn2=classify(S3,z.new);sgn2
mean(sgn2!=y3) check whether it is classified correctly



class(S3[,1])
typeof(S3[,1])
typeof(x)

y33=as.matrix(y3)
library(ggplot2)
#convert the data into their corresponding 2D representation
S2.plot=data.frame(S21=S2.plot[,1],S22=S2.plot[,2],y=as.matrix(y2))
S3.plot=data.frame(S31=S3.plot[,1],S32=S3.plot[,2],y=as.matrix(y3))
S3.plot=S3[,1:2]
#The test data set and the classifier hyperplane
names(S3)
p2=ggplot(data=S3.plot,aes(x=S31,y=S32,colour=y))+geom_point()

p2=p2+geom_abline(intercept=-z.new[3]/z.new[2],slope=-z.new[1]/z.new[2]);p2

plot(S3.plot[,1],S3.plot[,2],col=y33+2)
vd=norm(as.matrix(z.new[1:2]));vd
abline(a=-z.new[3]/z.new[2],b=-z.new[1]/z.new[2],col="red")
z.history2

#Trajectory of the algorithm by visualizing Z_history
plot(S2.plot[,1],S2.plot[,2],col=as.matrix(y2)+2)
for (i in 1:nrow(z.history2)){
	if (i<nrow(z.history2))
	abline(a=-z.history2[i,3]/z.history2[i,2],b=-z.history2[i,1]/z.history2[i,2],lty=3)
	else if (i==nrow(z.history2))
	abline(a=-z.history2[i,3]/z.history2[i,2],b=-z.history2[i,1]/z.history2[i,2],lty=1,col="red",lwd=2)
}


p=ggplot(data=S2.plot,aes(x=S21,y=S22,colour=y))+geom_point()
for (i in 1:nrow(z.history2)){
	if (i<nrow(z.history2))
	p=p+geom_abline(intercept=-z.history2[i,3]/z.history2[i,2],slope=-z.history2[i,1]/z.history2[i,2],lty=2)
	else if (i==nrow(z.history2))
	p=p+geom_abline(intercept=-z.history2[i,3]/z.history2[i,2],slope=-z.history2[i,1]/z.history2[i,2],colour="red",size=1)
}
p
