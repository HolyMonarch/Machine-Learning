X=as.matrix((read.table("Documents/uspsdata/uspsdata.txt",header=F)))
y=as.matrix((read.table("Documents/uspsdata/uspscl.txt",header=F)))
#dim(X)
#dim(y)
head(X)
AdaBoost=function(X,y,B){
	w=matrix(nrow=nrow(X))
	w[,1]=rep(1/nrow(X),nrow(X))
	pars=matrix(ncol=4)
	E=c()
	alpha=c()
	allPars=matrix(ncol=4)

	for (b in 1:B){
		pars=train(X,w[,b],y)
		label=classify(X,pars)
		I=(label!= y)
		E[b]=(as.vector(I)%*%w[,b])/sum(w[,b])
		alpha[b]=log((1-E[b])/E[b])
		w=cbind(w,w[,b]*exp(alpha[b]*I))
		allPars=rbind(allPars,pars)
		}
	allPars=allPars[2:nrow(allPars),]
	
	c(list(alpha=alpha,allPars=allPars))
	
	}


agg_class=function(X,alpha,allPars){
	lab=matrix(rep(0,nrow(X)))
	B=(dim(as.matrix(allPars))[2])
	for(b in 1:B){
		lab=lab+as.matrix(classify(X,allPars[b,])*alpha[b])
		}
	sign(lab)
}


classify=function(X,pars){
 	label=matrix()
 	for (i in 1:nrow(X)){
 		if (X[i,pars[1]]<=pars[2]){
 			label[i]=pars[3]
 		}
 		if(X[i,pars[1]]>pars[2]){
 			label[i]=pars[4]
 		}
 	}
 	label
 }



train=function(X,w,y){
	tj=matrix(nrow=ncol(X)) #n tj
	Qj=matrix(nrow=ncol(X)) #n cost of split
	m=matrix(ncol=2)  #class
	for (j in 1:ncol(X)){
		label=matrix(nrow=nrow(X))
		datj=as.matrix(cbind(X[,j],y))
		sortdatj=datj[order(datj[,1]),]
		if (abs(min(cumsum(sortdatj[,2])))>abs(max(cumsum(sortdatj[,2])))){
			label.less=-1
			label.more=1
			theta=sortdatj[which.min(cumsum(sortdatj[,2])),1]
		}
		if (abs(min(cumsum(sortdatj[,2])))<=abs(max(cumsum(sortdatj[,2])))){
			label.less=1
			label.more=-1
			theta=sortdatj[which.max(cumsum(sortdatj[,2])),1]
		}
		for (i in 1:nrow(X)){
			if (X[i,j]<=theta){
				label[i]=label.less
			}
			if(X[i,j]>theta){
				label[i]=label.more
			}
			}
		error=(as.vector(label!=y)%*%w)/sum(w)
		
		tj[j]=theta
		Qj[j]=error
		m=rbind(m,c(label.less,label.more))
	}
	m=m[2:257,]
	j.min=which.min(Qj) # best splitting of axis
	tj.min=tj[j.min] # best splitting points 
	c(j.min,tj.min,m[j.min,])
}

cross_validation(X,y,1)
cross_validation=function(X,y,B){
	error.train=c()
	error.test=c()
	index=sample(1:200)
	for (i in 1:5){
		s=index[((i-1)*40+1):(i*40)]
 		train.X=X[-s,]
 		train.y=y[-s,]
 		test.X=X[s,]
 		test.y=y[s,]
 	
 		classifier=AdaBoost(train.X,train.y,B)
 		alpha=classifier$alpha
 		allPars=classifier$allPars
		error.train[i]=mean(agg_class(train.X,alpha,allPars)!=train.y)
		error.test[i]=mean(agg_class(test.X,alpha,allPars)!=test.y)
 	}
 	error.train=mean(error.train)
 	error.test=mean(error.test)
 	return(c(error.train,error.test))
 }


 
test.error=c()
train.error=c()
for (b in 2:30){
	error=cross_validation(X,y,b)
 	test.error[b]=error[2]
 	train.error[b]=error[1]
 }
test.error
train.error

data=data.frame(b=1:30,train.error=train.error,test.error=test.error)
library(ggplot2)
p=ggplot(data=data,aes(x=b,y=test.error))+geom_line()
p=p+geom_line(data=data,aes(x=b,y=train.error),colour="red");p
