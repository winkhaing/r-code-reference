#
install.packages("dummies")
rm(list=ls())
set.seed(123)
n<-500
rr <- round(abs(rnorm(n,30,10)))
hr <- round(abs(rnorm(n,90,20)))
crp <- round(abs(rnorm(n,150,80)))
library(dummies)
beta0=-7; betarr=0.05
betahr=0.02; betacrp=0.02
linpred <- cbind(1, rr,hr,crp) %*%  c(beta0,betarr,betahr,betacrp)
pi <- exp(linpred) / (1 + exp(linpred))
sepsis.tag <- rbinom(n=n, size=1, prob=pi)
dt <- data.frame(rr,hr,crp,sepsis.tag)


ntbft<-function(data,outcome,frm=NULL, 
                exterdt=NULL,pred=NULL,xstart=0.01,  
                xstop=0.99,step=0.01,type="treated") {  
  pt<-seq(from=xstart,to=xstop,by=step)  
  lpt<-length(pt)  
  if(type=="treated") coef<-cbind(rep(1,lpt),rep(0,lpt))  
  if(type=="untreated") coef<-cbind(rep(0,lpt),rep(1,lpt))  
  if(type=="overall") coef<-cbind(rep(1,lpt),rep(1,lpt))  
  if(type=="adapt") coef<-cbind(1-pt,pt)  
  response<-as.vector(t(data[outcome]))  
  if(is.data.frame(exterdt)) response<-as.vector(t(exterdt[outcome]))  
  event.rate<-mean(response)  
  nball<-event.rate-(1-event.rate)*pt/(1-pt)  
  nbnone<-1-event.rate-event.rate*(1-pt)/pt  
  
  if(is.null(pred)){  
    model<-glm(frm,data=data,family=binomial("logit"))  
    pred<-model$fitted.values  
    if(is.data.frame(exterdt))  
      pred<-predict(model,newdata=exterdt,type="response")  
    }  
# pred and response should be of the same length  
  N<-length(pred)  
  nbt<-rep(NA,lpt)  
  nbu<-rep(NA,lpt)  
  for(t in 1:lpt){  
    tp<-sum(pred>=pt[t] & response==1)  
    fp<-sum(pred>=pt[t] & response==0)  
    fn<-sum(pred<pt[t] & response==1)  
    tn<-sum(pred<pt[t] & response==0)  
    nbt[t]<-tp/N-fp/N*(pt[t]/(1-pt[t]))  
    nbu[t]<-tn/N-fn/N*((1-pt[t])/pt[t])  
  }
  nb<-data.frame(pt)  
  names(nb)<-"threshold"  
  nb["all"]<-coef[,1]*nball  
  nb["none"]<-coef[,2]*nbnone  
  nb["pred"]<-coef[,1]*nbt+coef[,2]*nbu  
  return(nb)  
}

outcome <- "sepsis.tag"  
model1 <- sepsis.tag~rr+hr  
model2 <- sepsis.tag~rr+hr+crp  
xstart <- 0.01; xstop <- 0.99; step <- 0.01  
type <- "treated"  
nb.simple<-ntbft(data=dt, outcome=outcome,  frm = model1, xstart=xstart, xstop=xstop,  step=step, type=type)  
nb.full <- ntbft(data=dt, outcome=outcome,  frm = model2, xstart=xstart,  xstop=xstop, step=step, type=type)


plot.ntbft<-function(nb,nolines=2:dim(nb)[2],  
                     nobands=NULL,ymin=-0.1,  
                     ymax=max(nb[,c(nolines,nobands)],na.rm=T),  
                     legpos=c(0.9,0.8)) {  
                        ylow<-nb[,1]  
                        yup<-nb[,1]  
                        if(!is.null(nobands)){  
                          ylow<-nb[,min(nobands)]  
                          yup<-nb[,max(nobands)]  
                          }  
                        nb.melt<-melt(nb[,c(1,nolines)],  
                                      id.vars="threshold",  
                                      value.name="Netbenefit",variable.name="Models")  
                        print(ggplot(nb.melt)+  
                                theme(legend.position=legpos)+  
                                geom_line(aes(x=threshold,y=Netbenefit,  
                                              colour=Models,linetype=Models))+  
                                geom_ribbon(data=nb,aes(x=threshold,  
                                                        ymin=ylow,ymax=yup),  
                                            linetype=2,alpha=0.2)+  
                                scale_y_continuous(limits=c(ymin,ymax))+
                                xlab("Threshold probability")+
                                ylab("Net benefit"))  
}

library(ggplot2)
library(reshape2)
nb <- cbind(nb.simple, pred.full=nb.full$pred)
names(nb)[names(nb)=="pred"] <- "pred.simple"
plot.ntbft(nb)



diffnet<-function(data,ii,outcome,frm,  
                  xstart=0.01,xstop=0.99,  
                  step=0.01,type="treated") {  
  dd<-data[ii,]  
  nb<-ntbft(data=dd,outcome=outcome,frm=frm,  
            xstart=xstart,xstop=xstop,  
            step=step,type=type)  
  nb0<-ntbft(data=dd,outcome=outcome,frm=frm,  
             exterdt=data,xstart=xstart,  
             xstop=xstop,step=step,type=type)  
  diff<-nb$pred-nb0$pred  
  cat(".")  
  return(diff)  
}

library(boot)  
set.seed(124)
R<-500  
rstls<-boot(data=dt,statistic=diffnet,  
            R=R,outcome=outcome,frm=model1,  
            xstart=xstart,xstop=xstop,  
            step=step,type=type)  
nb.simple<-cbind(nb.simple,  
                 pred.bootc=nb.simple$pred-rowMeans(t(rstls$t)))  

plot.ntbft(nb.simple,nolines=2:5)



ntbft.cv<-function(data,outcome,frm,
                   n_folds=10,R=200,xstart=0.01,
                   xstop=0.99,step=0.01,type="treated"){  
  cv<-NULL  
  for(i in 1:R){  
    n_train<-nrow(data)  
    folds_i<-sample(rep(1:n_folds,length.out=n_train))  
    pred<-rep(NA,n_train)
    for(k in 1:n_folds){  
      test_i<-which(folds_i==k)  
      train_dt<-data[-test_i,]  
      test_dt<-data[test_i,]  
      model<-glm(frm,data=train_dt,family=binomial("logit"))  
      pred[test_i]<-predict(model,newdata=test_dt,type="response")  
    }  
    cv<-cbind(cv,ntbft(data=data,outcome=outcome,  
                       pred=pred,xstart=xstart,xstop=xstop,  
                       step=step,type=type)$pred)  
  }  
  return(rowMeans(cv))  
}



set.seed(125)  
n_folds<-10  
R<-200  
cv<-ntbft.cv(data=dt,outcome=outcome,  
             frm=model1,n_folds=n_folds,R=R,  
             xstart=xstart,xstop=xstop,step=step,type=type)  
nb.simple<-cbind(nb.simple,pred.cv=cv)  
plot.ntbft(nb.simple,nolines=c(2:4,6))
