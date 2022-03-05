#-----------------------------------------------------------------------------
##############        mode
#-----------------------------------------------------------------------------


getmode <- function(v){
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#-----------------------------------------------------------------------------
##############        factor
#-----------------------------------------------------------------------------


names_fact <- function(df){
  df$source <- NULL
  factor <- sapply(df,is.factor)
  return(names(factor[factor==TRUE]))
}

#-----------------------------------------------------------------------------
##############        numerique
#-----------------------------------------------------------------------------


names_num <- function(df){
  factor <- sapply(df,is.factor)
  return(names(factor[factor==FALSE]))
}

#-----------------------------------------------------------------------------
##############        dummy
#-----------------------------------------------------------------------------

dummyfication <- function(df){
  dummy <- dummyVars(" ~ .", data=df, fullRank = T)
  return(data.frame(predict(dummy, newdata = df)))
}

#-----------------------------------------------------------------------------
##############        partition
#-----------------------------------------------------------------------------

partition <- function(X,y,p=0.7){
  inTrain <- createDataPartition(y = y, p = 0.7, list = FALSE)
  D <- list()
  D$X_train <- X[inTrain, ]
  D$X_test <- X[-inTrain, ]
  D$y_train = y[inTrain]
  D$y_test = y[-inTrain]
  
  return(D)
}


#-----------------------------------------------------------------------------
##############        order
#-----------------------------------------------------------------------------

library(plyr)
order_n <- function(data,X,groupe,decreasing = T){
  order <- sort(with(data,tapply(X, groupe, mean)),decreasing = decreasing)
  return(mapvalues(groupe, 
                   from =names(order),
                   to = 1:length(order)
  )
  )
}

#-----------------------------------------------------------------------------
##############        skewness et kurtosis
#-----------------------------------------------------------------------------

skew_comp <- function(x) {
  L <- matrix(rep(0,10*2),ncol=2,
              dimnames = list(c("id","log","log+1","log.max","sqrt","sqrt.max","squared","exp","inv","inv.max"),
                              c("skewness","kurtosis")))
  
  L[1,1] <- skewness(x) ; L[1,2] <- moments::kurtosis(x)
  
  L[2,1] <- skewness(log(x)) ; L[2,2] <- moments::kurtosis(log(x))
  
  L[3,1] <- skewness(log(x+1)) ; L[3,2] <- moments::kurtosis(log(x+1))
  
  L[4,1] <- skewness(log(max(x+1) - x)) ; L[4,2] <- moments::kurtosis(log(max(x+1) - x))
  
  L[5,1] <- skewness(sqrt(x)) ; L[5,2] <- moments::kurtosis(sqrt(x))
  
  L[6,1] <- skewness(sqrt(max(x+1) - x)) ; L[6,2] <- moments::kurtosis(sqrt(max(x+1) - x))
  
  L[7,1] <- skewness((x)^2) ; L[7,2] <- moments::kurtosis((x)^2)
  
  L[8,1] <- skewness(exp(x)) ; L[8,2] <- moments::kurtosis(exp(x))
  
  L[9,1] <- skewness(1/(x)) ; L[9,2] <- moments::kurtosis(1/(x))
  
  L[10,1] <- skewness(1/(max(x+1) - x)) ; L[10,2] <- moments::kurtosis(1/(max(x+1) - x))
  
  return(L)
}

#-----------------------------------------------------------------------------
##############        plot des scatters
#-----------------------------------------------------------------------------

myplot <- function(x, mylwd = 3, mycol = rgb(0, 0.5, 0.5), myborder = grey(0.8),
                   mycolhist = rgb(0, 1, 1, 0.1), mycolnorm = rgb(0.7, 0.5, 0.7), ...){
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mfrow = c(1, 3))
  hx <- hist(x, plot = FALSE)
  dx <- density(x)
  hist(x, freq = FALSE, border = myborder, col = mycolhist, ylim = c(0, max(hx$density, dx$y)), ...)
  lines(dx, lwd = mylwd, col = mycol)
  plot(dx$x, log10(dx$y), type = "l", main = "Coordonnées semi-logarithmiques", xlab = "x",
       ylab = "log10(Density)", las = 1)
  legend("center", legend = round(skewness(x), 4), cex = 2, text.col = mycol, bty = "n",
         adj = 0.5)
  lines(dx$x, log10(dnorm(dx$x, mean(x), sd(x))), col = mycolnorm, lwd = mylwd)
  qqnorm(x, main = "Droite de Henry", xlab = "Quantiles théoriques",
         ylab = "Quantiles observés", las = 1)
  qqline(x)
}


#-----------------------------------------------------------------------------
##############        calcul de la corrélation
#-----------------------------------------------------------------------------

mycor_Sales <- function(x){
  ind <- which(is.na(data$Item_Outlet_Sales))
  return(cor(x[-ind],data$Item_Outlet_Sales[-ind]))
}

#-----------------------------------------------------------------------------
##############        plot des scatters
#-----------------------------------------------------------------------------


scatter<-function(X,Y) {
  par(mfrow=c(3,3))
  R2<-round(summary(lm(Y~X))$adj.r.squared,2)
  plot(y=Y,x=X,main=paste("R2=",R2),xlab='X')
  
  R2<-round(summary(lm(Y~log(X)))$adj.r.squared,2)
  plot(y=Y,x=log(X), main=paste("R2=",R2),xlab="log(X)")
  
  R2<-round(summary(lm(Y~sqrt(X)))$adj.r.squared,2)
  plot(y=Y,x=sqrt(X),main=paste("R2=",R2),xlab="sqrt(X)")
  
  R2<-round(summary(lm(Y~(X)^2))$adj.r.squared,2)
  plot(y=Y,x=(X)^2,main=paste("R2=",R2),xlab="(X)^2")
  
  R2<-round(summary(lm(Y~exp(X)))$adj.r.squared,2)
  plot(y=Y,x=exp(X),main=paste("R2=",R2),xlab="exp(X)")
  
  R2<-round(summary(lm(Y~1/X))$adj.r.squared,2)
  plot(y=Y,x=1/(X),main=paste("R2=",R2),xlab="1/(X)")
  
  R2<-round(summary(lm(Y~log(X+1)))$adj.r.squared,2)
  plot(y=Y,x=X,main=paste("R2=",R2),xlab='log(X+1)')
  
  R2<-round(summary(lm(Y~log(max(X+1)-X)))$adj.r.squared,2)
  plot(y=Y,x=X,main=paste("R2=",R2),xlab='log(max(X+1)-X)')
  
  R2<-round(summary(lm(Y~sqrt(max(X+1)-X)))$adj.r.squared,2)
  plot(y=Y,x=X,main=paste("R2=",R2),xlab='sqrt(max(X+1)-X)')
  
  par(mfrow=c(1,1))
}

