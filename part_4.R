
#-----------------------------------------------------------------------------
##################
##################  PARTIE 4 - Prédiction
##################
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
##############        importation
#-----------------------------------------------------------------------------

new_data <- read.csv("new_data.csv",sep=',')

train <- subset(new_data,source=='train')
test <- subset(new_data,source=='test')

y_to_pred <- read.csv("sample_submission_8RXa3c6.csv",sep=',')

summary(train)
train$source <- NULL
summary(test)
test$Item_Outlet_Sales <- NULL
test$source <- NULL

#-----------------------------------------------------------------------------
##############        Partition
#-----------------------------------------------------------------------------

D <- partition(subset(train,select=-c(Item_Outlet_Sales)),train$Item_Outlet_Sales)

x <- as.matrix(D$X_train) 
y <- as.matrix(D$y_train)

M <- c()
#-----------------------------------------------------------------------------
##############        Régression
#-----------------------------------------------------------------------------

set.seed(1235)
mod_lm <- train(x = D$X_train,
               y = log(D$y_train),
               method='lm', 
               trControl= trainControl(method="cv", number=10))



plot(varImp(mod_lm))
yhat <- exp(predict(mod_lm, D$X_test))
summary(yhat)

plot(yhat,D$y_test)
lines(D$y_test,D$y_test,col='red',type='o')


M$RMSE_lm <- Metrics::rmse(D$y_test,yhat)

#-----------------------------------------------------------------------------
##############        Régression Ridge
#-----------------------------------------------------------------------------

set.seed(1236)
ctrl <- trainControl(method="cv", number=10)
Grid <- expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0001))
mod_ridge <- train(x = D$X_train,
                      y = log(D$y_train),
                      method='glmnet', 
                      trControl= ctrl, 
                      tuneGrid = Grid)

varImp(mod_ridge)

plot(mod_ridge)
summary(mod_ridge)
yhat <- exp(predict(mod_ridge, D$X_test))
summary(yhat)

plot(yhat,D$y_test)
lines(D$y_test,D$y_test,col='red',type='o')


M$RMSE_ridge <- Metrics::rmse(D$y_test,yhat)

#-----------------------------------------------------------------------------
##############        Régression Lasso
#-----------------------------------------------------------------------------

set.seed(1237)
ctrl <- trainControl(method="cv", number=10)
Grid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0001))
mod_lasso <- train(x = D$X_train,
                   y = log(D$y_train),
                   method='glmnet', 
                   trControl= ctrl, 
                   tuneGrid = Grid,
                   metrics='Rsquared'#,preProcess=c("scale","center")
                   )

varImp(mod_lasso)

plot(mod_lasso)
summary(mod_lasso)
yhat <- exp(predict(mod_lasso, D$X_test))
summary(yhat)

plot(yhat,D$y_test)
lines(D$y_test,D$y_test,col='red',type='o')

M$RMSE_lasso <- Metrics::rmse(D$y_test,yhat)

#-----------------------------------------------------------------------------
##############        Random Forest
#-----------------------------------------------------------------------------

set.seed(1)
tim <- proc.time()
mod_rf <- train(D$X_train, ### 1h 42 minutes !!!
                  log(D$y_train),
                  method='rf', 
                  tuneLength=10,
                  ntree=1500,
                  trControl = trainControl(method  = "cv",
                                           number  = 5,
                                           verboseIter = TRUE))
tmp <- proc.time()-tim
tmp[3]/60

plot(varImp(mod_rf))

plot(mod_rf)
summary(mod_rf)
yhat <- exp(predict(mod_rf, D$X_test))

summary(yhat)

plot(yhat,D$y_test)
lines(D$y_test,D$y_test,col='red',type='o')

M$RMSE_rf <- Metrics::rmse(D$y_test, yhat)


#-----------------------------------------------------------------------------
##############        Régression nnet
#-----------------------------------------------------------------------------

set.seed(1238)
ctrl <- trainControl(method="cv", number=10)
Grid <- expand.grid(size = c(1, 3, 5,10,15,20),
                    decay = c(0.1, 0.001, 0.000001))
tim <- proc.time()
mod_nnet <- train(x = D$X_train, ### 28 minutes
                  y = log(D$y_train),
                  method='nnet', 
                  trControl= ctrl, 
                  tuneGrid = Grid,
                  linout=TRUE,
                  maxit = 1000)
tmp <- proc.time()-tim
tmp[3]/60

plot(varImp(mod_nnet))

plot(mod_nnet)
summary(mod_nnet)
yhat <- exp(predict(mod_nnet, D$X_test))
summary(yhat)

plot(yhat,D$y_test)
lines(D$y_test,D$y_test,col='red',type='o')

M$RMSE_nnet <- Metrics::rmse(D$y_test,yhat)

#-----------------------------------------------------------------------------
##############        Régression xgboost
#-----------------------------------------------------------------------------

set.seed(1337)

X_train = xgb.DMatrix(as.matrix(D$X_train))
y_train = D$y_train
X_test = xgb.DMatrix(as.matrix(D$X_test))
y_test = D$y_test

xgb_trcontrol <- trainControl(method = "cv", 
                             number = 10, 
                             allowParallel = TRUE, 
                             verboseIter = TRUE,
                             returnData = FALSE)

xgbGrid <- expand.grid(nrounds = c(50,100,200),   # best = 100
                       max_depth = c(1,3, 5, 10, 15, 20), # best = 3
                       colsample_bytree = seq(0.5, 0.9, length.out = 5), # best=0.5
                       ## valeurs par défaut : 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0)
tim <- proc.time()
mod_xgb <- train(X_train, ### 24 minutes
                  log(y_train), 
                  trControl = xgb_trcontrol, 
                  tuneGrid = xgbGrid, 
                  method = "xgbTree"
                  )
tmp <- proc.time()-tim
tmp[3]/60

plot(varImp(mod_xgb))

plot(mod_xgb)
summary(mod_xgb)
yhat <- exp(predict(mod_xgb,X_test))
summary(yhat)

plot(yhat,D$y_test)
lines(D$y_test,D$y_test,col='red',type='o')

M$RMSE_xgb <- Metrics::rmse(D$y_test,yhat)

##
##
##
M

#-----------------------------------------------------------------------------
##############        Prédire Y
#-----------------------------------------------------------------------------

#a <- exp(predict(mod_lm,test))
b <- exp(predict(mod_ridge,test))
c <- exp(predict(mod_lasso,test))
d <- exp(predict(mod_nnet,test))
e <- exp(predict(mod_rf,test))
f <- exp(predict(mod_xgb,test))

tmp <- cbind(b,c,d,e,f)
#y_to_pred$Item_Outlet_Sales <- apply(tmp,1,mean)
y_to_pred$Item_Outlet_Sales <- exp(predict(mod_lasso,test))

  
summary(y_to_pred)

#-----------------------------------------------------------------------------
##############        exportation
#-----------------------------------------------------------------------------

write.table(y_to_pred,"subm_Y_pred.csv",sep=',')


#-----------------------------------------------------------------------------
##################
##################  FIN DE LA PARTIE 4
##################
#-----------------------------------------------------------------------------


