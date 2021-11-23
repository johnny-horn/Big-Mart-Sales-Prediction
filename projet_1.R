source("library_biblio.R")
source("fonctions.R")


#-----------------------------------------------------------------------------
##################
##################  PARTIE 1 - IMPUTATIONS
##################
#-----------------------------------------------------------------------------

# Dans cette partie, le but est d'imputer les données manquantes
# par la moyenne par groupe pour Item_Weight et Item_Visibility, par le mode par
# groupe pour Outlet_Size


#-----------------------------------------------------------------------------
##############        importation
#-----------------------------------------------------------------------------

train <- read.csv("train_v9rqX0R.csv",sep=',',na.strings=c("", "NA"))
test <- read.csv("test_AbJTz2l.csv",sep=',',na.strings=c("", "NA"))
y_exemple <- read.csv("sample_submission_8RXa3c6.csv",sep=',')

train$source <- as.factor('train')
test$Item_Outlet_Sales <- NA
test$source <- as.factor('test')

data <- rbind(train,test)

#-----------------------------------------------------------------------------

str(data) # 5 num et int et 7 factor
summary(data)
# remarques :
# 1) dans la colonne Item_Fat_Content : erreurs car low fat est aussi LF, reg est Regular...
# 2) Item_Visibility ne peut être égal à 0
# 3) La variable Outlet_Establishment_Year est considérée comme quantitative 
# 4) il y a énormément de données manquantes

#-----------------------------------------------------------------------------
################## Item_Fat_Content
#-----------------------------------------------------------------------------

data$Item_Fat_Content <- mapvalues(data$Item_Fat_Content, 
                                   from =c("LF","Low Fat","low fat","reg"), 
                                   to = c("Low Fat","Low Fat","Low Fat","Regular"))
levels(data$Item_Fat_Content)

#-----------------------------------------------------------------------------
################## Item_Visibility
#-----------------------------------------------------------------------------

plot(data$Item_Visibility,data$Item_Outlet_Sales)
# -> le min de Item_Visibilityest 0. Cela n'a aucun sens quand un produit est 
# vendu dans un magasin : la visibilité ne peut être 0.

boxplot(as.matrix(data$Item_Visibility),main="data$Item_Visibility")
# tellement de points aberrants


# on calcule le 3e quartile
bench <- summary(data$Item_Visibility)["3rd Qu."] + 1.5*IQR(data$Item_Visibility) 
bench <- round(bench,2)

# on supprime les 0
data$Item_Visibility[data$Item_Visibility == 0] <- NA
data$Item_Visibility[data$Item_Visibility > bench] <- NA

aggregate(data$Item_Visibility, list(data$Item_Identifier), FUN=mean) 

# on remplit en fonction de Item_Identifier
data$Item_Visibility <-ave(data$Item_Visibility,data$Item_Identifier,
                            FUN=function(x) ifelse(is.na(x), 
                                                   mean(x,na.rm=TRUE), 
                                                   x))

#-----------------------------------------------------------------------------
################## Outlet_Establishment_Year
#-----------------------------------------------------------------------------

data$Outlet_Establishment_Year <- as.factor(data$Outlet_Establishment_Year)

#-----------------------------------------------------------------------------
################## Item_Weight
#-----------------------------------------------------------------------------

boxplot(data$Item_Weight)
par(mfrow=c(1,2))
plot(data$Item_Weight,data$Item_Outlet_Sales,col='red',main="avant l'imputation")
length(unique(data$Item_Weight))/nrow(data) # seuls les 2% sont différents

data$Item_Weight <-ave(data$Item_Weight,data$Item_Identifier,FUN=getmode)
plot(data$Item_Weight,data$Item_Outlet_Sales,col='red',main="après l'imputation")
par(mfrow=c(1,1))

#-----------------------------------------------------------------------------
################## Outlet_Size
#-----------------------------------------------------------------------------

table(data$Outlet_Size)
table(data$Outlet_Identifier, is.na(data$Outlet_Size))

col_Out_Id_na = c("OUT010","OUT017","OUT045")

idx_fct <- function(n_col){
  which((data$Outlet_Identifier == n_col) & is.na(data$Outlet_Size))
}

# OUT010
tmp <- data[idx_fct(col_Out_Id_na[1]),]
summary(tmp[,names_fact(tmp)])
# OUT017
tmp <- data[idx_fct(col_Out_Id_na[2]),]
summary(tmp[,names_fact(tmp)])
# OUT045
tmp <- data[idx_fct(col_Out_Id_na[3]),]
summary(tmp[,names_fact(tmp)])

# 1) -> Pour OUT017 et OUT045, Outlet_Location_Type et Outlet_Type sont les
# les mêmes, ie Tier 2 et Supermarket Type1

idx <- which((data$Outlet_Location_Type == "Tier 2") & (data$Outlet_Type =="Supermarket Type1"))
tmp <- data[idx,]
summary(tmp) # donc c'est Small 
data[idx,]$Outlet_Size = 'Small'


# 2) -> il reste plus qu'à trouver pour OUT010

tmp <- data[idx_fct(col_Out_Id_na[1]),]
summary(tmp[,names_fact(tmp)])

table(data$Outlet_Location_Type, is.na(data$Outlet_Size)) # trier 3
table(data$Outlet_Type, is.na(data$Outlet_Size)) # Grocery Store

tmp <- data[which(data$Outlet_Location_Type =="Tier 3"),]
summary(tmp[,names_fact(tmp)])

tmp <- data[which(data$Outlet_Type =="Grocery Store"),]
summary(tmp[,names_fact(tmp)])

# -> on va devoir utiliser un classifieur pour OUT010

#-----------------------------------------------------------------------------
################## Prédire Outlet_Size pour OUT010
#-----------------------------------------------------------------------------

data$Outlet_Size <- as.numeric(data$Outlet_Size,
                               levels=c("Small","Medium","High"))  

data$Outlet_Size <- as.factor(data$Outlet_Size)

data$Item_Fat_Content <- as.numeric(data$Item_Fat_Content,
                                    levels=c("Low Fat","Regular"))                             

names(data)
n_cols_dummy <- c("Item_Type","Outlet_Identifier","Outlet_Location_Type","Outlet_Type")

# dummyfication
# ----------------------------------------------------------------------------
tmp <- dummyfication(data[,n_cols_dummy])
tmp <- cbind(tmp,data[,!names(data) %in% n_cols_dummy])

n_cols_no <- c("Item_Identifier" ,"Outlet_Size","source",
               "Outlet_Establishment_Year","Item_Outlet_Sales")

y_cols <- as.character('Outlet_Size')
X_Cols <- names(tmp %>% dplyr::select(-c(n_cols_no)))

# partitionnement
# ----------------------------------------------------------------------------
tmp0 <- subset(tmp, source=="train")
tmp0 <- tmp0[,!names(tmp) %in% c("source")]

X <- tmp0[,X_Cols]
X <- X[-which(is.na(tmp0[,y_cols])),]
y <- tmp0[-which(is.na(tmp0[,y_cols])),y_cols]

D <- partition(X,y,p=0.7)

# knn 2 minutes
# ----------------------------------------------------------------------------
IdentifKValeurCrossValidation <- function(X,Y,deb_K,fin_K,cv,scoring="Accuracy"){
  k_range <- seq(deb_K, fin_K)
  k_scores = rep(0,length(k_range))
  for(k in k_range){
    trControl <- trainControl(method  = "cv",number  = cv)
    scores <- caret::train(x = model.matrix(~. - 1, X),
                           y = as.factor(Y),
                           method     = "knn",
                           tuneGrid   = expand.grid(k = k),
                           trControl  = trControl,
                           metric     = scoring)$results[2]
    k_scores[k] <- scores[1,1]
  }
  z = which(k_scores == max(k_scores))
  print("ind pour Max Accuaracy est :")
  for(i in z){
    print(k_range[i])
  }
  plot(k_range, k_scores,xlab='Valeur de k pour KNN',ylab='Accuracy CV',type='l')
  print("Valeur de k est :")
  return(k_range[i])
}
tim <- proc.time()
k <- IdentifKValeurCrossValidation(X=D$X_train,
                                   Y=D$y_train,
                                   cv=5,
                                   deb_K=1,
                                   fin_K=50,
                                   scoring="Accuracy")
k
tim <- proc.time()-tim
tim[3]/60
model_knn <- train(D$X_train,as.factor(D$y_train),
                method     = "knn",
                tuneGrid   = expand.grid(k = k),
                trControl  = trainControl(method  = "cv",number  = 5),
                metric     = "Accuracy")

cm <- confusionMatrix(predict(model_knn,newdata = D$X_test),as.factor(D$y_test))
print("Accuracy de test : ")
cm$overall[1] # OK on préférerait proche de 1

idx <- which(is.na(tmp[,y_cols]) & tmp$source == 'train')
table(predict(knnFit,newdata = tmp[idx,X_Cols]))

# SVM 4 secondes
# ----------------------------------------------------------------------------
tim <- proc.time()
trControl <- trainControl(method  = "cv",number  = 5)
model_svm <- train(D$X_train,as.factor(D$y_train),
                        method='svmLinear',
                        tuneLength=2,
                        trControl = trControl)
tim <- proc.time()-tim
tim[3]/60 

cm <- confusionMatrix(predict(model_svm,newdata = D$X_test),as.factor(D$y_test))
print("Accuracy de test : ")
cm$overall[1] # 100 %

idx <- which(is.na(tmp[,y_cols]) & tmp$source == 'train')
table(predict(model_svm,newdata = tmp[idx,X_Cols])) # medium


#### adaboost 21/24 min !
# ----------------------------------------------------------------------------
tim <- proc.time()
trControl <- trainControl(method  = "cv",number  = 5)
model_adaboost <- train(D$X_train,as.factor(D$y_train),
                       method='AdaBoost.M1',
                       tuneLength=2,
                       trControl = trControl)
tim <- proc.time()-tim
tim[3]/60

cm <- confusionMatrix(predict(model_adaboost,newdata = D$X_test),as.factor(D$y_test))
print("Accuracy de test : ")
cm$overall[1]

idx <- which(is.na(tmp[,y_cols]) & tmp$source == 'train')
table(predict(model_adaboost,newdata = tmp[idx,X_Cols])) # donc medium !

# imputation
# ----------------------------------------------------------------------------
idx <- which(is.na(tmp$Outlet_Size))
tmp[idx,]$Outlet_Size <- 2
table(tmp$Outlet_Size)
colSums(is.na(tmp))

#-----------------------------------------------------------------------------
################## write csv
#-----------------------------------------------------------------------------
tmp$Outlet_Size <- mapvalues(tmp$Outlet_Size,
                             from =c(1,2,3),
                             to = c('High','Medium','Small'))

levels(tmp$Outlet_Size)

setwd("~/Dropbox/Big Mart Sales Prediction")
tmp_train <- subset(tmp, source=='train')

train$Item_Weight <- tmp_train$Item_Weight
train$Item_Visibility <- tmp_train$Item_Visibility
train$Outlet_Size <- tmp_train$Outlet_Size
train$source <- NULL

tmp_test <- subset(tmp, source=='test')

test$Item_Weight <- tmp_test$Item_Weight
test$Item_Visibility <- tmp_test$Item_Visibility
test$Outlet_Size <- tmp_test$Outlet_Size
test$source <- NULL
test$Item_Outlet_Sales <- NULL

summary(train)
summary(test)

dim(train)
dim(test)

write.table(train,file="train_imp.csv",sep=',')
write.table(test,file="test_imp.csv",sep=',')

# vérificiation
#-----------------------------------------------------------------------------
train <- read.csv("train_v9rqX0R.csv",sep=',',na.strings=c("", "NA"))
tmp_t <- read.csv("train_imp.csv",sep=',',na.strings=c("", "NA"))

train$Outlet_Size[21:50]
tmp_t$Outlet_Size[21:50]

test <- read.csv("test_AbJTz2l.csv",sep=',',na.strings=c("", "NA"))
tmp_test <- read.csv("test_imp.csv",sep=',',na.strings=c("", "NA"))

test$Outlet_Size[1:20]
tmp_test$Outlet_Size[1:20]


#-----------------------------------------------------------------------------
##################
##################  FIN DE LA PARTIE 1
##################
#-----------------------------------------------------------------------------

