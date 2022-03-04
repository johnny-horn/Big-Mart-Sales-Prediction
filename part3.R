
#-----------------------------------------------------------------------------
##################
##################  PARTIE 3 - Transformations des variables
##################              et sélection de variables
##################
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------

str(data) # 5 num et int et 7 factor
summary(data)

Y_col <- "Item_Outlet_Sales"
sous_Y_cols <- c("Qty_Vente","Outlet_Ranking","Sum_of_ordinals","produit_num")

data$Outlet_Establishment_Year <- as.factor(data$Outlet_Establishment_Year)
data$Item_MRP_clusters <- as.factor(data$Item_MRP_clusters)


#-----------------------------------------------------------------------------
#  dummy
tmp <- data[,names_fact(data)] 
tmp <- tmp[,!names(tmp) %in% sous_Y_cols]
data_dummy <- dummyfication(tmp)
names(data_dummy)

#-----------------------------------------------------------------------------
##############  transformations des variables pour prédire au mieux Item_Outlet_Sales
#-----------------------------------------------------------------------------

# Item_Weight
which.min(abs(skew_comp(data$Item_Weight)[,1]))
mycor_Sales(data$Item_Weight)

# Item_Visibility
bc <- boxcox(data$Item_Outlet_Sales~data$Item_Visibility)
lambda <- bc$x[which.max(bc$y)]
new <- (data$Item_Visibility^lambda-1)/lambda
skew_comp(new)
myplot(new,main='Item_Visibility av trsf')
mycor_Sales(new)

skew_comp(data$Item_Visibility)
which.min(abs(skew_comp(data$Item_Visibility)[,1])) # sqrt
myplot(data$Item_Visibility,main='Item_Visibility av trsf')
mycor_Sales(data$Item_Visibility)
myplot(sqrt(data$Item_Visibility),main='Item_Visibility ap trsf')
mycor_Sales(sqrt(data$Item_Visibility))
data$Item_Visibility <- sqrt(data$Item_Visibility)

# Item_MRP
which.min(abs(skew_comp(data$Item_MRP)[,1]))

# Outlet_Age
skew_comp(data$Outlet_Age)
which.min(abs(skew_comp(data$Outlet_Age)[,1])) # log+1
myplot(data$Outlet_Age,main='Outlet_Age av trsf')
myplot(log(data$Outlet_Age+1),main='Outlet_Age ap trsf')
data$Outlet_Age <- log(data$Outlet_Age+1)

# sommes_num
which.min(abs(skew_comp(data$sommes_num)[,1]))
mycor_Sales(data$sommes_num)

# MrpParUnite
skew_comp(data$MrpParUnite)
which.min(abs(skew_comp(data$MrpParUnite)[,1])) # log+1
myplot(data$MrpParUnite,main='MrpParUnite av trsf')
myplot(log(data$MrpParUnite+1),main='MrpParUnite ap trsf')
data$MrpParUnite <- log(data$MrpParUnite+1)

# Visi_plus_MRP
which.min(abs(skew_comp(data$Visi_plus_MRP)[,1]))

# Visi_fois_MRP
skew_comp(data$Visi_fois_MRP)
which.min(abs(skew_comp(data$Visi_fois_MRP)[,1])) # log+1
myplot(data$Visi_fois_MRP,main='Visi_fois_MRP av trsf')
myplot(log(data$Visi_fois_MRP+1),main='Visi_fois_MRP ap trsf')
data$Visi_fois_MRP <- log(data$Visi_fois_MRP+1)

# MRP_plus_Weight
which.min(abs(skew_comp(data$MRP_plus_Weight)[,1]))

# MRP_fois_Weight
skew_comp(data$MRP_fois_Weight)
which.min(abs(skew_comp(data$MRP_fois_Weight)[,1])) # sqrt
myplot(data$MRP_fois_Weight,main='MRP_fois_Weight av trsf')
myplot(sqrt(data$MRP_fois_Weight),main='MRP_fois_Weight ap trsf')
data$MRP_fois_Weight <- sqrt(data$MRP_fois_Weight)

# ItemAvgMRPmap
skew_comp(data$ItemAvgMRPmap)
which.min(abs(skew_comp(data$ItemAvgMRPmap)[,1])) # log+1
myplot(data$ItemAvgMRPmap,main='ItemAvgMRPmap av trsf')
myplot(log(data$ItemAvgMRPmap+1),main='ItemAvgMRPmap ap trsf')
data$ItemAvgMRPmap <- log(data$ItemAvgMRPmap+1)

# Sum_of_ordinals
skew_comp(na.omit(data$Sum_of_ordinals))
which.min(abs(skew_comp(na.omit(data$Sum_of_ordinals))[,1])) # sqrt
myplot(na.omit(data$Sum_of_ordinals),main='Sum_of_ordinals av trsf')
mycor_Sales(na.omit(data$Sum_of_ordinals))
myplot(sqrt(na.omit(data$Sum_of_ordinals)),main='Sum_of_ordinals ap trsf')
mycor_Sales(sqrt(na.omit(data$Sum_of_ordinals)))
data$Sum_of_ordinals <- sqrt(data$Sum_of_ordinals)


# ls("package:DataExplorer")
plot_correlation(na.omit(data),type="continuous")

data %>% count(Item_Identifier)
data$Item_Id_Category2 <- as.factor(substr(data$Item_Identifier,1,3))
data %>% count(Item_Id_Category2)

plot_correlation(na.omit(data),type="discrete")

#-----------------------------------------------------------------------------
##############  suppression des colonnes n'ayant aucun sens
#-----------------------------------------------------------------------------

names(data)
data$Item_Identifier <- NULL
data$Outlet_Establishment_Year <- NULL
data$Item_Id_Category2 <- NULL

data$Item_Type <- NULL

data$Outlet_Ranking <- NULL
data$Sum_of_ordinals <- NULL
data$produit_num <- NULL

data_Y <- data
data$Qty_Vente <- NULL


data_Y$Item_Outlet_Sales <- NULL
names(data)
names(data_Y)
summary(data)
summary(data_Y)
sum(is.na(data))
colSums(is.na(data_Y))

#-----------------------------------------------------------------------------
##############  dummyfication
#-----------------------------------------------------------------------------
# pour data
summary(data)
tmp <- data
#data <- tmp
names_fact(data)
names_num(data)
data_dummy <- dummyfication(data[,names_fact(data)])
data <- cbind(data_dummy,data[,names_num(data)])
data$source <- tmp$source
names(data)

train <- subset(data, source =='train')
test <- subset(data, source =='test')

# pour data_Y
summary(data_Y)
tmp <- data_Y
#data_Y <- tmp
names_fact(data_Y)
names_num(data_Y)
data_dummy <- dummyfication(data_Y[,names_fact(data_Y)])
data_Y <- cbind(data_dummy,data_Y[,names_num(data_Y)])
data_Y$source <- tmp$source
names(data_Y)

train_Y <- subset(data_Y, source =='train')
test_Y <- subset(data_Y, source =='test')

#-----------------------------------------------------------------------------
##############  sélection de variables
#-----------------------------------------------------------------------------

D <- partition(subset(train,select=-c(Item_Outlet_Sales,source)),
               train$Item_Outlet_Sales)

#-----------------------------------------------------------------------------
# boruta # ATTENTION 45min
tim <- proc.time()
# bor <- Boruta(D$X_train,D$y_train)
bor <- Boruta(subset(train,select=-c(Item_Outlet_Sales,source)),
                     train$Item_Outlet_Sales,
              doTrace = 1)
tim <- proc.time()-tim
tim[3]/60

gS <- getSelectedAttributes(bor, withTentative = TRUE)
write.table(gS,file="gS.csv")
roughFixMod <- TentativeRoughFix(bor)
boruta_signif <- getSelectedAttributes(roughFixMod)
boruta_signif

write.table(boruta_signif,file="boruta_signif.csv")

col_selec <- c(boruta_signif[,1])
#new_data <- data[,col_selec]
new_data <- data[,boruta_signif]
new_data$Item_Outlet_Sales <- data$Item_Outlet_Sales
new_data$source <- data$source
summary(new_data)
write.table(new_data,file="new_data.csv",sep=",")



imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

i <- imps2[order(-imps2$meanImp), ]
write.table(i,file="impVar.csv")

plot(bor, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

# boruta pour prédire Qty_Vente 23 minutes
tim <- proc.time()
bor <- Boruta(subset(train_Y,select=-c(Qty_Vente,source)),
              train_Y$Qty_Vente,
              doTrace = 1)
tim <- proc.time()-tim
tim[3]/60

gS2 <- getSelectedAttributes(bor, withTentative = TRUE)
write.table(gS2,file="gS2.csv")
roughFixMod2 <- TentativeRoughFix(bor)
boruta_signif2 <- getSelectedAttributes(roughFixMod2)
boruta_signif2

write.table(boruta_signif2,file="boruta_signif2.csv")

col_selec2 <- c(boruta_signif2)

new_data2 <- data_Y[,boruta_signif2]
new_data2$Qty_Vente <- data_Y$Qty_Vente
new_data2$source <- data$source
summary(new_data2)
write.table(new_data2,file="new_data2.csv",sep=",")



imps_2 <- attStats(roughFixMod2)
imps2_2 = imps_2[imps_2$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2_2[order(-imps2_2$meanImp), ])  # descending sort

i2 <- imps2_2[order(-imps2_2$meanImp), ]
write.table(i2,file="impVar2.csv")

plot(bor, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


#-----------------------------------------------------------------------------
##################
##################  FIN DE LA PARTIE 3
##################
#-----------------------------------------------------------------------------
