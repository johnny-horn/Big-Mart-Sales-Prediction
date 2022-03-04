#-----------------------------------------------------------------------------
##################
##################  PARTIE 2 - EXPLORATION DES DONNEES
##################
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
################## Analyse univariée
#-----------------------------------------------------------------------------

#Item_Identifier
#-----------------------------------------------------------------------------
data %>% count(Item_Identifier)
data$Item_Id_Category <- as.factor(substr(data$Item_Identifier,1,2))

#Item_Weight
#-----------------------------------------------------------------------------
boxplot(data$Item_Weight)

hist(data$Item_Weight,freq=F)
lines(density(data$Item_Weight),col='red')

qqnorm(data$Item_Weight)
qqline(data$Item_Weight, col = 'red')
# -> Item_Weight semble normale

#Item_Fat_Content
#-----------------------------------------------------------------------------
barplot(table(data$Item_Fat_Content),main="data$Item_Fat_Content")
# -> Obs : les articles les plus achetés sont Low Fat

# Item_Visibility
#-----------------------------------------------------------------------------
boxplot(data$Item_Visibility)

hist(data$Item_Visibility,freq=F)
lines(density(data$Item_Visibility),col='red')

qqnorm(data$Item_Visibility)
qqline(data$Item_Visibility, col = 'red')


# Item_Type
#-----------------------------------------------------------------------------
boxplot(data$Item_MRP)

data %>% count(Item_Type,sort=TRUE)
barchart(table(data$Item_Type),main="data$Item_Type",col='red',xlab='')
# -> Fruits and Vegetables et Snack Foods sont les plus vendus
# -> car les gens les mangent le plus dans la vie quotidienne

# Item_MRP 
#-----------------------------------------------------------------------------
hist(data$Item_MRP,freq=F)
lines(density(data$Item_MRP),col='red')

qqnorm(data$Item_MRP)
qqline(data$Item_MRP, col = 'red')

#Outlet_Identifier
#-----------------------------------------------------------------------------
data %>% count(Outlet_Identifier,sort=TRUE)
barchart(table(data$Outlet_Identifier),main="data$Outlet_Identifier",col='red',xlab='')

#Outlet_Establishment_Year
#-----------------------------------------------------------------------------
boxplot(data$Outlet_Establishment_Year)

data$Outlet_Age <- as.numeric(substr(Sys.Date(),1,4)) - data$Outlet_Establishment_Year
# Sys.Date(),1,4) = 2021
barplot(table(data$Outlet_Age),main="data$Outlet_Age",las=1)
# -> les points de vents depuis 36 ans vendent les plus

data$Outlet_Establishment_Year <- as.factor(data$Outlet_Establishment_Year)

# Outlet_Size
#-----------------------------------------------------------------------------
barplot(table(data$Outlet_Size),main="data$Outlet_Size")
# -> les vents sont majoritairement de taille Small

# Outlet_Location_Type
#-----------------------------------------------------------------------------
barplot(table(data$Outlet_Location_Type),main="data$Outlet_Location_Type")
# -> les plus vendus sont dans les villes de type Trier 3

# Outlet_Type
#-----------------------------------------------------------------------------
barchart(data$Outlet_Type,main="data$Outlet_Type",col='red',xlab='')
data %>% count(Outlet_Type)
# -> les articles les vendus sont dans les supermarchés de type 1

# Item_Outlet_Sales pour train
#-----------------------------------------------------------------------------
hist(data$Item_Outlet_Sales,freq=F)
lines(density(data$Item_Outlet_Sales,na.rm = TRUE),col='red')

qqnorm(data$Item_Outlet_Sales)
qqline(data$Item_Outlet_Sales, col = 'red')


#-----------------------------------------------------------------------------
################## Analyse bivariée
#-----------------------------------------------------------------------------
names(train)

train <- subset(data, source =='train')
test <- subset(data, source =='test')


# 1) en comparant avec Item_Outlet_Sales
# 2) en comparant avec Item_Weight
# 3) en comparant avec Item_Visibility
# 4) en comparant avec Item_Fat_Content
# 



# 1) en comparant avec Item_Outlet_Sales
#-----------------------------------------------------------------------------

pairs(train[,names_num(train)],col='red')

# Item_Weight
plot(train$Item_Weight,train$Item_Outlet_Sales,col='red')


#Item_Type
barchart(data$Item_Type ~ data$Item_Outlet_Sales,  data = data,col='red')
# -> the products available were Fruits and Veggies and Snack foods but
# the sales of Seafood and Starchy foods seems higher and hence the sales can be
# improved with having stock of products that are most bought by customers

ggplot(data, aes(Item_Outlet_Sales,Item_Visibility,col = Item_Type)) + geom_point()  
ggplot(data, aes(Item_Outlet_Sales,Item_Visibility,col = Outlet_Type)) + geom_point()

# permet de voir les boîtes à moustaches de différentes variables.
#maintenant, nous allons examiner les caractéristiques ayant des valeurs 
# aberrantes et mettre en œuvre une technique de détection des valeurs 
# aberrantes appropriée
# nous ne regarderons que les variables qui ne sont pas catégorielles


# Item_MRP
plot(train$Item_MRP,train$Item_Outlet_Sales,col='red')
# -> on peut voir des clusters et on peut utiliser kmeans :
Item_MRP_clusters = kmeans(data$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster)
data$Item_MRP_clusters <- as.factor(Item_MRP_clusters$cluster)

# Outlet_Identifier
ggplot(data, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_boxplot()

#Outlet_Year
ggplot(data, aes(Item_Outlet_Sales,Item_MRP,col = Outlet_Establishment_Year)) + geom_point()


# 2) en comparant avec Item_Weight
#-----------------------------------------------------------------------------

# Outlet_Identifier
ggplot(data, aes(Outlet_Identifier, Item_Weight)) + geom_boxplot()

# Item_Type
ggplot(data, aes(Item_Type, Item_Weight)) + geom_boxplot() 
# -> nous pouvons voir que les poids de tous les types d'articles sont là.

# Item_Fat_Content
ggplot(data, aes(Item_Fat_Content, Item_Weight)) + geom_boxplot() 

# 3) en comparant avec Item_Visibility
#-----------------------------------------------------------------------------

# Item_Type
ggplot(data, aes(Item_Type,Item_Visibility)) + geom_boxplot()
# > Ainsi, les points au-dessus de chaque type d'élément représentent 
# les valeurs aberrantes dans la visibilité de l'élément correspondant 
# à cet élément particulier.

# Outlet_Identifier
ggplot(data, aes(Outlet_Identifier,Item_Visibility)) + geom_boxplot() 
# -> il ne sert à rien d'afficher la boîte à moustaches de Item_Visibility
# par rapport à Item_Identifier.

# 4) en comparant avec Item_Fat_Content
#-----------------------------------------------------------------------------
summary(data$Item_Fat_Content)
summary(data$Item_Id_Category)
# -> quelques produits non mangeables !
data$Item_Fat_Content <- as.character(data$Item_Fat_Content)
data$Item_Fat_Content[which(data$Item_Id_Category == "NC")] = 'Non Edible'
data$Item_Fat_Content <- as.factor(data$Item_Fat_Content)
summary(data$Item_Fat_Content)



#-----------------------------------------------------------------------------
################## Nouvelles colonnes
#-----------------------------------------------------------------------------

train <- subset(data, source =='train')
test <- subset(data, source =='test')
#

data$sommes_num <- apply(data[,c("Item_Weight","Item_Visibility","Item_MRP")],1,sum)


data$Qty_Vente <- data$Item_Outlet_Sales/data$Item_MRP
data$MrpParUnite <- data$Item_MRP / as.numeric(data$Outlet_Size)
names_num(data)

data$Visi_plus_MRP <- data$Item_Visibility + data$Item_MRP
data$Visi_fois_MRP <- data$Item_Visibility * data$Item_MRP

data$Visi_plus_Weight <- data$Item_Visibility + data$Item_Weight
data$Visi_fois_Weight <- data$Item_Visibility * data$Item_Weight

data$MRP_plus_Weight <- data$Item_MRP + data$Item_Weight
data$MRP_fois_Weight <- data$Item_MRP * data$Item_Weight

#
tmp <- as.numeric(order_n(train,train$Item_Outlet_Sales,
                          train$Outlet_Identifier,
                          decreasing = FALSE))
data$Outlet_Ranking <- rbind(as.matrix(tmp),as.matrix(test$Item_Outlet_Sales))

data$Sum_of_ordinals <- apply(data[,names_num(data)],1,sum)# + data$Outlet_Ranking
write.table(data,"data00.csv",sep=',')
data$ItemAvgMRPmap <- ave(data$Item_MRP,data$Item_Identifier,FUN=mean)/ data$Item_Weight

data$produit_num <- apply(data[,names_num(data)],1,prod)

# data$Item_Type_Cat
data$Item_Type_Cat <- as.character(data$Item_Type)
data$Item_Type_Cat[data$Item_Type_Cat == 'Fruits and Vegetables'] <- 'Fruits_Veg'
data$Item_Type_Cat[data$Item_Type_Cat == 'Household'] <- 'HH'
data$Item_Type_Cat[data$Item_Type_Cat == 'Health and Hygiene'] <- 'HH'
data$Item_Type_Cat[data$Item_Type_Cat == 'Baking Goods'] <- 'bake'
data$Item_Type_Cat[data$Item_Type_Cat == 'Snack Foods'] <- 'bake'
data$Item_Type_Cat[data$Item_Type_Cat == 'Canned'] <- 'frozen_canned'
data$Item_Type_Cat[data$Item_Type_Cat == 'Frozen Foods'] <- 'frozen_canned'
data$Item_Type_Cat[data$Item_Type_Cat == 'Dairy'] <- 'dairy'
data$Item_Type_Cat[data$Item_Type_Cat == 'Breakfast'] <- 'break_bread'
data$Item_Type_Cat[data$Item_Type_Cat == 'Breads'] <- 'break_bread'
data$Item_Type_Cat[data$Item_Type_Cat == 'Starchy Foods'] <- 'break_bread'
data$Item_Type_Cat[data$Item_Type_Cat == 'Seafood'] <- 'Seafood_Meat'
data$Item_Type_Cat[data$Item_Type_Cat == 'Meat'] <- 'Seafood_Meat'
data$Item_Type_Cat[data$Item_Type_Cat == 'Hard Drinks'] <- 'drinks'
data$Item_Type_Cat[data$Item_Type_Cat == 'Soft Drinks'] <- 'drinks'
data$Item_Type_Cat[data$Item_Type_Cat == 'Others'] <- 'others'
data$Item_Type_Cat <- as.factor(data$Item_Type_Cat)

levels(data$Item_Type_Cat) 

# Item_Type_new
data %>% dplyr::count(Item_Type) # 16 types dalimentation/boissons/non alimentaires
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")
data$Item_Type_new <-as.factor(ifelse(data$Item_Type %in% perishable, "perishable",
                                      ifelse(data$Item_Type %in% non_perishable, "non_perishable", "not_sure")))


names(data)
summary(data)
# avec library(DataExplorer)
plot_missing(data)
plot_bar(data)
plot_histogram(data)

#-----------------------------------------------------------------------------
################## Corrélation
#-----------------------------------------------------------------------------

train <- subset(data, source =='train')
test <- subset(data, source =='test')

plot_correlation(train[,names_num(train)])
ndrop =c("Visi_plus_Weight","Visi_fois_Weight")
data <- data[,!names(data) %in% ndrop]

train <- subset(data, source =='train')
test <- subset(data, source =='test')

plot_correlation(train[,names_num(train)])

plot_correlation(train[,!names(train) %in% c("source")])
