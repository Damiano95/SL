rm(list=ls())
library(corrplot)
library("caret")
library(caretEnsemble)
library(Metrics)

audio <- read.csv("https://damiano95.github.io/SL/audio_features.txt", sep="")

# elimino le variabili "track_name", "artist_name", "album_name", "track_uri"
da_elim <- which(names(audio) %in% c("track_name", "artist_name", "album_name", "track_uri"))
# divido tra train e test

set.seed(123)
oss_test <- sample(1:nrow(audio), size = nrow(audio)*0.30, replace=F)

train <- audio[-oss_test,-da_elim]
test <- audio[oss_test,-da_elim]



######## CORRPLOT ########

numeric = sapply(train,is.numeric)

 
# andiamo a vedere la correlazione tra le variabili numeriche
# prendo le variabili numeriche, calcolo la correlazione tra tutte le variabili e ordino i valori

num_train <- train[,numeric]
corr <- cor(num_train, use="pairwise.complete.obs")
corr_ord <- as.matrix(sort(corr[,"Class"], decreasing= T))

# adesso prendo solo le correlazioni che sono superiori allo 0.5 e costruisco la matrice delle correlazioni

high_corr <- names(which(apply(corr_ord, 1, function(x) abs(x)>0.5)))
corr_fin <- corr[high_corr, high_corr]

# corrplot.mixed(corr_fin) 

# le uniche variabili correlate con la variabile Class sono "valence" e "danceability"

# come ci aspettavamo già le variabili numeriche che discriminano meglio "Class" sono "danceability" e "valence"
#proviamo ad utilizzare una random forest per vedere anche le varibili qualitative

train$Class <- as.factor(train$Class)
levels(train$Class) <- c("Giacomo", "Damiano")

#Configurazione parametri
ctrl <- trainControl(summaryFunction = twoClassSummary,classProbs = TRUE)
ctrl2 <- trainControl(method= "cv", number = 10, summaryFunction = twoClassSummary,classProbs = TRUE)
my_grid.i<-expand.grid(nrounds=500, eta=0.02, max_depth=5, gamma=5,
                       min_child_weight=3, subsample=0.74, colsample_bytree=0.57 )

######## IMPORTANZA VARIABILI ########

#IMPORTANZA RF

set.seed(321)
rf.i <- train(Class ~ ., 
            train,
            method = "rf",
            trControl=ctrl,
            metric="ROC")
# plot(varImp(rf.i))

#IMPORTANZA LASSO

set.seed(321)
fit.lasso.i<- train(Class ~ .,
                  train,
                  method = "glmnet",
                  trControl=ctrl2,
                  tuneGrid=expand.grid(alpha=1,lambda=seq(0.001, 1, by = 0.001)),
                  family="binomial",
                  metric = "ROC")
# plot(varImp(fit.lasso.i))

#IMPORTANZA XGBOOST

set.seed(321)
fit.xgb.i <- train(Class ~ .,
                 train,
                 method = "xgbTree",
                 trControl=ctrl2,
                 tuneGrid=my_grid.i,
                 family="binomial",
                 metric = "ROC")
# plot(varImp(fit.xgb.i))

######## ESPORTAZIONE DATASET ########


#RANDOM FOREST

set.seed(321)
rf <- train(Class ~ danceability+valence+energy+speechiness+instrumentalness+acousticness+
              tempo+liveness+loudness+time_signature+mode+key+duration_ms, 
                     train,
                     method = "rf",
                     trControl=ctrl,
                     preProcess=c("center","scale"),
                     metric="ROC")
prob_rf=predict(rf, newdata=test,type="prob")[,2]

#LASSO

set.seed(321)
fit.lasso<- train(Class ~ danceability+valence+energy+liveness+speechiness+instrumentalness, 
                   train,
                   method = "glmnet",
                   trControl=ctrl2,
                   tuneGrid=expand.grid(alpha=1,lambda=seq(0.001, 1, by = 0.001)),
                   preProcess=c("center","scale"),
                   family="binomial",
                   metric = "ROC")

phat_lasso=predict(fit.lasso, newdata=test,type="prob")[,2]

#XGBOOST
set.seed(321)
fit.xgb <- train(Class ~ danceability+valence+energy+liveness+
                   speechiness+instrumentalness+loudness+
                   acousticness+key+tempo+time_signature,
                 train,
                 method = "xgbTree",
                 trControl=ctrl2,
                 tuneGrid=my_grid,
                 family="binomial",
                 preProcess=c("center","scale"), 
                 metric = "ROC")

phat_xgb=predict(fit.xgb, newdata=test,type="prob")[,2]

#GLM
set.seed(321)
fit.glm <- train(Class~danceability+valence+energy+speechiness + instrumentalness,
                 train,
                 method = "glm",
                 trControl=ctrl2,
                 family="binomial",
                 preProcess=c("center","scale"), 
                 metric = "ROC")
phat_glm=predict(fit.glm, newdata=test,type="prob")[,2]


write.table(file="rf.txt", prob_rf, row.names=F, col.names=F)
write.table(file="lasso.txt", phat_lasso, row.names=F, col.names=F)
write.table(file="xgb.txt", phat_xgb, row.names=F, col.names=F)
write.table(file="glm.txt",phat_glm, row.names=F, col.names=F)