
##
## 6. Logistic Regression
##    (is_pop vs acoustic features)
##

# test af's for normal distribution
# histograms
library(gridExtra)
a <- ggplot(sp_ArtSongAF,aes(x=acousticness))+geom_histogram()
d <- ggplot(sp_ArtSongAF,aes(x=danceability))+geom_histogram()
e <- ggplot(sp_ArtSongAF,aes(x=energy))+geom_histogram()
i <- ggplot(sp_ArtSongAF,aes(x=instrumentalness))+geom_histogram()  # skip this one
li <- ggplot(sp_ArtSongAF,aes(x=liveness))+geom_histogram()
lo <- ggplot(sp_ArtSongAF,aes(x=loudness))+geom_histogram()
s <- ggplot(sp_ArtSongAF,aes(x=speechiness))+geom_histogram() # skip this one
v <- ggplot(sp_ArtSongAF,aes(x=valence))+geom_histogram()
grid.arrange(a,d,e,i,li,lo,s,v,nrow=2)

library(dplyr)
# add track ID from tracks
spt <- left_join(sp,trk,by="song_id")

# trim columns
spt <- spt[,1:5]

# add artist ID from releases
spa <- left_join(spt,rls,by="album_id")

# trim columns and remove NA
spa <- spa[,1:6]   # 25,193 rows
spa <- na.omit(spa)  # 14,347 rows

# get artist names 
sp_art <- left_join(spa,art,by="artist_id")

# trim columns 
sp_art <- sp_art[1:11]
sp_art <- sp_art[,-8]

# get song names
sp_artsng <- left_join(sp_art,sg,by="song_id")

# trim columns and remove NA
sp_artsng <- sp_artsng[,1:11]

# add acoustic features
sp_ArtSongAF <- left_join(sp_artsng,af,by="song_id")
View(sp_ArtSongAF)
# take out columns key(col13), mode(14), time_signature(15), instumentalness(19) tempo(24)
sp_ArtSongAF <- sp_ArtSongAF[-24]
#sp_ArtSongAF <- sp_ArtSongAF[-19]
sp_ArtSongAF <- sp_ArtSongAF[-15]
sp_ArtSongAF <- sp_ArtSongAF[-14]
sp_ArtSongAF <- sp_ArtSongAF[-13]

# scale loudness to between 0 and 1
sp_ArtSongAFcp <- sp_ArtSongAF
sp_ArtSongAF$loudness
sp_ArtSongAF$loudness <- (sp_ArtSongAF$loudness-min(sp_ArtSongAF$loudness))/(max(sp_ArtSongAF$loudness)-min(sp_ArtSongAF$loudness))

# initially, d,e,lo,v possibly normal distrib and a,i,li,s not
# k-s test for normality - Ho (that the distribution is normal) fails in all cases so is rejected
ks.test(sp_ArtSongAF$acousticness,"pnorm")
ks.test(sp_ArtSongAF$danceability,"pnorm")
ks.test(sp_ArtSongAF$energy,"pnorm")
ks.test(sp_ArtSongAF$instrumentalness,"pnorm")
ks.test(sp_ArtSongAF$liveness,"pnorm")
ks.test(sp_ArtSongAF$loudness,"pnorm")
ks.test(sp_ArtSongAF$speechiness,"pnorm")
ks.test(sp_ArtSongAF$valence,"pnorm")

# test variances: Kuskal-Wallis test
kruskal.test(acousticness~danceability,data=sp_ArtSongAF)
aov(acousticness~danceability,energy,instrumentalness,liveness,loudness,speechiness,valence, data=sp_ArtSongAF)

# boxplots
aa <- ggplot(sp_ArtSongAF,aes(y=acousticness))+geom_boxplot()
dd <- ggplot(sp_ArtSongAF,aes(y=danceability))+geom_boxplot()
ee <- ggplot(sp_ArtSongAF,aes(y=energy))+geom_boxplot()
ii <- ggplot(sp_ArtSongAF,aes(y=instrumentalness))+geom_boxplot()
liv<- ggplot(sp_ArtSongAF,aes(y=liveness))+geom_boxplot()
lou<- ggplot(sp_ArtSongAF,aes(y=loudness))+geom_boxplot()
ss <- ggplot(sp_ArtSongAF,aes(y=speechiness))+geom_boxplot()
vv <- ggplot(sp_ArtSongAF,aes(y=valence))+geom_boxplot()
grid.arrange(aa,dd,ee,ii,liv,lou,ss,vv,nrow=2)
# only valence has no outliers

plot3d(x=sp_ArtSongAF$danceability,y=sp_ArtSongAF$energy,z=sp_ArtSongAF$valence)
rglwidget()

summary(sp_ArtSongAF)
# SO:- decide that MLR is not suitable => Binary Logistic Regression

#########################
### Logistic Regression
#########################

# dependent variable: is_pop
# independent variables: valence, danceability, energy, etc

# 80:20 split for test:train
samprate <- 0.8
numpoints <- nrow(sp_ArtSongAF)
ntest <- round((nrow(sp_ArtSongAF) * (1 - samprate)))
ntrain <- round(nrow(sp_ArtSongAF) - ntest)
ntest+ntrain==nrow(sp_ArtSongAF)

# rows for training set
blr_training <- sample(1:numpoints, round(samprate * numpoints), replace=FALSE)
# make training set
blr_train <- subset(sp_ArtSongAF[blr_training,],select=c(is_pop,acousticness,danceability,energy,
                                                     instrumentalness,liveness,loudness,speechiness,valence))
length(blr_train)
nrow(blr_train) # 11474

# rows for test set
blr_testing <- setdiff(1:numpoints,blr_training)
# make test set
blr_test <- subset(sp_ArtSongAF[blr_testing,],select=c(is_pop,acousticness,danceability,energy,
                                                   instrumentalness,liveness,loudness,speechiness,valence))
nrow(blr_test) #2868

# check train + test = original
nrow(blr_test) + nrow(blr_train) == nrow(sp_ArtSongAF)

binary_model <- glm(is_pop ~ acousticness+danceability+energy+
                    instrumentalness+liveness+loudness+speechiness+valence,
          family=binomial(link="logit"),
          data=blr_train)

summary(binary_model)

binomial_probabilities <- predict(binary_model,newdata=blr_test,type="response")
binomial_predictions <- ifelse(binomial_probabilities>0.5,1,0)

head(blr_test)
head(blr_train)
head(binomial_probabilities)
head(binomial_predictions)

# check test set lengths
length(binomial_predictions)==length(blr_test$is_pop)

# evaluate for accuracy
binomial_classification_error <- mean(binomial_predictions != blr_test$is_pop)

print(paste("Accuracy", 1-binomial_classification_error))
## 58.12% 


##
# redo model with fewer inputs
##

binary_model1 <- glm(is_pop ~ energy+
                      instrumentalness+loudness+speechiness+valence,
                    family=binomial(link="logit"),
                    data=blr_train)

summary(binary_model1)

binomial_probabilities <- predict(binary_model1,newdata=blr_test,type="response")
binomial_predictions <- ifelse(binomial_probabilities>0.5,1,0)

head(blr_test)
head(blr_train)
head(binomial_probabilities)
head(binomial_predictions)

# check test set lengths
length(binomial_predictions)==length(blr_test$is_pop)

# evaluate for accuracy
binomial_classification_error1 <- mean(binomial_predictions != blr_test$is_pop)

print(paste("Accuracy", 1-binomial_classification_error1))
## 57.98%




