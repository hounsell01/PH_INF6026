
## 
## 4. further exploration of acoustic features
## 

install.packages("usethis")
library(usethis)

# look at songs and acoustic features
library(tidyverse)
library(ggplot2)

# join Songs with AcousticFeastures on song_id
songWfeat <- left_join(sg,af,by="song_id")
df <- songWfeat

# trim columns 
df <- df[,-2:-10]

# assess threshold for features = 0.5
df[3:11]>=0.5

# scale loudness to between 0 and 1
df$loudness <- (df$loudness-min(df$loudness))/(max(df$loudness)-min(df$loudness))

# assess threshold for features = 0.25 #
df[3:11]>=0.25

# get mean values
means <- c(mean(df$acousticness),mean(df$danceability),mean(df$energy),mean(df$instrumentalness),mean(df$liveness),mean(df$loudness),mean(df$speechiness),mean(df$valence),mean(df$tempo))
# acousticness,danceability,energy,instrumentalness,liveness,loudness,speechiness,valence,tempo
strg <- c("acousticness danceability energy instrumentalness liveness loudness speechiness valence tempo")
nms <- str_split(strg," ")

# what are mean values of features?
means <- data.frame(means)
means <- transpose(means) #vector
colnames(means) <- NULL
means

install.packages("gridExtra")
library(gridExtra)

# histograms of acoustic features 
a <- ggplot(df,aes(acousticness))+geom_histogram(colour="black",fill="white")
d <- ggplot(df,aes(danceability))+geom_histogram(colour="black",fill="white")
e <- ggplot(df,aes(energy))+geom_histogram(colour="black",fill="white")
i <- ggplot(df,aes(instrumentalness))+geom_histogram(colour="black",fill="white")
l <- ggplot(df,aes(liveness))+geom_histogram(colour="black",fill="white")
lo <- ggplot(df,aes(loudness))+geom_histogram(colour="black",fill="white")
s <- ggplot(df,aes(speechiness))+geom_histogram(colour="black",fill="white")
v <- ggplot(df,aes(valence))+geom_histogram(colour="black",fill="white")
t <- ggplot(df,aes(tempo))+geom_histogram(colour="black",fill="white")

grid.arrange(a,d,e,i,l,lo,s,v,t,ncol=3,top="Histogram plots of Acoustic Features")

# poss correlations?
ggplot(df, aes(x=danceability,y=tempo))+geom_point()
ggplot(df, aes(x=danceability,y=tempo))+geom_point()
ggplot(df, aes(x=energy,y=valence))+geom_point()
qqplot(df$energy,df$valence)
qqline(df$energy,df$valence)

# all correlations
cor(df$energy,df$valence) # 0.3561
cor(df$acousticness,df$liveness) # 0.02
cor(df$acousticness,df$danceability) # 0.2697


attach(df)
clm <- lm(acousticness~danceability+energy+instrumentalness+liveness+speechiness+valence)
plot(clm,which=1)
summary(clm)
detach(df)

# look at correlation coefficient matrix (pairwise)11
corr_df <- df[,1:9]
cor(corr_df)
# acousticness~energy -0.57
# acousticness~loudness -0.37
# danceability~valence +0.43
# energy~loudness +0.68
# loudness~speechiness +0.15

# plot these pairs
plot(acousticness,energy)
plot(acousticness,loudness)
plot(danceability,valence)
plot(energy,loudness)
plot(loudness,speechiness)

### correlations between acoustic features
### with tile heatmap

# Calculate correlation matrix
corr_matrix <- cor(sp_ArtSongAF[, c("acousticness", "danceability", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "valence")])

install.packages("corrplot")
library(corrplot)

# Plot correlation matrix as heatmap
corrplot(corr_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         title = "Figure 6. Correlation Matrix of Music Features")

# correlation plot in base R
heatmap(corr_matrix)

# create correlation heatmap in ggplot
# add library to melt df's
library(reshape)

# create df in {x1,x2,val} format
corr_melt <- melt(corr_matrix)

# copy melt df and round value to 2 DP
df <- corr_melt
df[,3] <- round((df[,3]), 2)

# ggplot heatmap
ggplot(data = df, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() +
  geom_text(aes(X2, X1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
        theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())+
        labs(title="Correlation Plots of Acoustic Features", caption="MusicOSet data")

# alternative colour scheme 1
ggplot(df,aes(x=X1,y=X2))+geom_tile(aes(fill=value))+
  geom_text(aes(label=value),colour="#59788E")+
  scale_fill_gradient2(low = "#281E5D", high = "#48AAAD",
                       limit = c(-1,1), name="Correlation") +
    labs(title="Correlation Plots of Acoustic Features", caption="MusicOSet data")

# alternative colour scheme 2
ggplot(df,aes(x=X1,y=X2))+geom_tile(aes(fill=value))+
  geom_text(aes(label=value),colour="#59788E")+
  scale_fill_gradient2(low = "#FCAE1E", high = "#BC544B",
                       limit = c(-1,1), name="Correlation") +
  labs(title="Figure 6. Correlation Plots of Acoustic Features", caption="MusicOSet data")

# alternative colour scheme 3, let system pick boundaries
ggplot(df,aes(x=X1,y=X2))+geom_tile(aes(fill=value))+
  geom_text(aes(label=value),colour="white")+
  labs(title="Figure 7. Correlation Plots of Acoustic Features", caption="MusicOSet data")

af_summ <- summary(ss_ArtSongAF[,19:27])



        