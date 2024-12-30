
##
## 5. investigate Multiple Linear Regression
##    (popularity vs acoustic features)
##

# load files
library(tidyverse)
af <- read_tsv("acoustic_features.csv")
ly <- read_tsv("lyrics.csv")
sg <- read_tsv("songs.csv")
sp <- read_tsv("song_pop.csv")
art <- read_tsv("artists.csv")
sp <- read_tsv("song_pop.csv")
sc <- read_tsv("song_chart.csv")
cht <- read_tsv("artist_chart.csv")
trk <- read_tsv("tracks.csv")
rls <- read_tsv("releases.csv")
alb <- read_tsv("albums.csv")
art <- read_tsv("artists.csv")


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
ks.test(ss_ArtSongAF$tempo,"pnorm")

library(car)

# test for equality of variances: fligner
leveneTest(acousticness~energy, data=ss_ArtSongAF)
leveneTest(main_genre,cbind(danceability,energy,instrumentalness,liveness,loudness,speechiness,valence), data=ss_ArtSongAF)
fligner.test(acousticness~energy, data=ss_ArtSongAF)
fligner.test(danceability~liveness, data=ss_ArtSongAF)
fligner.test(speechiness~valence, data=ss_ArtSongAF)
fligner.test(liveness~loudness, data=ss_ArtSongAF)
fligner.test(danceability~loudness, data=ss_ArtSongAF)

# boxplots
aa <- ggplot(ss_ArtSongAF,aes(y=acousticness))+geom_boxplot()
dd <- ggplot(ss_ArtSongAF,aes(y=danceability))+geom_boxplot()
ee <- ggplot(ss_ArtSongAF,aes(y=energy))+geom_boxplot()
ii <- ggplot(ss_ArtSongAF,aes(y=instrumentalness))+geom_boxplot()
liv<- ggplot(ss_ArtSongAF,aes(y=liveness))+geom_boxplot()
lou<- ggplot(ss_ArtSongAF,aes(y=loudness))+geom_boxplot()
ss <- ggplot(ss_ArtSongAF,aes(y=speechiness))+geom_boxplot()
vv <- ggplot(ss_ArtSongAF,aes(y=valence))+geom_boxplot()
tt <- ggplot(ss_ArtSongAF,aes(y=tempo))+geom_boxplot()
grid.arrange(aa,dd,ee,ii,liv,lou,ss,vv,tt,nrow=3, top="Figure 7. Boxplots of distribution for acoustic features")
# only valence that has no outliers

# SO:- decide that MLR is not suitable => Binary Logistic Regression






