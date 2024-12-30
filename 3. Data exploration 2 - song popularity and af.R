##
## 3. look at songs and acoustic features
##

library(tidyverse)
library(ggplot2)

# join 'Songs' with 'Acousticfeatures' on 'song_id'
songWfeat <- left_join(sg,af,by="song_id")

# cut out extraneous columns
afdf <- songWfeat[,3:20]

# scale loudness to between 1 and 0
afdf$loudness <- (afdf$loudness-min(afdf$loudness))/(max(afdf$loudness)-min(afdf$loudness))
# assess threshold for features = 0.5
afdf[1:8]>=0.5

# assess threshold for features = 0.25
afdf[1:8]>=0.25

hist(afdf$acousticness)

means <- c(mean(afdf$acousticness),mean(afdf$danceability),mean(afdf$energy),mean(afdf$instrumentalness),mean(afdf$liveness),mean(afdf$loudness),mean(afdf$speechiness),mean(afdf$valence),mean(afdf$tempo))
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
a <- ggplot(afdf,aes(acousticness))+geom_histogram(colour="black",fill="white")
d <- ggplot(afdf,aes(danceability))+geom_histogram(colour="black",fill="white")
e <- ggplot(afdf,aes(energy))+geom_histogram(colour="black",fill="white")
i <- ggplot(afdf,aes(instrumentalness))+geom_histogram(colour="black",fill="white")
l <- ggplot(afdf,aes(liveness))+geom_histogram(colour="black",fill="white")
ll <- ggplot(afdf,aes(loudness))+geom_histogram(colour="black",fill="white")
s <- ggplot(afdf,aes(speechiness))+geom_histogram(colour="black",fill="white")
v <- ggplot(afdf,aes(valence))+geom_histogram(colour="black",fill="white")
t <- ggplot(afdf,aes(tempo))+geom_histogram(colour="black",fill="white")

grid.arrange(a,d,e,i,l,ll,s,v,t,ncol=3, top="Figure 5. Histogram Plots of Acoustic Features")

# poss correlations?
ggplot(afdf, aes(x=danceability,y=tempo))+geom_point()
ggplot(afdf, aes(x=danceability,y=tempo))+geom_point()
ggplot(afdf, aes(x=energy,y=valence))+geom_point()
qqplot(afdf$energy,df$valence)
qqline(afdf$energy,df$valence)

# all correlations
cor(afdf$energy,afdf$valence) # 0.3561
cor(afdf$acousticness,afdf$liveness) # 0.02
cor(afdf$acousticness,afdf$danceability) # 0.2697

# look at correlation coefficient matrix (pairwise)
corr_df <- afdf[,10:18]
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

###################################################
# 2a. weeks on chart as a measure for popularity?
###################################################

library(dplyr)

# what song has been longest on chart? SongChart table
arrange(sc,desc(weeks_on_chart))
max(sc$weeks_on_chart)
sc

# join Tracks and Releases to get Artist
scWtr <- left_join(sc,trk,by="song_id") 
sWtrWrel <- left_join(scWtr, rls,by="album_id")
srtWa <- left_join(sWtrWrel,art,by="artist_id")
srtaWsg <- left_join(srtWa,sg,by="song_id")
# all have 250,392 rows, same as SongChart

# artist with most weeks on chart
srtWa1 <- na.omit(srtaWsg) # 153,853 rows
View(srtWa1)
View(srtWa1 %>% group_by(artist_id))

# look at album 
arrange(ac,desc(week_count))
acWalb <- left_join(ac,alb,by="album_id")
aaWrel <- left_join(acWalb,rls,by="album_id")
aaWrel <- left_join(aaWrel,art,by="artist_id")
arrange(aarWart,desc(week_count))
View(aaWrel)


###################################################

cht_new <- cht
arrange(cht,weeks_on_chart,rank_score)
arrange(cht,weeks_on_chart,week)
cht_new <- select(cht,contains("2017"))

# get 2017 from artist_chart
cht2017 <- with(cht, cht[(week >= "2017-01-01" & week <= "2017-12-31") , ])
tail(cht2017)
# sort by weeks on chart (desc)
arrange(cht2017,desc(weeks_on_chart))

# get 1967 from artist_chart
cht1967 <- with(cht, cht[(week >= "1967-01-01" & week <= "1967-12-31") , ])
arrange(cht1967,desc(weeks_on_chart))
top1967 <- arrange(cht1967,desc(week))

# get top 10 weeks on chart for last week of 1967
topten67 <- top1967[1:10,]
topten67

# add artist
topten67 <- left_join(topten67,art,by="artist_id")
topten67 <- topten67[,1:8]

### song_chart
sc
sc2017 <- with(sc, sc[(week >= "2017-01-01" & week <= "2017-12-31") , ])
# sort by weeks on chart (desc)
arrange(sc2017,desc(week),desc(weeks_on_chart))

# get top eleven songs in terms of weeks on chart for last week of 2017
topsc2017 <- arrange(sc2017,desc(week),desc(weeks_on_chart))
topsc2017 <- topsc2017[1:11,]

# add album ID from 'tracks'
topalb2017 <- left_join(topsc2017,trk,by="song_id")

# trim superfluous columns
topalb2017 <- topalb2017[,1:6]

# add artist ID from 'releases'
topalb2017 <- left_join(topalb2017,rls,by="album_id")

# trim superfluous columns
topalb2017 <- topalb2017[,1:7]

# remove row 9 - NA 
topalb2017a <- topalb2017[1:8,]
topalb2017b <- topalb2017[10:11,]
topalb2017c <- rbind(topalb2017a,topalb2017b)

# add artist
topalb2017 <- topalb2017c
topalb2017 <- left_join(topalb2017,art,by="artist_id")

# trim superfluous columns
topalb2017 <- topalb2017[,1:8]

# add acoustic features
topalb2017 <- left_join(topalb2017,af,by="song_id")
view(topalb2017)
# trim superfluous columns
topalb2017 <- topalb2017[,1:7]

# get sum of each column of af
sum17 <- 0
sum17
sum(topalb2017$acousticness)

# add % acousticness total column
append(topalb2017,(topalb2017$acousticness / sum(topalb2017$acousticness) * 100))

sum(topalb2017$weeks_on_chart)

# add column for %weeksonchart
topalb2017a <- topalb2017 %>% mutate(pc_weeks_on_chart=topalb2017$weeks_on_chart / sum(topalb2017$weeks_on_chart) * 100) 
View(topalb2017a)


##################
# songpop
##################

sp # 25193 rows

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
# take out columns key, mode, time_signature, tempo
sp_ArtSongAF <- sp_ArtSongAF[-21]

library(gridExtra)
#########################
# scatter plots:        #
#########################

# danceability v liveness
a <- ggplot(sp_ArtSongAF,aes(x=acousticness))+geom_histogram()
b <- ggplot(sp_ArtSongAF,aes(x=danceability))+geom_histogram()
c <- ggplot(sp_ArtSongAF,aes(x=energy))+geom_histogram()
d <- ggplot(sp_ArtSongAF,aes(x=instrumentalness))+geom_histogram()  # skip this one
e <- ggplot(sp_ArtSongAF,aes(x=liveness))+geom_histogram()
f <- ggplot(sp_ArtSongAF,aes(x=loudness))+geom_histogram()
g <- ggplot(sp_ArtSongAF,aes(x=speechiness))+geom_histogram() # skip this one
h <- ggplot(sp_ArtSongAF,aes(x=valence))+geom_histogram()
grid.arrange(a,b,c,d,e,f,g,h,nrow=2)

# scale loudness to between 0 and 1
sp_ArtSongAF$loudness
mean(sp_ArtSongAF$loudness)
min(sp_ArtSongAF$loudness)
max(sp_ArtSongAF$loudness)
sp_ArtSongAFcp <- sp_ArtSongAF



# scatter plots, not including instrumentalness and speechiness
s_ad <- ggplot(sp_ArtSongAF,aes(x=acousticness,y=liveness))+geom_point()
s_ae <- ggplot(sp_ArtSongAF,aes(x=acousticness,y=energy))+geom_point()
s_ali<- ggplot(sp_ArtSongAF,aes(x=acousticness,y=liveness))+geom_point()
s_alo<- ggplot(sp_ArtSongAF,aes(x=acousticness,y=loudness))+geom_point()
s_av <- ggplot(sp_ArtSongAF,aes(x=acousticness,y=valence))+geom_point()
s_de <- ggplot(sp_ArtSongAF,aes(x=danceability,y=energy))+geom_point()
s_dli<- ggplot(sp_ArtSongAF,aes(x=danceability,y=liveness))+geom_point()
s_dlo<- ggplot(sp_ArtSongAF,aes(x=danceability,y=loudness))+geom_point()
s_dv <- ggplot(sp_ArtSongAF,aes(x=danceability,y=valence))+geom_point()
s_eli<- ggplot(sp_ArtSongAF,aes(x=energy,y=liveness))+geom_point()
s_elo<- ggplot(sp_ArtSongAF,aes(x=energy,y=loudness))+geom_point()
s_ev <- ggplot(sp_ArtSongAF,aes(x=energy,y=valence))+geom_point()
s_lilo<- ggplot(sp_ArtSongAF,aes(x=liveness,y=loudness))+geom_point()
s_liv<- ggplot(sp_ArtSongAF,aes(x=liveness,y=valence))+geom_point()
s_lov<- ggplot(sp_ArtSongAF,aes(x=loudness,y=valence))+geom_point()

grid.arrange(s_ad,s_ae,s_ali,s_alo,s_av,s_de,s_dli,s_dlo,s_dv,s_eli,s_elo,s_ev,s_lilo,s_liv,s_lov,nrow=3)

s_dlo
s_elo

### look at is_pop variable
(filter(sp,is_pop==TRUE) %>% nrow())/(filter(sp,is_pop==FALSE) %>% nrow())

sp_ArtSongAF
# plot year end score v popularity and colour by year
# shows bias in popularity scores - more recent scores are higher
ggplot(sp_ArtSongAF, aes(x=year_end_score,y=popularity.x, colour=year))+geom_point()

# how many 'main' genres?
gr <-sp_ArtSongAF$main_genre
length(gr)

sp_ArtSongAF %>% group_by(main_genre) %>% summarise(n()) # 304 genres, plus -
test <- sp_ArtSongAF

# plot genres against popularity
ggplot(sp_ArtSongAF,aes(x=main_genre))+geom_bar()+
  theme(axis.text.x=element_text(angle=90))+
  scale_x_discrete(breaks=function(x) x[seq(1,length(x),by=5)])

# group by main_genre and order by sum of popularity
genre_pop <- sp_ArtSongAF
genre_pop %>% arrange(main_genre) # 14347
gp <- genre_pop %>% group_by(main_genre) %>% summarise(total_pop=sum(popularity.x)) %>% arrange(desc(total_pop))

# take top 50 values and plot
gp1 <- gp[1:50,]
gp1 <- arrange(desc(total_pop))
ggplot(gp1,aes(x=main_genre,y=total_pop))+geom_col()+
  theme(axis.text.x=element_text(angle=90))
  
ggplot(gp1,aes(x=reorder(main_genre, -total_pop),y=total_pop))+geom_col()+
  theme(axis.text.x=element_text(angle=90))

scale_x_discrete(breaks=function(x) x[seq(1,length(x),by=5)])

# remove '-' from df
nrow(test) #14347
test1 %>% test %>% drop_na()
nrow(test1) #14342
View(test1)

