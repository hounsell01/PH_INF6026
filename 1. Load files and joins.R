
## 1. load files from tab delimited files

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
ac <- read_tsv("album_chart.csv")

library(dplyr)

###############################################
## song popularity

## join tables to get the acoustic features data by song and artist 
## build key data frame - ss_ArtSongAF

# add track ID from tracks
spt <- left_join(sp,trk,by="song_id")

# add in song popularity
spt_sg <- left_join(spt,sg,by="song_id")
spt_sg <- spt_sg[,-7]
spt_sg <- spt_sg[,-7]
spt_sg <- spt_sg[,1:7]

# add artist ID from releases
sstWa <- left_join(spt_sg,rls,by="album_id")

# trim columns and remove NA
sstWa <- sstWa[,1:8] # 25,193 rows
sstWa <- na.omit(sstWa) # 14,347 rows

# get artist names
ssta_art <- left_join(sstWa,art,by="artist_id")
ssta_art <- ssta_art[1:13]

# get song names
sstaa_sg <- left_join(ssta_art,sg,by="song_id")

# trim columns and remove NA
sstaa_sg <- sstaa_sg[1:14]

# add acoustic features
ss_ArtSongAF <- left_join(sstaa_sg,af,by="song_id")
View(ss_ArtSongAF)

# scale loudness to between 0 and 1
ss_ArtSongAFcp <- ss_ArtSongAF
ss_ArtSongAF$loudness
ss_ArtSongAF$loudness <- (ss_ArtSongAF$loudness-min(ss_ArtSongAF$loudness))/(max(ss_ArtSongAF$loudness)-min(ss_ArtSongAF$loudness))
ss_ArtSongAF$loudness

###############################################
## artist popularity

## join tables to get the acoustic features data by song and artist 
## build key data frame - sp_ArtSongAF


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
# remove 'followers'
sp_art <- sp_art[,-8]

# get song names
sp_artsng <- left_join(sp_art,sg,by="song_id")

# trim columns and remove NA
sp_artsng <- sp_artsng[,1:11]   # 14347 rows
sp_artsng <- na.omit(sp_artsng) # 14342 rows

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
sp_ArtSongAF$loudness

# check correlations with loudness before and after scaling
cor(sp_ArtSongAFcp$loudness, sp_ArtSongAF$danceability)
cor(sp_ArtSongAF$loudness, sp_ArtSongAF$danceability)


max_l <- arrange(sp_ArtSongAF,desc(duration_ms))
View(max_l)
View(sp_ArtSongAF)
