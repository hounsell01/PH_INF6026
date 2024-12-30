##
## 2. assess a song's musical genre against popularity
##

summary_stats <- sp_ArtSongAF %>%
  group_by(main_genre) %>%
  summarise(
    Mean_Popularity = mean(popularity.x),
    Median_Popularity = median(popularity.x),
    SD_Popularity = sd(popularity.x),
    Min_Popularity = min(popularity.x),
    Max_Popularity = max(popularity.x)
  )
summary_stats <- arrange(summary_stats,desc(Mean_Popularity))
ggplot(summary_stats,aes(x=reorder(main_genre, -Mean_Popularity), y=Mean_Popularity))+geom_count()

View(summary_stats)

# Boxplot to compare Popularity across Genre
ggplot(sp_ArtSongAF, aes(x = reorder(main_genre,-popularity.x), y = popularity.x)) +
  geom_boxplot() +
  labs(title = "Boxplots of Popularity Distribution by Genre",
      x = "Genre", y = "Popularity")+
      theme(axis.text.x=element_text(angle=90))+
      scale_x_discrete(breaks=sp_ArtSongAF$main_genre[seq(1,nrow(sp_ArtSongAF),by=25)])
)

# above is very very messy - try sampling out every nth row
gdf <- ss_ArtSongAF[seq(1,nrow(ss_ArtSongAF),by=15),] # 5,10 not much improvement
View(gdf)
ggplot(gdf, aes(x = reorder(main_genre,-popularity.y), y = popularity.y)) +
  geom_boxplot() +
  labs(title = "Figure 1. Boxplots of Artist Popularity Distribution by Genre (5% sample)", caption="MusicOsetData",
       x = "Genre", y = "Artist Popularity")+
  theme(axis.text.x=element_text(angle=90))+
  scale_x_discrete(breaks=sp_ArtSongAF$main_genre[seq(1,nrow(sp_ArtSongAF),by=5)])



# get distinct genres 
library(dplyr)
genre_df <- sp_ArtSongAF %>% distinct(sp_ArtSongAF$main_genre)

# give each genre a unique number
genre_df$genre_num <- c(1:304)

# rename column to match main df
nm <- names(genre_df)
nm[[1]] <- c("main_genre")
names(genre_df) <- nm

# join genre and number to main df
dataGenrenum <- left_join(ss_ArtSongAF,genre_df,by="main_genre")

# scatter plot genre vs popularity
ggplot(dataGenrenum,aes(x=genre_num,y=popularity.y))+geom_point()+
  labs(title="Figure 2. Scatter Plot Showing Music Genre and Popularity Score", caption="MusicOset Data",
       x="Genre", y="Popularity")

# most popular and least popular genres?

# Get the genre popularities
genre_by_popularityd <- decade_df %>%
  group_by(main_genre) %>%
  summarise(total_popularity = sum(popularity.y)) %>%
  arrange(desc(total_popularity)) %>%
  group_by(decade)

head(genre_by_popularity)
tail(genre_by_popularity)

# popularity by decade?
decade_df <- ss_ArtSongAF %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade)
View(decade_df)

genre_by_popularityd <- decade_df %>%
  group_by(decade, main_genre) %>%
  summarise(total_popularity = sum(popularity.y)) %>%
  arrange(desc(total_popularity))
View(genre_by_popularityd)

tdf <- genre_by_popularityd %>% arrange(desc(decade))
View(tdf)

tdf <- na.omit(tdf)
min(tdf$total_popularity)
max(tdf$total_popularity)

tdf <- arrange(tdf,desc(total_popularity))
tail(tdf)
View(tdf)

install.packages("viridis")
library(viridis)

# scrape top 5 by decade into excel and read back as .csv
most_pop <- read.csv("most pop genre by decade.csv")

# plot top 5 most popular genres by decade
data_m <- most_pop %>% mutate(Year_Genre = paste(Year,Total.Popularity, Genre, sep = " - "))  # Combine Year and Genre

plotm <- ggplot(data_m, aes(x = Year_Genre, y = Total.Popularity, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Figure 3. Most Popular - Total Popularity by Genre and Year",
       x = "Genre",
       y = "Total Popularity",
       fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)

# scrape bottom 5 by decade into excel and read back as .csv
least_pop <- read.csv("least pop genre by decade.csv")

# plot top 5 least popular genres by decade
data_l <- least_pop %>%
  mutate(Year_Genre = paste(Year,Total.Popularity, Genre, sep = " - "))  # Combine Year and Genre

plotl <- ggplot(data_l, aes(x = Year_Genre, y = Total.Popularity, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Genre")+
  labs(title = "Figure 4. Least Popular - Total Popularity by Genre and Year",
       x = "Genre", y = "Total Popularity", fill = "Year", caption="MusicOSet data") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0,100)+
  scale_color_viridis(discrete = TRUE, option = "B")+
  scale_fill_viridis(discrete = TRUE)

grid.arrange(plotm, plotl, nrow=2)

# k-s test for normality - genre & popularity
ks.test(dataGenrenum$genre_num,"pnorm")
ks.test(dataGenrenum$popularity.x,"pnorm")
# both are non-normal distributions

median(dataGenrenum$genre_num)
median(dataGenrenum$popularity.x)

# Mann-Whitney non-parametric test for comparison of distributions
# Ho is that  distributions are the same
wilcox.test(dataGenrenum$popularity.x, dataGenrenum$genre_num)
# p <  0.05 so H0 is not fulfilled: variances follow different distributions

# spearman rank sum coorrelation
cor(dataGenrenum$popularity.x,dataGenrenum$genre_num, method = "spearman", use="complete.obs")
# value = -0.388

# simple regression model - how does genre predict popularity?
model <- lm(popularity.x ~ genre_num, data=dataGenrenum)
summary(model)

?wilcox.test


# calculate IQR for AFs
iqr_a <- IQR(ss_ArtSongAF$acousticness)
iqr_d <- IQR(ss_ArtSongAF$danceability)
iqr_t <- IQR(ss_ArtSongAF$energy)
iqr_i <- IQR(ss_ArtSongAF$instrumentalness)
iqr_li <- IQR(ss_ArtSongAF$liveness)
iqr_lo <- IQR(ss_ArtSongAF$loudness)
iqr_s <- IQR(ss_ArtSongAF$speechiness)
iqr_v <- IQR(ss_ArtSongAF$valence)
iqr_t <- IQR(ss_ArtSongAF$tempo)

# outliers for acousticness
q1a <- quantile(ss_ArtSongAF$acousticness,0.25)
q3a <- quantile(ss_ArtSongAF$acousticness,0.75)
outliers_a <- subset(ss_ArtSongAF,ss_ArtSongAF$acousticness < (q1a - 1.5 * iqr_a) | ss_ArtSongAF$acousticness > (q3a + 1.5 * iqr_a))

# outliers for loudness
q1l <- quantile(ss_ArtSongAF$loudness,0.25)
q3l <- quantile(ss_ArtSongAF$loudness,0.75)
outliers_l <- subset(ss_ArtSongAF,ss_ArtSongAF$loudness < (q1l - 1.5 * iqr_li) | ss_ArtSongAF$loudness > (q3l + 1.5 * iqr_li))


