
# removing all the environment variables before starting the R code.

########### This code is by Jose Antonio####################

rm(list=ls(all=TRUE))

# Handling missing values in movies dataset
NAvalues_movies = sum(is.na(movies))
colSums(is.na(movies))

movies<-centralImputation(movies)

#Handling missing values in credits dataset
NAvalues_credit = sum(is.na(credits))
colSums(is.na(credits))

####################################################################################################
# Below code is to finding the highest frequency genres by forming a word cloud and making a list 
####################################################################################################
genre_wc=as.vector(movies$genres)
genre_wc <- Corpus(VectorSource(movies$genres))
#inspect(genre_wc)

toSpace <- content_transformer(function (x, pattern ) gsub(pattern, " ", x))
genre_wc <- tm_map(genre_wc, toSpace, "/")

genre_wc <- tm_map(genre_wc, toSpace, "@")
genre_wc <- tm_map(genre_wc, toSpace, "\\|")

genre_wc <- tm_map(genre_wc, content_transformer(tolower))
genre_wc <- tm_map(genre_wc, removeNumbers)

genre_wc <-tm_map(genre_wc, removeWords, c("name"))

genre_wc <- tm_map(genre_wc, removeWords, stopwords("english"))


genre_wc <- tm_map(genre_wc, removePunctuation)
genre_wc <- tm_map(genre_wc, stripWhitespace)


dtm <- TermDocumentMatrix(genre_wc)
m <- as.matrix(dtm)

v <- sort(rowSums(m),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)
head(d, 30)

wordcloud(words = d$word, freq = d$freq, min.freq = 5, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 4)

# making a list
frequent_genre=vector()
frequent_genre=findFreqTerms(dtm, lowfreq = 4)
frequent_genre=as.data.frame(frequent_genre)

write.csv(frequent_genre,file="frequent_genre.csv")


########### This code is by Jose Antonio####################

############################################################################################################
# Below code is to finding the highest frequency Keywords by forming a word cloud and making a list
############################################################################################################

keywords_wc=as.vector(movies$keywords)
keywords_wc <- Corpus(VectorSource(movies$keywords))
inspect(keywords_wc)

toSpace <- content_transformer(function (x, pattern ) gsub(pattern, " ", x))
keywords_wc <- tm_map(keywords_wc, toSpace, "/")

keywords_wc <- tm_map(keywords_wc, toSpace, "@")
keywords_wc <- tm_map(keywords_wc, toSpace, "\\|")

keywords_wc <- tm_map(keywords_wc, content_transformer(tolower))
keywords_wc <- tm_map(keywords_wc, removeNumbers)

keywords_wc <-tm_map(keywords_wc, removeWords, c("name"))

keywords_wc <- tm_map(keywords_wc, removeWords, stopwords("english"))


keywords_wc <- tm_map(keywords_wc, removePunctuation)
keywords_wc <- tm_map(keywords_wc, stripWhitespace)


dtm_kw <- TermDocumentMatrix(keywords_wc)
m_kw <- as.matrix(dtm_kw)

v <- sort(rowSums(m_kw),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)
head(d, 30)

wordcloud(words = d$word, freq = d$freq, min.freq = 2, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm_kw, lowfreq = 4)

kw_with_freq=d

write.csv(kw_with_freq,file = 'frequent_keywords.csv')



########### This code is by Jose Antonio####################
#########################################################################################################
#After pulling out the list of Keywords in the order of Highest Frequencies,
#we are cleaing them by a approach of Synonyms- Which is  joining more than one words with same meaning


# For this Synonyms cleaning we worked on Tableau and cleaned all the key words and saved in a CSV with all the Frequencies in descending order

# Here after Synonym approach the Keywords count has drasctically decreased (which helped us a lot.)

##########################################################################################################

syn_cleaned_kw=read.csv("After_Synonyms_Combied Frequencies.csv",header = TRUE,sep= ",")

syn_cleaned_kw=data.frame(syn_cleaned_kw)
new_kw_feq=data.frame()


for(i in 1:nrow(syn_cleaned_kw))
{if(syn_cleaned_kw[i,2]>3){new_kw_feq=rbind(new_kw_feq,syn_cleaned_kw[i,])}}

# converting it to numeric and arranging in decresing order

new_kw_feq$Frequency=as.numeric(new_kw_feq$Frequency)
new_kw_feq=new_kw_feq[order(-new_kw_feq[,2]), ]

# removing row names and writing it to a csv to use it in the final script to build a matrix.
rownames(new_kw_feq) <- NULL;
write.csv(new_kw_feq,file = "KeyWords_Engine.csv")

####################################################################################################################################
####################################################################################################################################
