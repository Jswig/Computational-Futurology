# dtm.r
# finds words most and least frequently used by both Satoshi and Szabo in
# a sample of texts from both authors 
# then does the same using a tf-idf weighting

library(tm)
library(ggplot2)

# function for pre-processing of corpuses
clean_corpus <- function(corpus, remove_stop) {
	corpus <- tm_map(corpus, removePunctuation)
	corpus <- tm_map(corpus, toSpace, "’")
	corpus <- tm_map(corpus, toSpace, "‘")
	corpus <- tm_map(corpus, toSpace, " -")
	corpus <- tm_map(corpus, toSpace, "“")
	corpus <- tm_map(corpus, toSpace, "”")
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, removeNumbers)
	corpus <- tm_map(corpus, stripWhitespace)
	if(remove_stop) {
		corpus <- tm_map(corpus, removeWords, stopwords("english"))
	}
	corpus <- tm_map(corpus, stemDocument)

	return(corpus)
}


setwd("/home/joachim/Documents/SchoolWork/CRWN88")

#creates corpus of Satoshi's and Szabo's writings
satoshi <- Corpus(DirSource("/home/joachim/Documents/SchoolWork/CRWN88/Satoshi"))
szabo <- Corpus(DirSource("/home/joachim/Documents/SchoolWork/CRWN88/Szabo"))

# pre-processing
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
a <-readline("Remove stopwords for frequency analysis? (y/n) :")
if (a == "yes" | a == "y") {
	remove_stop <- TRUE
} else {
	remove_stop <- FALSE
}
satoshi <- clean_corpus(satoshi, remove_stop)
szabo <- clean_corpus(szabo, remove_stop)


# -------------------------
# Simple frequency analysis
# -------------------------

# builds and orders Document term matrices 
dtm_sza <- DocumentTermMatrix(szabo)
dtm_sat <- DocumentTermMatrix(satoshi)
freq_sat <- colSums(as.matrix(dtm_sat))
freq_sza <- colSums(as.matrix(dtm_sza)) 
ord_sza <- order(freq_sza, decreasing = TRUE)
ord_sat <- order(freq_sat, decreasing = TRUE)


# display 6 most and least frequently used words for satoshi and szabo
print("Words most used by Satoshi:")
print(freq_sat[head(ord_sat)])
print("Words least used by Satoshi:")
print(freq_sat[tail(ord_sat)])
print("Words most used by Szabo:")
print(freq_sza[head(ord_sza)])
print("Words least used by Szabo:")
print(freq_sza[tail(ord_sza)])

# print histograms of 6 most frequent words used by each author 
barplot(freq_sza[head(ord_sza)], main = "Words most used by Szabo", las = 2)
barplot(freq_sat[head(ord_sat)], main = "Words most used by Satoshi", las =2)


# --------------
# Tf-idf analysis
# --------------

tfidf_dtm_sat <- TermDocumentMatrix(satoshi, control = list(weighting = weightTfIdf))
tfidf_dtm_sza <- TermDocumentMatrix(szabo, control = list(weighting = weightTfIdf))
tfidf_freq_sat <- rowSums(as.matrix(tfidf_dtm_sat))
tfidf_freq_sza <- rowSums(as.matrix(tfidf_dtm_sza))
tfidf_ord_sat <- order(tfidf_freq_sat, decreasing = TRUE)
tfidf_ord_sza <- order(tfidf_freq_sza, decreasing = TRUE)

# display 6 word with highest tf-idf weight for each author
print("Words used by Satoshi with highest tf-idf weight:")
print(tfidf_freq_sat[head(tfidf_ord_sat)])
print("Words used by Szabo with highest tf-idf weight:")
print(tfidf_freq_sza[head(tfidf_ord_sza)])

# print histograms of 6  words with highest tf-idf weight for each author 
barplot(tfidf_freq_sat[head(tfidf_ord_sat)], main = "Words used by Satoshi with highest tf-idf weights", las = 2)
barplot(tfidf_freq_sza[head(tfidf_ord_sza)], main = "Words used by Szabo with highest tf-idf weights", las = 2)


# --------------------------------
# Hierarchical clustering analysis
# --------------------------------

# pre-processing
alltexts <- Corpus(DirSource("/home/joachim/Documents/SchoolWork/CRWN88/Alltexts"))

a <-readline("Remove stopwords for cluster analysis? (y/n) :")
if (a == "yes" | a == "y") {
	remove_stop <- TRUE
} else {
	remove_stop <- FALSE
}
alltexts <- clean_corpus(alltexts, remove_stop)

# builds dtm and prepares for clustering
dtm_all <- DocumentTermMatrix(alltexts)
m_all <- as.matrix(dtm_all)
write.csv(m_all, file = "dtmEight2Late.csv")
rownames(m_all) <- paste(substring(rownames(m_all),1,3),rep("..",nrow(m_all)), substring(rownames(m_all), nchar(rownames(m_all))-12, nchar(rownames(m_all))-4))
#compute distance between document vectors
d <- dist(m_all)
#run hierarchical clustering using Ward's algorithm
groups <- hclust(d,method="ward.D")

# plot dendogram
plot(groups, hang = -1)


