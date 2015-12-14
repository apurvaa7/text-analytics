# install.packages("stringdist")
# install.packages("dplyr")
# install.packages("tm")
# install.packages("Matrix")
# install.packages("scales")
# install.packages("SnowballC")
#install.packages("lsa")

#read user question passed from UI
#args = commandArgs(trailingOnly = FALSE)
#inputquestion <- as.character(args)
#sample question
inputquestion <- "shouldn't marijuana/cigarettes be legalized? it is safer than alcohol and not addictive and dangerous"

load_libraries <- function(){
  library(RPostgreSQL)
  library(tm)
  library(Matrix)
  library(dplyr)
  library(MASS)
  library(stringdist)
  library(scales)
  library(SnowballC)
  library(lsa)
}
suppressMessages(load_libraries())

#function to create and clean corpus
clean_corpus <- function(text_source){
  myCorpus <- Corpus(VectorSource(text_source))
  
  ## Preprocessing  
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))  # *Converting to lowercase:*   
  myCorpus <- tm_map(myCorpus, removeWords, c("alcohol", "alcoholic","alcoholics","alcoholism","related","please","drink"))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))   # *Removing "stopwords"
  myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace))   # *Stripping whitespace 
  # stem words
  myCorpus <- tm_map(myCorpus, stemDocument)
  return(myCorpus)
}


#process input question
#replace punctuations with spaces, remove letters and apostrophes with empty strings
inputquestion <- gsub("[^a-zA-Z[:space:]']", " ", inputquestion)
inputquestion <- gsub("[']", "", inputquestion)
cleanedinputcorpus <- clean_corpus(inputquestion)
inputdtm <- DocumentTermMatrix(cleanedinputcorpus)
inputmat <- as.matrix(inputdtm)
inputterms <- colnames(inputmat)
inputquestion_cleaned <- data.frame(text=unlist(sapply(cleanedinputcorpus,`[`,"content")), stringsAsFactors=F)


#get questions data from csv
questionsdf <- read.csv("cleanedquestions.csv",stringsAsFactors = FALSE)

#append input question
completequestions <- append(questionsdf$completequestion,inputquestion)


cleanedquestioncorpus <- clean_corpus(completequestions)
dtm <- DocumentTermMatrix(cleanedquestioncorpus)
#remove empty documents
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]
empty.rows <- dtm[rowTotals == 0, ]$dimnames[1][[1]]
cleanedquestioncorpus <- cleanedquestioncorpus[-as.numeric(empty.rows)]
questionsdf <- questionsdf[-as.numeric(empty.rows),]

#write tf-idf matrix for completequestion
dtm_tfxidf <- weightTfIdf(dtm.new,normalize=TRUE)       
tfidf_completequestions <- as.matrix(dtm_tfxidf)
#remove sparse terms
tfidf_completequestions <- drop0(tfidf_completequestions)

#write cleaned questions
cleaned_completequestions <- data.frame(text=unlist(sapply(cleanedquestioncorpus,`[`,"content")), stringsAsFactors=F)

#write term-document matrix for complete questions
#find adjacent word pairs
BigramTokenizer <- function(x) { 
  unlist(
    lapply(ngrams(words(x), 2), paste, collapse = " "), 
    use.names = FALSE
  ) 
}
numrows <- nrow(tfidf_completequestions)

tdm <- TermDocumentMatrix(cleanedquestioncorpus, control = list(tokenize = BigramTokenizer))
tdm_completequestions <- as.matrix(tdm[,-numrows])


#tf-idf cosine similarity
cosine_sim <- vector()
for(i in 1:(numrows - 1)){
  cosine_sim[i] <- cosine(as.vector(unname(tfidf_completequestions[i,])),
                          as.vector(unname(tfidf_completequestions[numrows,])))
}

#cosine stringdist
cos_dist <- rep(0,numrows-1)
for (i in 1 :(numrows - 1)){
  cos_dist[i] <- stringdist(cleaned_completequestions[i,1],inputquestion_cleaned[1,1],method = "cosine")
}

#tf-idf for matching terms
matchingmatrixsubset <- tfidf_completequestions[c(1:(numrows-1)),colnames(tfidf_completequestions) %in% inputterms]
if(class(matchingmatrixsubset) != "numeric"){
  totalmatchingtfidf <- rowSums(matchingmatrixsubset)
}else{
  totalmatchingtfidf <- matchingmatrixsubset
}

#tf-idf for  non-matching terms
notmatchingsubset <- tfidf_completequestions[c(1:(numrows-1)),!colnames(tfidf_completequestions) %in% inputterms]
if(class(notmatchingsubset) != "numeric"){
  notmatchingtfidf <- rowSums(notmatchingsubset)
}else{
  notmatchingtfidf <- notmatchingsubset
}

#find adjacent word pairs

inputtdm <- TermDocumentMatrix(cleanedinputcorpus, control = list(tokenize = BigramTokenizer))
inputtdmatrix <- as.matrix(inputtdm)

adjacentwordscore <- tdm_completequestions[1,]
adjacentwordscore[] = 0
if(nrow(inputtdmatrix) > 0)
{
for(i in 1:nrow(inputtdmatrix)){
  if(is.element(rownames(inputtdmatrix)[i],rownames(tdm_completequestions))){
    adjacentwordscore <- adjacentwordscore + tdm_completequestions[rownames(inputtdmatrix)[i],]
  }
}
}
#
#calculate total scores
if(length(inputterms) > 7)
{
nettfidfscores <- totalmatchingtfidf - notmatchingtfidf
}else{
  nettfidfscores <- totalmatchingtfidf
}
netscores <- nettfidfscores  + rescale(adjacentwordscore) + cosine_sim - cos_dist
#find top matches
topmatches <- tail(order(netscores),10)
topmatches <- rev(topmatches)
qids <- questionsdf$qid[topmatches]


#write top 10 qids to text file for Python
write(qids, "BEST_QIDS.txt", sep="\n")


Sample output:
Top 10 closest question IDs: 
20110124220548AAyEsBx
20140115163929AAAqmcd
20140101115640AAbg7Yh
20140112131720AAetZdZ
20140131153302AAyoMRg
20110412104950AAhjNd8
20120602111408AAcEyXm
20110901085741AAQYTfD
20130409003958AAEjERM
20131229204414AA8wLOl
