library(rJava)
.jinit(parameters="-Xmx4g")
# If there are more memory problems, invoke gc() after the POS tagging
library(ggplot2)
library(wordcloud)
library(NLP) 
library(openNLP) 
library(openNLPmodels.en)
library(tm)
library(stringr)
install.packages("viridis")  # Install
library("viridis") 
library(color.hist)

#getAnnotationsFromDocument returns annotations for the text document: word, sentence, part-of-speech, and Penn Treebank parse annotations.
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- NLP::annotate(x, pos_tag_annotator, y1)
  return(y2)  
} 

#getAnnotatedMergedDocument returns the text document merged with the annotations.

getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
} 

#getAnnotatedPlainTextDocument returns the text document along with its annotations in an AnnotatedPlainTextDocument.

getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
}



getBarplot = function(data, text, style, names){
  barplot(data, 
        col = rev(brewer.pal(n = 4, name = style)), 
        border="black", 
        names.arg = names,
        ylab = text
        )
  }



getwd()

raw_text <- DirSource("Speeches", encoding = "UTF-8" )

positive <- scan('positive-words.txt', what='character', comment.char = ";")
negative = scan('negative-words.txt', what='character', comment.char = ";")

corpus = Corpus(raw_text)

corpus
#####CLEANING UP THE TEXT 

##Lowering the letters 
corpus <- tm_map(corpus, content_transformer(tolower))

##removing stopwords()
myStopwords = c(stopwords(),"donald","trump","president", "Queen", 
                          "elizabeth", "pope", "francis", "together", 
                            "december", "'s", "'re", "“", "”", "’s")
corpus = tm_map(corpus,removeWords,myStopwords)

corpus = tm_map(corpus,removeWords,stopwords())

#removing punctuation
corpus = tm_map(corpus,removePunctuation)


#removing numbers
corpus = tm_map(corpus,removeNumbers)

#removing spaces
corpus = tm_map(corpus,stripWhitespace)

#stemming the document
#corpus = tm_map(corpus,stemDocument)

#corpus <- gsub(pattern="\\b[A-z]\\b(1)", replace =" ", corpus)

####COMPARISON CLOUD  

corpus.content <-corpus$content

new_corpus <- Corpus(VectorSource(corpus.content))

tdm = TermDocumentMatrix(new_corpus)

tdm.matrix <- as.matrix(tdm)
colnames(tdm.matrix) <- c("Pope","Putin","Queen","Trump")

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(new_corpus, random.order = FALSE, colors = rainbow(10))

dev.new(width = 1000, height = 1000, unit = "px")
comparison.cloud(tdm.matrix,random.order = FALSE, max.words=60)


####Sentiment Analysis
words <- str_split(corpus.content, pattern="\\s+")

words.positives = unlist(lapply(words, function(x){sum(!is.na(match(x,positive)))}))
words.negatives = unlist(lapply(words, function(x){sum(!is.na(match(x,negative)))}))

words.total = unlist(lapply(words, function(x){sum(!is.na(match(x,positive)))  - sum(!is.na(match(x,negative))) } )) 

mean(words.total)
words.total
sd(words.total)

#Plotting the BarPlots of the Sentiment Analysis

getBarplot(words.positives,"Positive words", "Greens", colnames(tdm.matrix))

getBarplot(words.negatives,"Negative words", "Reds", colnames(tdm.matrix))

getBarplot(words.total,"Positive - Negative words", "RdBu", colnames(tdm.matrix))


#Annotations

annotations = lapply(corpus, getAnnotationsFromDocument)

corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)

corpus.taggedText

###Frequency table

freq=rowSums(as.matrix(tdm))
high.freq=tail(sort(freq),n=20)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq,  fill = names) ) +
  geom_bar(stat="identity") + coord_flip() +
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies") + 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,color="#535353",size=16)) +
  theme(axis.text.x=element_text(size=12,color="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,color="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,color="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,color="#535353",face="bold",vjust=-.5)) +
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))+
  theme(legend.position="none")
  
#Creating a wordcloud

Christmas_corpus_split <- VectorSource(corpus)
Christmas_corpus_split <- VCorpus(Christmas_corpus_split)

Christmas_dtm <- DocumentTermMatrix(Christmas_corpus_split) 

Christmas_dtm.mx = as.matrix(Christmas_dtm)

Christmas_frequency <- colSums(Christmas_dtm.mx)
Christmas_frequency <- sort(Christmas_frequency, decreasing=TRUE)
Christmas_frequency[1:10] # list 10 most frequent terms

Christmas_wf = data.frame(term=names(Christmas_frequency),occurrences=Christmas_frequency)
Christmas_wf <- Christmas_wf[order(-Christmas_wf$occurrences), ] 
Christmas_wf <- Christmas_wf[1:10,] # produce a dataframe with 10 most frequent terms 

Christmas_words <- names(Christmas_frequency)
wordcloud(Christmas_words[1:50], Christmas_frequency[1:50], colors=brewer.pal(6,"Dark2"),random.order = FALSE )




