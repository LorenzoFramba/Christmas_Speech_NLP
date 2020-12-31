if (!require(rJava)) install.packages('rJava')
library(rJava)
.jinit(parameters="-Xmx4g") # If there are more memory problems, invoke gc() after the POS tagging

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(wordcloud)) install.packages('wordcloud')
library(wordcloud)

if (!require(NLP)) install.packages('NLP')
library(NLP)

if (!require(openNLP)) install.packages('openNLP')
library(openNLP)

if (!require(openNLPmodels.en)) install.packages('openNLPmodels.en')
library(openNLPmodels.en)

if (!require(tm)) install.packages('tm')
library(tm)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(viridis)) install.packages('viridis')
library(viridis)


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

#Barplot 
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
corpusRaw = Corpus(raw_text)

#####CLEANING UP THE TEXT 

##Lowering the letters 
corpus <- tm_map(corpus, content_transformer(tolower))

##removing stopwords()
myStopwords = c(stopwords(),"donald","trump","president", "Queen", 
                          "elizabeth", "pope", "francis", "melania" )

corpus = tm_map(corpus,removeWords,myStopwords)
corpus = tm_map(corpus,removeWords,stopwords())

#removing punctuation
corpus = tm_map(corpus,removePunctuation)

#removing spaces
corpus = tm_map(corpus,stripWhitespace)


#removing numbers
corpus = tm_map(corpus,removeNumbers)

#removing spaces
corpus = tm_map(corpus,stripWhitespace)

#stemming the document
#corpus = tm_map(corpus,stemDocument)

#showing differences 
corpusRaw$content
corpus$content

#Annotations


annotations = lapply(corpus, getAnnotationsFromDocument)

corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)

corpus.taggedText

#CLOUDS  

corpus.content <-corpus$content

new_corpus <- Corpus(VectorSource(corpus.content))

tdm = TermDocumentMatrix(new_corpus)

tdm.matrix <- as.matrix(tdm)
colnames(tdm.matrix) <- c("Pope","Putin","Queen","Trump")


#dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(new_corpus, random.order = FALSE, colors =brewer.pal(6,"Dark2"), max.words=100)

#dev.new(width = 1000, height = 1000, unit = "px")
comparison.cloud(tdm.matrix,random.order = FALSE, max.words=60)


#SENTIMENT ANALYSIS
words <- str_split(corpus.content, pattern="\\s+")

words.positives = unlist(lapply(words, function(x){sum(!is.na(match(x,positive)))}))
words.negatives = unlist(lapply(words, function(x){sum(!is.na(match(x,negative)))}))
words.total = unlist(lapply(words, function(x){sum(!is.na(match(x,positive)))  - sum(!is.na(match(x,negative))) } )) 


print("Mean of differences from positive and negative words")
mean(words.total)

print("Total differences from positive and negative words ")
words.total
print("Standard Deviation of total differences from positive and negative words ")
sd(words.total)


#PLOTTING THE BARPLOTS of the Sentiment Analysis

getBarplot(words.positives,"Positive words", "Greens", colnames(tdm.matrix))

getBarplot(words.negatives,"Negative words", "Reds", colnames(tdm.matrix))

getBarplot(words.total,"Positive - Negative words", "RdBu", colnames(tdm.matrix))


#FREQUENCY TABLE

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

