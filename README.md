# Christmas_Speech_NLP

At the beginning of the code, all the packages will be instantly installed with the following commands :  


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



To make the program run correctly, just go on the project folder, open the terminal and run it with:
```bash
Rscript NLP.R
```

All the visualization results will be instantly saved in the folder itself. 


You also have the option to open RStudio and run it manually, line by line and follow all the comments of the code.



