#Load the required packages
library(tm)
library(wordcloud)
library(RColorBrewer)


#Load the required Data
setwd('~/Coding/R/RahulArnab/')
rawData = read.csv('RahulGandhiArnab.csv')

write.csv(rawData,'InterviewData.csv')
#Initializing the variable to store their respective conversation
RahulData = ''
ArnabData = ''

for(i in rawData[,1]){

  if(length(grep('^Rahul:',i)) > 0){
    RahulData <- c(RahulData,i)
  } else if(length(grep('^Arnab:',i)) > 0 ){
    ArnabData <- c(ArnabData,i)
  }
 
}

#World cloud generating function. Also provides the top 5 words
wordcloudGenerator <- function(name,conversation,stopwords){

  # build a corpus
  mydata.corpus <- Corpus(VectorSource(conversation))
  
  # make each letter lowercase
  mydata.corpus <- tm_map(mydata.corpus, tolower)
  
  mydata.corpus <- tm_map(mydata.corpus, removeNumbers)
  
  # remove punctuation
  mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
  
  # remove generic and custom stopwords
  my_stopwords <- c(stopwords('english'),stopwords)
  
  mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
  
  # build a term-document matrix
  tdm <- TermDocumentMatrix(mydata.corpus)
  tdm <- removeSparseTerms(tdm, sparse=0.999)
  
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, "PuBuGn")
  pal <- pal[-(1:2)]
  png(paste(name,"wordcloud.png",sep=""), width=1280,height=800)
  wordcloud(d$word,d$freq, colors=pal)
  dev.off() 
  print(d[1:5,])
}




#Generate their repective word clouds
stopwords <- c( 'rahul','arnab','going','gandhi','want','question','can','say','will','said','narendra')
wordcloudGenerator('Arnab',ArnabData,stopwords)
wordcloudGenerator('Rahul',RahulData,stopwords)
