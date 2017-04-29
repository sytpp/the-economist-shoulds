require(RCurl)
require(XML)

articleURLs <- read.delim("articleURL.txt",header=F)
shouldDF <- data.frame()

for(i in 339:dim(articleURLs)[1]){
  date <- articleURLs[i,"V1"]
  link <- articleURLs[i,"V2"]
  print (i)
  webpage <- getURL(articleURLs[i,"V2"])
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  doc <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
  
  if(!is.null(xmlRoot(doc))) {
    
    doc.text = unlist(xpathApply(doc, "//article/div[@class='main-content']",xmlValue))
    # Replace all \n by spaces
    doc.text = gsub('\\n', ' ', doc.text)
    
    m <- list(-1)
    m <- gregexpr("[^.!?]+[ \t\r\n\v\f]should[ \t\r\n\v\f][^.!?]+", doc.text, perl=T) 
    if(unlist(m)[1]>=0){
      shoulds <- unlist(regmatches(doc.text, m))
      shoulds <- gsub("^ *","",shoulds)
      who_should <- sapply(strsplit(shoulds, "should"),"[[",1)
      should_what <- sapply(strsplit(shoulds, "should"),"[[",2)
      shouldDF <- rbind.data.frame(shouldDF, data.frame(date = date, who = who_should, what = should_what, link = link))      
    }
  }
}

# side1 <- shouldDF
side2 <- shouldDF
shouldDF <- rbind.data.frame(side1, side2)

## PLOTs
library(ggplot2)
ggplot(ddply(shouldDF, .(date), nrow), aes(date, V1)) + geom_bar(stat="identity") 

shouldDF$helper <- 1
shouldDF <- ddply(shouldDF, .(date), mutate, counter=1:sum(helper))

shouldDF$flag <- "ETC"
shouldDF[sapply(gregexpr("\\/news\\/letters\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "LETTERS"
shouldDF[sapply(gregexpr("\\/news\\/briefing\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "BRIEFING"
shouldDF[sapply(gregexpr("\\/news\\/obituary\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "OBITUARY"
shouldDF[sapply(gregexpr("\\/news\\/international\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "INTERNATIONAL"
shouldDF[sapply(gregexpr("\\/news\\/asia\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "ASIA"
shouldDF[sapply(gregexpr("\\/news\\/americas\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "AMERICAS"
shouldDF[sapply(gregexpr("\\/news\\/china\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "CHINA"
shouldDF[sapply(gregexpr("\\/news\\/europe\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "EUROPE"
shouldDF[sapply(gregexpr("\\/news\\/britain\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "BRITAIN"
shouldDF[sapply(gregexpr("\\/news\\/business\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "BUSINESS"
shouldDF[sapply(gregexpr("\\/news\\/leaders\\/", shouldDF$link, perl=T),"[[",1)>0,"flag"] <- "LEADERS"


write.table(shouldDF, file="D3_Vis/EcoSHOULDS_2015.tab", sep="\t", col.names=T, row.names=F, quote=F)
write.table(paste(shouldDF$date, "\t", shouldDF$flag, "\t", shouldDF$who, " SHOULD ", shouldDF$what,sep=""), file="shoulds_2015.txt")

library(tm)
library(wordcloud)


who_should <- shouldDF$who


clean_n_count <- function(x){
  mywords <- c("-","since","mr",",","among","also","said","will","says","one","new","can","even","two","now");
  x <- tolower(as.character(x))
  x_words <- unlist(strsplit(x, " "));
  x_strip <- removeWords(x_words, c(stopwords("english"),mywords));
  x_strip_clean <- x_strip[x_strip!=""];
  x_strip_clean_df <- as.data.frame(table(x_strip_clean));
  
  return (x_strip_clean_df[order(x_strip_clean_df$Freq, decreasing = T),]);
}

head(clean_n_count(who_should), n=10)


shouldDF[sapply(gregexpr("Merkel", shouldDF$who, perl=TRUE),"[[",1) >= 0,]
