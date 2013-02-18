library(gam)
library(tm)
library(reshape)
library(ggplot2)
library(snowball)
library(plry)
library(foreach)

options(scipen=500)

NAME = "Firstsearchblue"
DIRECTORY = '~/Documents/SEO research/'

##############################
#Fit a curve with a Spline with GAM and linear model to the CTR data to be used in a function to estimate volume of traffic based
# on keyword position and rank

#Load known points
#Sources: http://www.smartinsights.com/search-engine-optimisation-seo/seo-analytics/the-number-one-spot-how-to-use-the-new-search-curve-ctr-data/

av.ctr <- c(.364,.125,.095,.079,.061,.041,.038,.035,.03,.022,.026,.015,.013,.011,.012,.012,.014,.013,.014,.014)
av.rank <- seq(1:20)
#add in square root 
av.ctr.sqrt <- sqrt(av.rank)
av.df <- as.data.frame(cbind(av.ctr,av.rank))
colnames(av.df)<- c("CTR","Rank")

#create models 
av.model.gam <- gam(CTR~s(Rank),data=av.df,family=poisson)

#simple diagnostic plot
plot(av.model.gam$fitted~av.ctr)


################################
# Munge data

#Load the SEMRush Datasets
competitorOne.conc <- read.csv(paste(DIRECTORY,"compOne.csv",sep=""))
ourData.conc <- read.csv(paste(DIRECTORY,"ourData.csv",sep=""))
competitorTwo.conc <- read.csv(paste(DIRECTORY,"compTwo.csv",sep=""))
all.data <- rbind(ourData.conc,competitorOne.conc,competitorTwo.conc)

#predict traffic by applying estimated CTR to search volume
traffic.pred <- function (df){
  ranks <- as.data.frame(df$Position)
  #some wiredness to deal with predict
  colnames(ranks)[1]<- "Rank"
  vol <- df$Search.Volume
  crt.pred <- predict.gam(av.model.gam,newdata=ranks,type="response")
  traf.pred <- vol*crt.pred
  return(traf.pred)
}

#predict traffic by applying estimated CTR to search volume and apply competition mutiplier
traffic.pred.wcomp <- function (df){
  ranks <- as.data.frame(df$Position)
  #some wiredness to deal with predict
  colnames(ranks)[1]<- "Rank"
  vol <- df$Search.Volume
  comp <- (1- df$Competition)
  crt.pred <- predict.gam(av.model.gam,newdata=ranks,type="response")
  traf.pred <- vol*crt.pred*comp
  return(traf.pred)
}

competitorOne.conc$Pred.traffic <- traffic.pred.wcomp(competitorOne.conc)
ourData.conc$Pred.traffic <- traffic.pred.wcomp(ourData.conc) 
competitorTwo.conc$Pred.traffic <- traffic.pred.wcomp(competitorTwo.conc)
all.data$Pred.traffic <- traffic.pred.wcomp(all.data)
# Tokenize Keywords and remove stemming

get_tdms <- function (post,  sensitivity = .96){
  #remove .com
  derp <- gsub('.com',"",post)
  # build a corpus
  mydata.corpus <- Corpus(VectorSource(derp))
  # make each letter lowercase,remove lowercase and stemmings
  mydata.corpus <- tm_map(mydata.corpus, tolower) 
  mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
  mydata.corpus <- tm_map(mydata.corpus, stemDocument)
  #uncomment to remove stopwords
  my_stopwords <- c(stopwords('english'), 'hey',"cant")
  mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
  
  # build a term-document matrix
  mydata.dtm <- TermDocumentMatrix(mydata.corpus)
  m2 <- removeSparseTerms(mydata.dtm, sparse=sensitivity)
  m2 <- as.matrix(m2)
  return(t(m2))
}

keyword.value <- function(df) {
  train.tdm <- get_tdms(as.character(df$Keyword), sensitivity = .99)
  train.tdm.df <- as.data.frame(cbind(as.character(df$Keyword),df$Pred.traffic,train.tdm))
  colnames(train.tdm.df)[1:2] <- c("Keyword","Pred.traffic")
  train.tdm.lng <- melt(train.tdm.df, id=c("Keyword","Pred.traffic"))
  train.tdm.lng <- train.tdm.lng[train.tdm.lng$value != 0,]
  return(train.tdm.lng)
}


#apply find the overall traffic driven by tokenized keywords
agg.keyword.value <- function(train.tdm.df) {
  keyword.values <- aggregate(x = as.numeric(train.tdm.lng$Pred.traffic), by = list(train.tdm.lng$variable), FUN = sum)
  return(keyword.values)
}

# Keyword Term doc matrix
competitorOne.kw.val.lng <- keyword.value(competitorOne.conc)
ourData.kw.val.lng <- keyword.value(ourData.conc) 
competitorTwo.kw.val.lng <- keyword.value(competitorTwo.conc)

# Agregate keyword tokens with values
competitorOne.kw.val <- agg.keyword.value(competitorOne.kw.val.lng)
ourData.kw.val <- agg.keyword.value(ourData.kw.val) 
competitorTwo.kw.val <- agg.keyword.value(competitorTwo.kw.val.lng)
comp.kw.vals <- merge(competitorOne.kw.val, competitorTwo.kw.val, by="Group.1",all=TRUE)
comp.kw.vals[is.na(comp.kw.vals)]<-0
#take the maximum traffic val of the competitors
comp.max <-apply(comp.kw.vals[,c(2,3)],1,max)
comp.kw.vals.total <- as.data.frame(cbind(Group.1 =as.character(comp.kw.vals$Group.1),comp.max=as.numeric(comp.max)),stringsAsFactors =FALSE)


####################################################
#compare ours to competition
comparison <- merge(comp.kw.vals.total, ourData.kw.val, by="Group.1",all=TRUE)
comparison$comp.max <- as.numeric(comparison$comp.max)
comparison[is.na(comparison)]<-0

comparison$Opportunity <- comparison$comp.max - comparison$x
colnames(comparison)[1:3]<- c('keywords','max competitor traffic','our traffic')
comparison$keywords <- factor(comparison$keywords)


#######################################################
#Create Dotpot to show findings
comparison.dp <- comparison[,c(1,4)]
comparison.dp <- transform(comparison.dp, 
                           keywords = reorder(keywords, Opportunity))
s<- ggplot(comparison.dp,aes(x=Opportunity,y=keywords)) + geom_point() + theme_bw()+ geom_vline(xintercept = 0,colour="red", linetype = "longdash")
s + scale_x_continuous(name="Relative Preformance Index") + scale_y_discrete(name="Keyword Theme Stems")


#######################################################
### Now that we've identified the top keywords we can find the ones with the top value

#combine all long melted term doc matricies
all.comp.keywords <- rbind(competitorOne.kw.val.lng,competitorTwo.kw.val.lng)
all.comp.keywords$Pred.traffic <- as.numeric(as.character(all.comp.keywords$Pred.traffic))
#find the max values
comp.keywords <- ddply(all.comp.keywords, .(Keyword,variable), summarise, max=max(Pred.traffic))
#subtract our data keyword values 
comp.keywords.mrg <- merge(comp.keywords, ourData.kw.val, by="Keyword",all=TRUE)
comp.keywords.mrg$Pred.traffic <- as.numeric(as.character(comp.keywords.mrg$Pred.traffic))
comp.keywords.mrg$Pred.traffic[is.na(comp.keywords.mrg$Pred.traffic)] <- 0
comp.keywords.mrg$max[is.na(comp.keywords.mrg$max)] <- 0
comp.keywords.mrg$final.kw.value <- comp.keywords.mrg$max - comp.keywords.mrg$Pred.traffic

##### generate overall kewyord opportunities
all.keywords <- comp.keywords.mrg[order(-comp.keywords.mrg$final.kw.value),]
all.keywords <- all.keywords[,c("Keyword","variable.x","final.kw.value")]
# get rid of duplicates and only keep keywords with higher value
all.keywords.simple <-all.keywords[!duplicated(all.keywords$Keyword),-2] 
all.keywords.simple <- all.keywords.simple[all.keywords.simple$final.kw.value >10,]

##### generate per topic lists
tokens.with.potential <- comparison.dp$keywords[comparison.dp$Opportunity >=100]
comp.keywords.mrg$variable.x <- as.character(comp.keywords.mrg$variable.x)
comp.keywords.mrg<- comp.keywords.mrg[!is.na(comp.keywords.mrg$variable.x),]
# Make directory to store our reports in
save.path = paste(DIRECTORY,NAME,sep="")
dir.create(save.path, showWarnings = TRUE, recursive = FALSE)
#
foreach(i=1:length(tokens.with.potential)) %do% {
  topic <- as.character(tokens.with.potential[i])
  file.name <- paste(topic,"topic keywords.csv",sep=" ")
  print(file.name)
  report.by.topic <- comp.keywords.mrg[comp.keywords.mrg$variable.x == topic,]
  report.by.topic <- report.by.topic[,c("Keyword","final.kw.value")]
  report.by.topic <- report.by.topic[order(-report.by.topic$final.kw.value),]
  write.table(report.by.topic, file=file.name, row.names=FALSE, col.names=FALSE, sep=",")
}





