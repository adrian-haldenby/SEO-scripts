library(gam)
library(tm)
library(reshape)
library(ggplot2)

options(scipen=500)

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
competitorOne.conc <- read.csv("~/ourData/competitorOne conc.csv")
ourData.conc <- read.csv("~/ourData/ourData conc.csv")
competitorTwo.conc <- read.csv("~/ourData/competitorTwo.com conc.csv")


# Estimate keyword traffic
derp <- as.data.frame(c(50))
colnames(derp)[1] <- "Rank"

traffic.pred <- function (df){
  ranks <- as.data.frame(df$Position)
  #some wiredness to deal with predict
  colnames(ranks)[1]<- "Rank"
  vol <- df$Search.Volume
  crt.pred <- predict.gam(av.model.gam,newdata=ranks,type="response")
  traf.pred <- vol*crt.pred
  return(traf.pred)
}

competitorOne.conc$Pred.traffic <- traffic.pred(competitorOne.conc)
ourData.conc$Pred.traffic <- traffic.pred(ourData.conc) 
competitorTwo.conc$Pred.traffic <- traffic.pred(competitorTwo.conc)

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

#apply find the overall traffic driven by tokenized keywords
find.keyword.value <- function(df) {
  train.tdm <- get_tdms(as.character(df$Keyword), sensitivity = .99)
  train.tdm.df <- as.data.frame(cbind(as.character(df$Keyword),df$Pred.traffic,train.tdm))
  colnames(train.tdm.df)[1:2] <- c("Keyword","Pred.traffic")

  train.tdm.lng <- melt(train.tdm.df, id=c("Keyword","Pred.traffic"))
  train.tdm.lng <- train.tdm.lng[train.tdm.lng$value != 0,]
  keyword.values <- aggregate(x = as.numeric(train.tdm.lng$Pred.traffic), by = list(train.tdm.lng$variable), FUN = sum)
}

competitorOne.kw.val <- find.keyword.value(competitorOne.conc)
ourData.kw.val <- find.keyword.value(ourData.conc) 
competitorTwo.kw.val <- find.keyword.value(competitorTwo.conc)

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
