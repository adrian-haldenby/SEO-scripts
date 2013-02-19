library(gam)

##############################
#Fit a curve with a GAM and linear model to the CTR data to be used in a function to estimate volume of traffic based
# on keyword position and rank

#Load known points from a big collection of webmaster tools rankings exports
filenames <- list.files("~/Estimate CTR/", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
ldf.df <- do.call( rbind, ldf )
ldf.df <- ldf.df[!(ldf.df$CTR=="-"),]
#some wierdness to deal with the numeric values being imported as factors...
ldf.ctr <- gsub("\\%","",as.character(ldf.df[,6]))
ldf.ctr <- as.numeric(ldf.ctr)/100
ldf.rank <- as.numeric(as.character(ldf.df[,8]))
plot(ldf.ctr~ldf.rank)

ctr.rank <- as.data.frame(cbind(ldf.ctr,ldf.rank))
colnames(ctr.rank)<- c("CTR","Rank")
# Use only up to rank 11 entries, after that things get spotty
ctr.rank <- ctr.rank[ctr.rank$Rank <= 11,]

#create simple model 
av.model.gam <- gam(CTR~s(Rank),data=ctr.rank,family=poisson)
av.rank <- as.data.frame(seq(1:50))
colnames(av.rank)[1] <- "Rank"

crt.pred <- predict.gam(av.model.gam,newdata=av.rank,type="response")

# Uncomment to use Optify data 
#  Source: http://www.optify.net/wp-content/uploads/2011/04/Changing-Face-oof-SERPS-Organic-CTR.pdf

#av.ctr <- c(.364,.125,.095,.079,.061,.041,.038,.035,.03,.022,.026,.015,.013,.011,.012,.012,.014,.013,.014,.014)
#av.rank <- seq(1:20)
#add in square root 
#av.ctr.sqrt <- sqrt(av.rank)
#av.df <- as.data.frame(cbind(av.ctr,av.rank))
#colnames(av.df)<- c("CTR","Rank")

#create model
#av.model.gam <- gam(CTR~s(Rank),data=av.df,family=poisson)
