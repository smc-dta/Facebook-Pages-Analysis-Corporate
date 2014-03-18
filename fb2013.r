######################################################################################
#Exploring Facebook data Jan 2013 thru Jan 2014
######################################################################################
#set wd
library(data.table)
library(ggplot2)
library(plyr)
library(scales)

##WORKING WITH PAGE DATA
#Read and set up page data
pagedata <- read.csv("fbPages2.csv", sep="\t",header=TRUE,
                     row.names = NULL, dec = ",", fill = TRUE, comment.char = "")
names(pagedata) <- c("id", "pagename", "likes", "checkins","talking", "werehere",
                     "upated")
pagedata <- pagedata[ ,1:7]

#File plist contains set of pages to include in analysis.  File plist also has company name
#associated with each page and new var 'pagename2' reducing long names so they
#fit on the graph better
plist <-  read.csv("plist.csv", sep="\t",header = TRUE, row.names = NULL, 
                   comment.char = "", fill = TRUE)
plist.t <- data.table(plist)
setkey(plist.t,pagename)

#Merging pagedata and plist, keeping only items on plist
pagedata.t <- data.table(pagedata)
setkey(pagedata.t,pagename)
pagedata <- merge(plist.t,pagedata.t,all.x=TRUE, all.y=FALSE)

#Set up this variable so bars grouped by company in the graphs
pagedata$pagename3 <- paste(pagedata$company,pagedata$pagename2)

#Bar Chart "Likes per Page"
a <- ggplot(data=pagedata, aes(x=pagename3, y=likes, fill=company,
                              format(scientific=FALSE)))
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(axis.text.x = element_text(angle=-70, size = 11, 
                          hjust=0, vjust=1),legend.position = "none")
a + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "'Likes' per Page") +
  ylab(" ") + scale_y_continuous(labels = comma) + xlab(" ")

#Bar Chart "PTAT per Page"
b <- ggplot(data=pagedata, aes(x=pagename3, y=talking, fill=company,
                               format(scientific=FALSE)))
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(axis.text.x = element_text(angle=-70, size = 11, 
                          hjust=0, vjust=1),legend.position = "none")
b + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "PTAT per Page") + 
  ylab(" ") + scale_y_continuous(labels = comma) + xlab(" ")

#PTATratio = PTAT/likes
pagedata$PTATratio <- pagedata[ ,talking/likes]*100

#Data on likes and PTAT over time is found in "snapshots"

##WORKING WITH POST DATA
#Read and set up post data
postdata <-  read.csv("fb2013v3.csv", sep="\t",header=TRUE,
        row.names = NULL, dec = ",", fill = TRUE, comment.char = "")
summary(postdata)
names(postdata) <- c("id","type","created","likes","comments","pagename","shares","updated")
postdata$id <- as.factor(postdata$id)#change class so can merge with other data
postdata$updated <- as.Date(postdata$updated)

#Getting data on posts of 2 more pages from Facebook
#Code can be found on fb2013fillin.r, uses RFacebook
ServerCloud <- read.csv("ServerCloud.csv", sep=",",header=TRUE,
                         row.names = NULL, dec = ",", fill = TRUE, comment.char = "")
#Arranging data from Rfacebook
ServerCloud$updated <- as.Date("2014-02-28")
postdata2 <- data.frame(ServerCloud$from_id, ServerCloud$type, ServerCloud$created_time, 
               ServerCloud$likes_count, ServerCloud$comments_count,
               ServerCloud$from_name, ServerCloud$shares_count, ServerCloud$updated)
names(postdata2) <- c("id","type","created","likes","comments","pagename","shares","updated")
WindowsServer <- read.csv("WindowsServer.csv", sep=",",header=TRUE,
                        row.names = NULL, dec = ",", fill = TRUE, comment.char = "")
WindowsServer$updated <- as.Date("2014-02-28")
postdata3 <- data.frame(WindowsServer$from_id, WindowsServer$type, 
              WindowsServer$created_time, WindowsServer$likes_count, 
              WindowsServer$comments_count, WindowsServer$from_name,
              WindowsServer$shares_count,WindowsServer$updated)
names(postdata3) <- c("id","type","created","likes","comments","pagename","shares","updated")
postdata$id <- as.numeric(as.character(postdata$id)) #adjust class to combine files
postdata <- rbind(postdata, postdata2, postdata3)

#Select dates Jan 31,2013-Jan 31, 2014
postdata <- postdata[as.Date(postdata$created) > as.Date("2013-01-31") &
            as.Date(postdata$created) < as.Date("2014-01-31"),] 

#Calculating Posts per Page
postdata$n <- 1
postdata.t <- data.table(postdata)
pposts <- postdata.t[ ,sum(n), by = pagename]
setnames(pposts,1:2,c("pagename", "posts"))
pposts <- arrange(pposts, desc(posts))
#pposts <- read.csv("pposts.csv", sep="\t",header=TRUE,
 #                  row.names = NULL, dec = ",", fill = TRUE, comment.char = "")

#merge pposts, plist including only pages in plist
pposts.t <- data.table(pposts)
setkey(pposts.t,pagename)
pposts <- merge(plist.t,pposts.t,all.x=TRUE, all.y=FALSE)
#Correcting for "Windows Style"
pposts$posts <- as.numeric(pposts$posts)
pposts[51,4] <- 189

#Bar Graph Posts per Page by Company
pposts$pagename3 <- paste(pposts$company,pposts$pagename2)#so bars are stacked by company
e <- ggplot(data=pposts,aes(x=pagename3, y=posts,fill=company,
                              format(scientific=FALSE))) 
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
  axis.text.x = element_text(angle=-70, size = 11, hjust=0, vjust=1),
  legend.position = "none")
e + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "Annual Posts per Page") + ylab(" ") + 
  scale_y_continuous(labels = comma)+ 
  xlab(" ")

#Calculating post stats on number of likes, comments, & shares
pLCS <- postdata
pLCS[is.na(pLCS)] <- 0
metrics <- function(pLCS){
          c(median(pLCS[pLCS$likes > 0, 4]),
          mean(pLCS[pLCS$likes > 0, 4]),
          max(pLCS$likes),
          median(pLCS[pLCS$comments > 0, 5]),
          mean(pLCS[pLCS$comments > 0, 5]),
          max(pLCS$comments),
          median(pLCS[pLCS$shares > 0, 7]),
          mean(pLCS[pLCS$shares > 0, 7]),
          max(pLCS$shares))
          }
results <- ddply(pLCS,.(pagename),.fun = metrics)
results[ ,2:10] <- round(results[ ,2:10], digits = 0)
names(results) <- c("pagename","medLikes","meanLikes","maxLikes",
                    "medComments","meanComments","maxComments",
                    "medShares","meanShares","maxShares")
#correcting "Windows Style" pagename
results$pagename <- as.character(results$pagename)
results[83,1] <- as.character("Windows Style")
results <- results[as.character(results$pagename) %in% plist$pagename, ]
#adding company info to results
plist$pagename2 <- as.character(plist$pagename2)
plist$company <- as.character(plist$company)
results <- data.table(results, key = "pagename")
results <- merge(results, plist.t)
results$pagename3 <- paste(results$company,results$pagename2)#so bars are stacked by company

#Bar Chart of Ave Likes by Page/Company
f <- ggplot(data=results,aes(x=pagename3, y=meanLikes,fill=company))
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
  axis.text.x = element_text(angle=-75, size = 12, hjust=0, vjust=1),
  legend.position = "none")
f + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "Average 'Likes' per Post") + 
  ylab(" ") + scale_y_continuous(labels = comma) + xlab(" ")

#Bar Chart of Ave Likes per Post EXCLUDING WMT and MSFT
resultsMod <- results[! results$pagename == "Walmart" 
                      & ! results$pagename == "Microsoft",]
fMod <- ggplot(data=resultsMod,aes(x=pagename3, y=meanLikes,fill=company))
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
  axis.text.x = element_text(angle=-75, size = 12, hjust=0, vjust=1),
  legend.position = "none")
fMod + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "Average 'Likes' per Post (excl Walmart and Microsoft)") + 
  ylab(" ") + scale_y_continuous(labels = comma) + xlab(" ")

#Bar Chart of Median Likes by Page/Company
g <- ggplot(data=results,aes(x=pagename3, y=medLikes,fill=company))
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
  axis.text.x = element_text(angle=-75, size = 11, hjust=0, vjust=1),
  legend.position = "none")
g + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "Median 'Likes' per Post") +  
  ylab(" ") + scale_y_continuous(labels = comma) + xlab(" ")

#Bar Chart of Shares by Page/Company
h <- ggplot(data=results,aes(x=pagename3, y=meanShares,fill=company))
h + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "Average Shares per Post") + ylab(" ") + xlab(" ")

#Bar Plot of Comments by Page/Company
j <- ggplot(data=results,aes(x=pagename3, y=meanComments,fill=company))
j + layer(
  geom = "bar",
  stat = "identity", 
  postition="dodge",
  stat_params = list(binwidth = 3),
  width=1.0) + 
  labs(title = "Average Comments per Post") + ylab(" ") + xlab(" ")

#Top pages by likes
Llikes <- format(pLCS[pLCS$likes > 100000,],scientific = FALSE)
post_ID <- format(pLCS[2,1],scientific = FALSE)
post <- getPost(post_ID, token, n = 10, likes = TRUE, comments = FALSE)
