#works with snapshots data to get information on page ¨likes¨ and ¨PTAT¨ over time

library(ggplot2)
library(scales)
library(dplyr)

#Reading Data
m <- read.csv("data/output-microsoft.csv", sep=",", header = TRUE)
m$created <- substr(m$created_at, 0, 10)
m$created <- as.Date(m$created)
names(m) <- c("mlikes","mtalking", "mcreated_at", "created")

w <- read.csv("data/snapshots-walmart.csv", sep=",", header = TRUE)
w$created <- substr(w$created_at, 0, 10)
w$created <- as.Date(w$created)
names(w) <- c("wlikes","wtalking", "wcreated_at", "created")

g <- read.csv("data/snapshots-ge-appliances.csv", sep=",", header = TRUE)
g$created <- substr(g$created_at, 0, 10)
g$created <- as.Date(g$created)
names(g) <- c("glikes","gtalking", "gcreated_at", "created")

j <- read.csv("data/snapshots-johnson-s-baby.csv", sep=",", header = TRUE)
j$created <- substr(j$created_at, 0, 10)
j$created <- as.Date(j$created)
names(j) <- c("jlikes","jtalking", "jcreated_at", "created")

x <- read.csv("data/snapshots-xbox.csv", sep=",", header = TRUE)
x$created <- substr(x$created_at, 0, 10)
x$created <- as.Date(x$created)
names(x) <- c("xlikes","xtalking", "xcreated_at", "created")

#Putting it all together
data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(m, w, g,j,x))
data <- data[data$created > as.Date("2013-01-31"),]

#Graphing 'Likes' over time
dataLikes <- data[,c(1,2,5,8,11,14)]
dataLikes <- dataLikes[,c(1,3,6,2,5,4)]
names(dataLikes) <- c("created", "Walmart","Xbox","Microsoft", "Johnson's Baby","GE Appliances")
meltdata <- melt(dataLikes, id="created")
names(meltdata)[2] <- 'Page'
theme_new <- theme_set(theme_bw())
p <- ggplot(data = meltdata, aes(x = created, y = value, color = Page,
                                 format(scientific=FALSE))) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Likes')+
  labs(title = "'Likes' for Facebook Pages")
p

#Annual Percentage Increase in Likes
pchg <- (dataLikes[dataLikes$created == as.Date("2014-02-01"),2:6]/dataLikes[1,2:6]-1)*100
pchg

#Graphing 'Talking About' over time
dataTalk <- data[,c(1,3,6,9,12,15)]
dataTalk <- dataTalk[,c(1,3,6,2,5,4)]
names(dataTalk) <- c("created", "Walmart","Xbox","Microsoft", "Johnson's Baby","GE Appliances")
meltdata <- melt(dataTalk, id="created")
names(meltdata)[2] <- 'Page'
theme_new <- theme_set(theme_bw())
q <- ggplot(data = meltdata, aes(x = created, y = value, color = Page,
                                 format(scientific=FALSE))) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = comma )+
  xlab('') +
  ylab('PTAT')+
  labs(title = "'PTAT' for Facebook Pages")
q

qplot(created,wtalking,data=data)

  #PTAT to Likes ratio
ratio <- (dataTalk[,2:6]/dataLikes[,2:6])*100
ratio <- cbind(data$created,ratio)
meltdata <- melt(ratio, id="data$created")
names(meltdata) <- c("created","Page","value")
r <- ggplot(data = meltdata, aes(x = created, y = value, color = Page,
                                 format(scientific=FALSE))) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = comma )+
  xlab('') +
  ylab('Ratio')+
  labs(title = "'PTAT' to 'Likes' Ratio")
r

summary(ratio)

