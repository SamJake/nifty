library(ggplot2)
library(dplyr)
setwd("C:/Users/sam.jacob/Desktop/R")
classes <- c(rep("character",6))
n <- read.csv("nifty.csv",colClasses = classes)
n$Date <- as.Date(n$Date,"%d-%b-%y")
#n[,2] <- lapply(n[,2],function(x) {gsub(",","",x)})
n[,2:5] <- lapply(n[,2:5],function(y) {lapply(y,function(x) {gsub(",","",x)})})
n[,2:5] <- lapply(n[,2:5],function(x) {as.numeric(x)})
n <- select(n,-6)
n <- mutate(n,HL_ratio = High/Low,CO_ratio = Close/Open,OH_ratio=Open/High)

setwd("./Images")
gplot <- function(n,col,title)
  {
    g1 <- ggplot(data=n,aes(x=n$Date,y=col))+geom_line(col="blue",size=1)
    #g1 <- g1 + geom_hline(yintercept =seq(7900,8800,by=100),size=0.1)
    g1 <- g1 + xlab("2016 Q4") + ylab("Nifty") + ggtitle(paste("NIFTY 50 - ",title,sep=""))
    ggsave(file=paste("Nifty_",title,".jpeg",sep = ""),width = 30, height = 20, units="cm")
}
names <- names(n[,2:7])

mapply(function(x,y) {gplot(n,x,y)},n[,2:7],names)
n <- mutate(n, month=months(as.POSIXlt(Date)))

jpeg(filename="boxplot.jpg", width = 640, height = 480, units="px")
boxplot(Close ~ month, data=n, col="red")
dev.off()


jpeg(filename = "multscatter.jpg", width = 800, height = 480, units="px")
par(mfrow=c(1,3),mar=c(4,4,2,1))
with(subset(n,month=="October"),plot(Date,Close,col="blue",main="October"))
with(subset(n,month=="November"),plot(Date,Close,col="red",main="November"))
with(subset(n,month=="December"),plot(Date,Close,col="green",main="December"))
dev.off()