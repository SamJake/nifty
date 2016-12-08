library(ggplot2)
library(dplyr)

classes <- c(rep("character",6))
n <- read.csv("nifty.csv",colClasses = classes)
n$Date <- as.Date(n$Date,"%d-%b-%y")
#n[,2] <- lapply(n[,2],function(x) {gsub(",","",x)})
n[,2:5] <- lapply(n[,2:5],function(y) {lapply(y,function(x) {gsub(",","",x)})})
n[,2:5] <- lapply(n[,2:5],function(x) {as.numeric(x)})

setwd("./Images")
gplot <- function(n,col,title)
  {
    g1 <- ggplot(data=n,aes(x=n$Date,y=col))+geom_line(col="blue",size=1)
    g1 <- g1 + geom_hline(yintercept =seq(7900,8800,by=100),size=0.1)
    g1 <- g1 + xlab("2016 Q4") + ylab("Nifty") + ggtitle(paste("NIFTY 50 - ",title,sep=""))
    ggsave(file=paste("Nifty_",title,".jpeg",sep = ""),width = 30, height = 20, units="cm")
}
names <- names(n[,2:5])

mapply(function(x,y) {gplot(n,x,y)},n[,2:5],names)