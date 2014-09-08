## Simple graphs for NCES college major data
## Needs Table_322_10_Extract.csv from http://github
## irm 9/7/14

remove(list=ls())
setwd("~/Dropbox/Majors")

Table_322_10_Extract <- read.csv("Table_322_10_Extract.csv",stringsAsFactors=FALSE)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

Table_322_10_Extract[,"Field.of.study"] <- trim(gsub("[.]","",Table_322_10_Extract[,"Field.of.study"]))
US_Pop <- read.csv("USpop.csv")

yrsRaw <- length(colnames(Table_322_10_Extract))
tmf <- Table_322_10_Extract[,3:yrsRaw]

colnames(tmf) <- as.character(as.numeric(substr(colnames(tmf),2,5))+1)

# linear interpolation of years with five year gaps
tvseq <- function(...)t(Vectorize(seq.default)(...))
years <- as.numeric(colnames(tmf))
d <- diff(years)
L <- lapply(seq(d), function(i) tvseq(from=tmf[,i], to=tmf[,i+1], length.out=d[i]+1)[,-1])

result <- cbind(tmf[,1], do.call(cbind, L))
colnames(result) <- min(years):max(years)

rezsum <- apply(result,2,sum)
rezperc <- sweep(result, 2, rezsum, '/')*100
rezpop <- sweep(result*100,2, US_Pop[,2], '/')

result1 <- cbind(Table_322_10_Extract[,1:2],result)
result2 <- cbind(Table_322_10_Extract[,1:2],rezperc)
result3 <- cbind(Table_322_10_Extract[,1:2],rezpop)

###########################################
# Create Line Charts

require(ggplot2)
require(reshape)

desorder <- c("Occupational","Science & Eng","Social Science","Liberal Arts","Other")

rezagg <- aggregate(result1[3:44],by=list(result1$Category),sum)
melt1 <- melt(rezagg,id=c("Group.1"))
colnames(melt1) <- c("Category","Year","Graduates")
melt1$Category <- as.factor(melt1$Category)
melt1$Category <- factor(melt1$Category, levels=desorder)

rezagg_p <- aggregate(result2[3:44],by=list(result2$Category),sum)
melt2 <- melt(rezagg_p,id=c("Group.1"))
colnames(melt2) <- c("Category","Year","Graduates")
melt2$Category <- as.factor(melt2$Category)
melt2$Category <- factor(melt2$Category, levels=desorder)

rezagg_v <- aggregate(result3[3:44],by=list(result3$Category),sum)
melt3 <- melt(rezagg_v,id=c("Group.1"))
colnames(melt3) <- c("Category","Year","Graduates")
melt3$Category <- as.factor(melt3$Category)
melt3$Category <- factor(melt3$Category, levels=desorder)

graphnames <- list(c("melt1","melt2","melt3"))
ylabs <- c("Graduates","Percent of Total Grads","Grad Pct of US Pop")
models <- list(melt1, melt2, melt3)
fnames <- c("a.pdf","b.pdf","c.pdf")
tnames <- c("US Undergraduate Degrees by Category of Major",
            "US Undergraduate Degrees by Percentage of Category",
            "US Undergraduate Degrees as % of US Population")
for (i in 1:3) {
    modx <- as.data.frame(models[i])
  
    p <- ggplot(modx, aes(x=Year,y=Graduates,colour=Category,group=Category)) + geom_line(size=2) + 
      ggtitle(tnames)
    
    p <- p + scale_x_discrete(breaks=c(seq(1971,2012,by=4))) + labs(y = ylabs[i])
    
    p <- p + theme(axis.text.x = element_text(size=rel(2)), axis.title.x = element_text(size = rel(2)), axis.title.y = element_text(size = rel(2))) 
    p <- p + theme(legend.text = element_text(size=14)) + theme(plot.title=element_text(size=rel(2.5)))
    if (i == 2) p <- p + scale_y_continuous(breaks=seq(0,60,10))
    p
    ggsave(fnames[i])
 
}
