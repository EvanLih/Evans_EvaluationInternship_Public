library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)
library(scales)

setwd("C:/Users/evanlih/Desktop/Github/Evans_EvaluationInternship")
setwd("C:/Users/Evanc/Desktop/Github/Evans_EvaluationInternship/")


##Alumni_Q1
#tab<-read.csv("Likert_Scale_Test.csv")
#mytitle<-"How valuable was your Evans Internship towards:"
#mylevels<-c( "Not Valuable at All","Not the most valuable","Somewhat Valuable", "Extremely Valuable")

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)

data<-read.csv("Alumni_Q2.csv",header=TRUE)
tab<-data

mytitle<-"On a scale of 1 to 4, how valuable was your Evans school internship towards:"
mylevels<-c("Not Valuable", "Not the most valuable", "Somewhat Valuable",  "Extremely Valuable")

numlevels<-length(tab[1,])-1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100
point1;point2;point3;point4;mymin;mymax
tab2<-tab

numlevels<-length(tab[1,])-1
temp.rows<-length(tab2[,1])
pal<-brewer.pal((numlevels),"RdBu")
legend.pal<-pal

tab3<-melt(tab2,id="Question")
tab3$col<-rep(pal,each=temp.rows)
tab3$value<-tab3$value*100
tab3$Question<-factor(tab3$Question, levels = tab2$Question[order(-(tab2[,4]+tab2[,5]))])
tab3$Question<-str_wrap(tab3$Question, width = 40)
highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
lows<-na.omit(tab3[1:(length(tab3[,1])/2),])
lows <- lows[rev(rownames(lows)),]

ggplot() + geom_bar(data=highs, aes(x = Question, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = Question, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + 
  coord_flip() +
  labs(title=mytitle, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(-100,100))

