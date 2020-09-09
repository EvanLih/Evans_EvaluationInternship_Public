library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)

setwd("C:/Users/evanlih/Desktop/Github/Evans_EvaluationInternship")
setwd("C:/Users/Evanc/Desktop/Github/Evans_EvaluationInternship/")


#tab <- read.csv("Likert_Q4_Employer.csv")


#Likert_Q3_Students
tab <- read.csv("Likert_Q3_Student.csv")
mytitle<-"Rank your motivation from 1 to 5 for pursuing an internship. \n(1 being highest motivation, 5 being lowest motivation)"
mylevels<-c("Least Motivated", "Not motivated", "Indifferent", "Somewhat motivated","Highly motivated")


##Likert_Q6_Students
tab <- read.csv("Likert_Q6_Student.csv")
mytitle<-"Which of the following would you most like an internship experience to include?"
mylevels<-c("Strongly Disagree", "Somewhat Disagree", "Neither Agree or Disagree", "Somewhat Agree", "Strongly Agree")

##Likert_Q6_Students
#tab <- read.csv("Likert_Q7_Student.csv")
#mytitle<-"What skills from your Evans School education are you \nhoping to apply in your ideal internship?"
#mylevels<-c("Not apply at all", "Minimally apply", "Somewhat apply", "Apply", " Strongly apply")



#Likert_Q8_Employer
#tab <- read.csv("Employers_Q8_Likert.csv")
mytitle<-"What expectations do you have for graduate level interns?"
mylevels<-c("Strongly disagree", "Somewhat disagree", "Neither agree or disagree", "Somewhat agree", "Strongly agree")

#Likert_Q8_Employer
tab <- read.csv("Employers_Q9_Likert.csv")
mytitle<-"What skills do Evans School interns bring that you value?"
mylevels<-c("Not important", "Slightly important", "Fairly Important", "Important", "Very Important")

Likert_Q4_Employer
tab <- read.csv("Likert_Q4_Employer.csv")
mytitle<-"What type of work do you typically have interns complete?"
mylevels<-c("Not common at all", "Somewhat uncommon", "Commonly", "Somewhat common", "Extremely common")

#Likert_Q10_Employer
#tab <- read.csv("Likert_Q10_Employer.csv")
#mytitle<-"How do Evans School interns compare to non-Evans Interns?"
#mylevels<-c("Much worse", "Somewhat worse", "About the same", "Somewhat better", "Much better")

#tab <- read.csv("Likert_Q14_Employer.csv")
#mytitle<-"What would be helpful for the Evans School to provide in building a \nlong-term sustainable relationship with your organization?"
#mylevels<-c("Strongly disagree", "Somewhat disagree", "Neither agree or disagree", "Somewhat agree", "Strongly agree")


numlevels<-length(tab[1,])-1
numcenter<-ceiling(numlevels/2)+1
tab$midvalues<-tab[,numcenter]/2
tab2<-cbind(tab[,1],tab[,2:ceiling(numlevels/2)],
            tab$midvalues,tab$midvalues,tab[,numcenter:numlevels+1])
colnames(tab2)<-c("outcome",mylevels[1:floor(numlevels/2)],"midlow",
                  "midhigh",mylevels[numcenter:numlevels])

numlevels<-length(mylevels)+1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100

numlevels<-length(tab[1,])-1
temp.rows<-length(tab2[,1])
pal<-brewer.pal((numlevels-1),"RdBu")
pal[ceiling(numlevels/2)]<-"#DFDFDF"
legend.pal<-pal
pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
       pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])

tab3<-melt(tab2,id="outcome")
tab3$col<-rep(pal,each=temp.rows)
tab3$value<-tab3$value*100
tab3$outcome<-str_wrap(tab3$outcome, width = 40)
#tab3$outcome<-factor(tab3$outcome, levels = tab2$outcome[order(-(tab2[,5]+tab2[,6]+tab2[,7]))])
highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
lows<-na.omit(tab3[1:(length(tab3[,1])/2),])
lows <- lows[rev(rownames(lows)),]
lows$col <- factor(lows$col, levels = c("#CA0020","#F4A582", "#DFDFDF"))

ggplot() + geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + 
  coord_flip() +
  labs(title=mytitle, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

