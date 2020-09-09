library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(ggthemes)
library(stringr)
library(scales)
library(magrittr)
setwd("C:/Users/Evanc/Desktop/Github/Evans_EvaluationInternship/")
setwd("C:/Users/evanlih/Desktop/Github/Evans_EvaluationInternship")

currentStudents<-read.csv("Career+Services,+Current+Student+Survey_August+20,+2019_12.28.csv", stringsAsFactors = FALSE)



currentStudents = currentStudents[-1,]
currentStudents$Q1[currentStudents$Q1=="No (please explain):"] <- "No"
legendPal<-c("#85754d","#4b2e83","#808080")

df$cols[df$cols=="green"] <- "purple"


StudenttitleQ1<-"Did you originally intend to pursue an internship \nwhen starting at the Evans School?"
ggplot(currentStudents, aes(Q1, fill = Q1)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values =  c("#CA0020","#0571B1")) +
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ1, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "None") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .15))

StudenttitleQ2<-"How many internships have you completed thus far at the Evans School? \n(including any experiences that are currently ongoing)"
ggplot(currentStudents, aes(Q2, fill = Q2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_brewer(palette="RdBu") +
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ2, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "None") +
  guides(fill=guide_legend(title="Number of internships completed")) + 
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .15))


StudenttitleQ3 <- "Rank your preferences from 1 to 3 for the following type of work \nyou would like to complete in an internship. \n (1 being most preferred, and 3 being least preferred)"

currentStudentsQ3 <- read.csv("Rank_Student_Q4.csv",stringsAsFactors = FALSE)
currentStudentsQ3<-melt(currentStudentsQ3,id="Question")
currentStudentsQ3$variable <- as.character(currentStudentsQ3$variable)
currentStudentsQ3$Question <- as.character(currentStudentsQ3$Question)
currentStudentsQ3$variable[currentStudentsQ3$variable=="Working.on.a.special.project.for.the.duration.of.the.internship"] <- "Working on a special project for the duration of the internship"
currentStudentsQ3$variable[currentStudentsQ3$variable=="Serving.as.a.member.of.the.organizational.team.in.a.daily.operational.capacity"] <- "Serving as a member of the organizational team in a daily operational capacity"
currentStudentsQ3$variable[currentStudentsQ3$variable=="Serving.as.a.member.of.an.organizational.team.through.smaller.ongoing.projects"] <- "Serving as a member of an organizational team through smaller ongoing projects"
currentStudentsQ3$variable <- as.factor(currentStudentsQ3$variable)
currentStudentsQ3$Question <- factor(currentStudentsQ3$Question, levels = c( "Least Preferred", "Impartial", "Most Preferred"))

charts.data <- ddply(currentStudentsQ3, .(variable),
                     transform, pos = cumsum(value) - (0.5 * value))

ggplot(currentStudentsQ3, aes(variable, value, fill = Question)) +
  geom_bar(stat="identity") +
  theme_fivethirtyeight() + 
  coord_flip() +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  labs(title=StudenttitleQ3, y="",x="") +
  geom_text(data=charts.data, aes(x = variable, y = pos,
                                  label = paste0(value*100,"%")), size=4) +
  scale_x_discrete(labels = wrap_format(30)) +
  guides(fill=guide_legend(title="Rank")) +
  scale_fill_manual(values = c("#C90020", "#F4A682","#0571B1"))

  scale_fill_gradient2(low="#0571B1", mid = "#F4A682", high="#C90020", midpoint = 2)
  

#####aLUMNI q2: #####
  alumni<-read.csv("Career+Services,+Alumni+Survey_August+20,+2019_13.04.csv", stringsAsFactors = FALSE)
  alumni = alumni[-1,]
  
  alumni <- alumni[!is.na(alumni$Q2),]
  
  
  alumni$Q2 <- factor(alumni$Q2, levels = rev(c("0", "1", "2", "3+", "Not applicable:")))
  


  
  ggplot(alumni, aes(Q2, fill = Q2, na.rm= TRUE)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_fill_manual(values = c("#C90020", "#F5D8CA", "#F4A582", "#92C6DE", "#0571B1")) +
    coord_flip() +
    theme_fivethirtyeight() + 
    labs(title="How many internships did you complete while a student at the Evans School?", y="",x="") +
    theme(plot.title = element_text(size=14, hjust=0.5)) +
    theme(axis.text.y = element_text(hjust=0)) +
    theme(legend.position = "none") +
    guides(fill=guide_legend(title="Percent")) + 
    scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.60))
  

####Q5
StudenttitleQ5<-"How much do you expect an internship will \nhelp you achieve your career goals post-Evans?"

currentStudents$Q5 <- factor(currentStudents$Q5, levels = c("Not at all", "A little", "A moderate amount", "A lot", "A great deal"))


ggplot(currentStudents, aes(Q5, fill = Q5)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_brewer(palette="RdBu") +
  scale_fill_manual(values = c("#C90020", "#F5D8CA", "#F4A582", "#92C6DE", "#0571B1")) +
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ5, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.35))
    

###Q9



StudenttitleQ9 <- "What would be your ideal timeline for completing an internship \nduring your time in graduate school? \n(1 being most ideal timeline, and 3 being least ideal timeline)"

currentStudentsQ9 <- read.csv("Rank_Student_Q9.csv",stringsAsFactors = FALSE)

currentStudentsQ9<-melt(currentStudentsQ9,id="Question")
currentStudentsQ9$variable <- as.character(currentStudentsQ9$variable)
currentStudentsQ9$Question <- as.character(currentStudentsQ9$Question)
currentStudentsQ9$variable[currentStudentsQ9$variable=="During.my.first.academic.year"] <- "During my first academic year"
currentStudentsQ9$variable[currentStudentsQ9$variable=="During.the.summer.between.my.first.and.second.academic.year"] <- "During the summer between my first and second academic year"
currentStudentsQ9$variable[currentStudentsQ9$variable=="During.my.second.academic.year"] <- "During my second academic year"
currentStudentsQ9$variable <- as.factor(currentStudentsQ9$variable)
currentStudentsQ9$Question <- factor(currentStudentsQ9$Question, levels = c("Least Ideal", "Impartial", "Most Ideal"))
currentStudentsQ9$Question <- factor(currentStudentsQ9$Question, levels = c("Least Ideal", "Impartial", "Most Ideal"))



charts.dataQ9 <- ddply(currentStudentsQ9, .(variable),
                     transform, pos = cumsum(value) - (0.5 * value))




charts.dataQ9$pos <- as.numeric(charts.dataQ9$pos)


ggplot(currentStudentsQ9, aes(variable, value, fill = Question)) +
  geom_bar(stat="identity") +
  theme_fivethirtyeight() + 
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  labs(title=StudenttitleQ9, y="",x="") +
  geom_text(data=charts.dataQ9, aes(x = variable, y = pos,
                                  label = paste0(value*100,"%")), size=3.8) +
  scale_x_discrete(labels = wrap_format(30)) +
  guides(fill=guide_legend(title="Rank")) +
  scale_fill_manual(values = c("#C90020", "#F4A682","#0571B1"))


###Q10
StudenttitleQ10<-"Rank your preferences from 1 to 4 for accepting your \nideal internship of the following time commitments. \n(1 being most preferred, and 4 being least preferred)"

currentStudentsQ10 <- read.csv("Rank_Student_Q10.csv",stringsAsFactors = FALSE)
currentStudentsQ10$value %<>% round(digits = 2)
currentStudentsQ10$Question %<>% as.numeric
#currentStudentsQ10$variable %<>% as.factor

currentStudentsQ10 %<>% arrange(desc(Question))



charts.dataQ10 <- ddply(currentStudentsQ10, .(variable),
                       transform, pos = cumsum(value) - (0.5 * value))
charts.dataQ10$pos <- as.numeric(charts.dataQ10$pos)

mylevels1<-c("Most preferred", "Slightly preferred", "Impartial", "Least preferred")



ggplot(currentStudentsQ10, aes(variable, y =value, fill = Question)) +
  geom_bar(stat="identity",position = position_fill(reverse = TRUE), width = .5) +
  theme_fivethirtyeight() + 
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  labs(title=StudenttitleQ10, y="",x="") +
  geom_text(data=charts.dataQ10, aes(x = variable, y = pos,
                                    label = paste0(value*100,"%")), size=2) +
  scale_x_discrete(labels = wrap_format(30)) +
  guides(fill=guide_legend(title="Rank")) +
  scale_fill_gradientn(labels = c("Most preferred","Slightly preferred", "Impartial","Least preferred"),colors = c("#0571B1","#92c6de","#f4a682","#C90020")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


####Q11

StudenttitleQ11<-"What length would you want your ideal internship to be?"

currentStudentQ11 <- read.csv("Students_Q11.csv")

currentStudentQ11$Answer <- factor(currentStudentQ11$Answer, levels = c("Other","1-3 months", "3-6 months", "6-12 months"))


ggplot(currentStudentQ11, aes(Answer, Percent, fill = Answer)) +
  geom_bar(stat = "Identity") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title=StudenttitleQ11, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  scale_fill_manual(values =  c("#f4a682","#C90020","#0571B1","#92c6de")) +

  theme(legend.position = "none") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.45))

####Q12
StudenttitleQ12<-"How long did/do you anticipate an internship search taking?"

currentStudents$Q12 <- factor(currentStudents$Q12, levels = c("Less than 1 month", "1-3 months", "More than 3 months"))

currentStudents <- currentStudents[!is.na(currentStudents$Q12), ]


ggplot(currentStudents, aes(Q12, fill = Q12)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#92c6de","#0571b1","#f4a682")) +
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ12, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "None") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.70))


###Q13
StudenttitleQ13<-"How far in advance of an internship start date would \nyou prefer an offer be extended and/or finalized?"

#currentStudents$Q13 <- factor(currentStudents$Q13, levels = c("Less than 1 month", "1-3 months", "More than 3 months"))

currentStudents <- currentStudents[!is.na(currentStudents$Q13), ]


ggplot(currentStudents, aes(Q13, fill = Q13)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_polar("y") +
  scale_fill_manual(values = c("#0571b1", "#92c6de","#f4a682")) +
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ13, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "None") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_x_discrete(labels = wrap_format(20)) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.65))

ggplot(currentStudents, aes(x= "",y = Q13, fill = Q13)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_polar("y") 
  


####Q14 

StudenttitleQ14<-" In considering a perfect-fit opportunity with your dream  \norganization what compensation would you be willing to accept?"

currentStudentQ14 <- read.csv("Students_Q14.csv")
currentStudentQ14$Answer <- factor(currentStudentQ14$Answer, levels = c("Unpaid", "$15 to $19 per hour", "$20 or more per hour", "One-time Stipend $2500 or less", "One-time Stipend over $2500"))

ggplot(currentStudentQ14, aes(Answer, Percent, fill = Answer)) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = c("#C90020", "#F5D8CA", "#F4A582", "#92C6DE", "#0571B1")) +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title=StudenttitleQ14, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.45))


####Q16

currentStudents <- currentStudents[!is.na(currentStudents$Q16), ]



ggplot(currentStudents, aes(Q16 ,fill = Q16)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = .5) +
  scale_fill_manual(values = c("#0571b1", "#92C5DE","#F4A582")) +
  theme_fivethirtyeight() + 
  labs(title="What is your gender?", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position="none") +
  scale_x_discrete(labels = wrap_format(30)) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.65)) +
  guides(fill=guide_legend(nrow=3, ncol = 2, byrow=TRUE))

####Q17
StudenttitleQ17<-"What was the primary reason you chose to enroll at the Evans School?"

#currentStudents$Q13 <- factor(currentStudents$Q13, levels = c("Less than 1 month", "1-3 months", "More than 3 months"))

currentStudents <- currentStudents[!is.na(currentStudents$Q17), ]
currentStudents$Q17[currentStudents$Q17=="Other (please specify):"] <- "Other"


ggplot(currentStudents, aes(Q17, fill = Q17)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("#C90020", "#F5D8CA", "#F4A582", "#92C6DE", "#0571B1")) +
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ17, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position="none") +
  labs(fill='Percent') +
  scale_x_discrete(labels = wrap_format(30)) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.37)) +
  guides(fill=guide_legend(nrow=3, ncol = 2, byrow=TRUE))

###Q18
StudenttitleQ18<-"How many years of work experience did you have \nprior to coming to the Evans School?"

#currentStudents$Q13 <- factor(currentStudents$Q13, levels = c("Less than 1 month", "1-3 months", "More than 3 months"))

currentStudents <- currentStudents[!is.na(currentStudents$Q17), ]
currentStudents$Q17[currentStudents$Q17=="Other (please specify):"] <- "Other"


ggplot(currentStudents, aes(Q18 ,fill = Q18)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = .5) +
  scale_fill_manual(values = rev(c("#0571b1", "#92C5DE","#F4A582"))) +
  theme_fivethirtyeight() + 
  labs(title=StudenttitleQ18, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position="none") +
  scale_x_discrete(labels = wrap_format(30)) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.50)) +
  guides(fill=guide_legend(nrow=3, ncol = 2, byrow=TRUE))

####Q19

StudenttitleQ19<-"A variety of factors contribute to a successful internship search. \nIf you currently are in an internship, which resources contributed \nto your success in getting the internship?"

currentStudentQ19 <- read.csv("Students_Q19.csv")

#currentStudentQ11$Answer <- factor(currentStudentQ11$Answer, levels = c("Other","1-3 months", "3-6 months", "6-12 months"))



ggplot(currentStudentQ19, aes(x = reorder(Answer, Percent), Percent, fill = Answer)) +
  geom_bar(stat = "Identity") +
  scale_fill_brewer(palette="RdBu") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title=StudenttitleQ19, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Percent")) + 
  scale_x_discrete(labels = wrap_format(50)) +
  theme(legend.position="none") +
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.35)) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))

####Q20

StudenttitleQ20<-"What are the following Evans School Career Development \nOffice resources you have used?"

currentStudentQ20 <- read.csv("Students_Q20.csv")

currentStudentQ20$Answer <- factor(currentStudentQ20$Answer, levels = c("Individual resume/cover letter review","Individual career counseling/advising", "Individual mock/practice interview", "Individual salary negotiation advice", "EvansJobs listings of jobs/internships", "Other"))


ggplot(currentStudentQ20, aes(x = reorder(Answer, Percent),y= Percent, fill = Answer)) +
  geom_bar(stat = "Identity") +
  scale_fill_brewer(palette="RdBu", direction = -1) +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title=StudenttitleQ20, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  guides(fill=guide_legend(title="Percent")) + 
  scale_x_discrete(labels = wrap_format(50)) +
  theme(legend.position="none") +
  scale_y_continuous(labels = percent, breaks = seq(0,1, by = .10), limits = c(0,.35)) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))


charts.dataQ9$pos[charts.dataQ9$pos == "0.875"] <- ".935"
charts.dataQ9$pos[charts.dataQ9$pos == "0.975"] <- ".83"
charts.dataQ9$pos[charts.dataQ9$pos == "0.34"] <- ".75"
charts.dataQ9$pos[charts.dataQ9$pos == "0.81"] <- ".25" 
charts.dataQ9$pos[charts.dataQ9$pos == "0.715"] <- ".40" 
charts.dataQ9$pos[charts.dataQ9$pos == "0.285"] <- ".85" 