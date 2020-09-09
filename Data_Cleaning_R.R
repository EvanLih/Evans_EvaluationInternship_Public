
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)

setwd("C:/Users/evanlih/Desktop/Github/Evans_EvaluationInternship")
read_xls("Initial_Dataset_through_6.19.2019.xls")
df <- read.csv("Initial_Dataset_through_6.19.2019.csv")
test <- filter(df, grepl("2018", Flags))
W

outcome <- (length(which(df$Paid.Unpaid == "No"))/length(df$Paid.Unpaid))*100
print(paste("Unpaid internships curently make up ",outcome, "% of 2020 graduating cohort' consists of unpaid internships's summer internships", sep = ""))

test <- read_excel("\\\\netid.washington.edu\\wfs\\Evans\\StudentServices\\Applicants\\Admissions\\For Evan\\2015-2019 Applicant Data COPY.xlsx", sheet = "Original Pivot Table", na = "n/a", col_types = NULL)

interests <- test %>% select("Incoming Year","interest area 1", "interest area 2", "status")

keep = c("Offer Accepted - Enrolled Packet Made", "Offer Accepted - Registered")

interests2018 <- subset(interests, status %in% keep & `Incoming Year` == 2018) %>% rename(Year = 'Incoming Year', interest1 = 'interest area 1', interest2 = 'interest area 2')

interests2018 <- interests2018 %>% filter(!is.na(interest1) & !is.na(interest2))

interests2018 <- as.data.frame(prop.table(table(interests2018$interest1)))

interests2018$Freq <- interests2018$Freq*100

interests2018 %>% arrange(desc(Freq))

interests2018$Year <- rep(2018, length.out = nrow(interests2018))

## Interests 2017
interests2017 <- subset(interests, status %in% keep & `Incoming Year` == 2017) %>% rename(Year = 'Incoming Year', interest1 = 'interest area 1', interest2 = 'interest area 2')

interests2017 <- interests2017 %>% filter(!is.na(interest1) & !is.na(interest2))

interests2017 <- as.data.frame(prop.table(table(interests2017$interest1)))

interests2017$Freq <- interests2017$Freq*100

interests2017 %>% arrange(desc(Freq))

interests2017$Year <- rep(2017, length.out = nrow(interests2017))


## Interests 2016 
interests2016 <- subset(interests, status %in% keep & `Incoming Year` == 2016) %>% rename(Year = 'Incoming Year', interest1 = 'interest area 1', interest2 = 'interest area 2')

interests2016 <- interests2016 %>% filter(!is.na(interest1) & !is.na(interest2))

interests2016 <- as.data.frame(prop.table(table(interests2016$interest1)))

interests2016$Freq <- interests2016$Freq*100

interests2016$Year <- rep(2016, length.out = nrow(interests2016))

##interests2015
interests2015 <- subset(interests, status %in% keep & `Incoming Year` == 2015) %>% rename(Year = 'Incoming Year', interest1 = 'interest area 1', interest2 = 'interest area 2')

interests2015 <- interests2015 %>% filter(!is.na(interest1) & !is.na(interest2))
0
interests2015 <- as.data.frame(prop.table(table(interests2015$interest1)))

interests2015$Freq <- interests2015$Freq*100

interests2015$Year <- rep(2015, length.out = nrow(interests2015))

####
yearTest <- rbind(interests2015,interests2016, interests2017, interests2018)

yearTest <- subset(yearTest, Var1 != "Science & Technology Policy")

ggplot(yearTest, aes(Var1, Freq)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label=round(Freq, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25, size=3) +
  facet_grid(. ~ Year) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

ggplot(yearTest, aes(x = Year, y = Freq)) +
  geom_smooth(colour = "lightblue") +
  geom_point() +
  theme_stata() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  facet_wrap(. ~ Var1, labeller = label_wrap_gen(width=10), ncol =4) +
  scale_x_continuous(breaks = c(2015,2016,2017,2018)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  ggtitle("Percent of Area of interest by Year") +
  ylab("Percent") +
  xlab("Year")


 


  

'################# TEST CODE ##############################
column_data_types <- vector()

column_names <- colnames(test)
for(i in 1: length(column_names)){

  #for date column names
if(column_names[i] == "Degree Date"){
  column_data_types[i] <- "date"}}

test2 <- read_excel("\\\\netid.washington.edu\\wfs\\Evans\\StudentServices\\Applicants\\Admissions\\2015-2019 Applicant Data COPY.xlsx", sheet = "Original Pivot Table", na = "n/a", col_types = column_names)
################# END TEST CODE ##############################

