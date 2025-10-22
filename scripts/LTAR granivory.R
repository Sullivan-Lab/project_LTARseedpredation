rm(list=ls())

library(readxl)
library(tidyverse)

data<-read_csv("../data/Seed predation data.csv")	#Seed predation data saved in Lauren's Google Drive

##Convert character data to factors (not character or numerical)
data$site<-as.factor(data$site)
data$crop<-as.factor(data$crop)
data$strip<-as.factor(data$strip)
data$transect<-as.factor(data$transect)
data$dist.fact<-as.factor(data$dist)

##Convert date to appropriate format
#Correction: "Y" must be capitalized or else R will use the first 2 digits of the year (not the last 2)
data$date<-as.Date(data$date, "%m/%d/%Y")

##Drop NAs (any observation where there is not an entry for number of seeds remaining)
data<-subset(data, seeds!="NA")

##Look at some summary stats by treatment
#Summarize to look at means within treatment by season/distance
library(plyr)
mean.data<- ddply(data, c("strip", "dist", "date", "season"), summarise,
                  mean = mean(seeds),
                  upperCI = mean + sd(seeds) / sqrt(length(seeds))*qt(.975, length(seeds)-1),
                  lowerCI = mean - sd(seeds) / sqrt(length(seeds))*qt(.975, length(seeds)-1)
)

#Make fall graphs first
#Limit the summarized mean.data to only "strips"
strip<-subset(mean.data,strip=="Y")
#Limit data to fall
fall.strip<-subset(strip,season=="fall")

#Make the "no strip" lines for fall
#This assumes that all points from "N" transects are essentially the same for purposes of comparison to the "Y" transects
#Find the mean and 95% C.I. for all points near strips or field centers by treatment, date, and season
mean.strip<- ddply(data, c("strip", "date", "season"), summarise,
                   mean = mean(seeds),
                   upperCI = mean + sd(seeds) / sqrt(length(seeds))*qt(.975, length(seeds)-1),
                   lowerCI = mean - sd(seeds) / sqrt(length(seeds))*qt(.975, length(seeds)-1)
)

#Find the mean number of seeds remaining in fields without strips
fall.line<-subset(mean.strip,season=="fall"&strip=="N")
fall.means<-data.frame(factor(fall.line$date), fall.line$mean)
colnames(fall.means)<-c("date","fall.mean")

#Make 95% confidence intervals around the mean number of seeds remaining in fields without strips
fall.upperCI<-data.frame(factor(fall.line$date), fall.line$upperCI)
colnames(fall.upperCI)<-c("date","fall.upperCI") #Fixes weird column names

fall.lowerCI<-data.frame(factor(fall.line$date), fall.line$lowerCI)
colnames(fall.lowerCI)<-c("date","fall.lowerCI") #Fixes weird column names



#Plot fall data (seeds remaining in fields with strips relative to distance from strip; compare to fields without strips)
library(ggplot2)
ggplot(data = fall.strip, aes(x=dist, y=mean)) +	#Basic graph
  geom_line(linewidth = 1, colour = "black") +		#Connect points
  geom_point(size = 3, colour = "black")+		#Make points reasonable size
  xlab("Distance (m)") +					#X-axis label
  ylab("Seeds remaining") +				#Y-axis label
  geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), 
                width=4, linewidth = 1, colour = "black") +	#Add 95% C.I. error bars to points
  facet_grid(rows = vars(date))+			#Break up data by date
  geom_hline(data = fall.means, 
             aes(yintercept = c(fall.mean)), linewidth =1, 
             colour = "gray")+					#Add mean seeds remaining in fields without strip
  geom_hline(data = fall.upperCI, 
             aes(yintercept = c(fall.upperCI)), 
             linetype="dashed", linewidth = 1, 
             colour = "gray")+					#Add 95% C.I. around mean seeds in field without strip
  geom_hline(data = fall.lowerCI, 
             aes(yintercept = c(fall.lowerCI)), 
             linetype="dashed", linewidth = 1, 
             colour = "gray")+					#Finishing up with the C.I.
  theme(axis.text.y=element_text(colour="black"),	#All the rest of this is formatting for the graph
        axis.text.x=element_text(colour="black"),
        axis.ticks=element_line(colour="black"),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.position = "none",
        legend.title = element_text(size=12),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill="white"),
        legend.text = element_text(size = 12),
        axis.line.x=element_line(colour="black",size=0.5,linetype="solid"),
        axis.line.y=element_line(colour="black",size=0.5,linetype="solid"),
        text = element_text(size = rel(4.5)))


#Now, spring graphs
#Limit data to spring
spring.strip<-subset(strip,season=="spring")

#Make the "no strip" lines for spring
#Mean seeds remaining in fields without strips
spring.line<-subset(mean.strip,season=="spring"&strip=="N")
spring.means<-data.frame(factor(spring.line$date), spring.line$mean)
colnames(spring.means)<-c("date","spring.mean")

#95% confidence intervals
spring.upperCI<-data.frame(factor(spring.line$date), spring.line$upperCI)
colnames(spring.upperCI)<-c("date","spring.upperCI")

spring.lowerCI<-data.frame(factor(spring.line$date), spring.line$lowerCI)
colnames(spring.lowerCI)<-c("date","spring.lowerCI")

#Plot spring data
ggplot(data = spring.strip, aes(x=dist, y=mean)) +
  geom_line(linewidth = 1, color="black") +
  geom_point(size = 3, color="black")+
  xlab("Distance (m)") +
  ylab("Seeds remaining") +
  geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), 
                width=4, linewidth = 1, color="black") +
  facet_grid(rows = vars(date))+
  geom_hline(data = spring.means, 
             aes(yintercept = c(spring.mean)), linewidth =1, 
             color="gray")+
  geom_hline(data = spring.upperCI, 
             aes(yintercept = c(spring.upperCI)), 
             linetype="dashed", linewidth = 1, 
             color="gray")+
  geom_hline(data = spring.lowerCI, 
             aes(yintercept = c(spring.lowerCI)), 
             linetype="dashed", linewidth = 1, 
             color="gray")+
  theme(axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        axis.ticks=element_line(colour="black"),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.position = "none",
        legend.title = element_text(size=12),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill="white"),
        legend.text = element_text(size = 12),
        axis.line.x=element_line(colour="black",size=0.5,linetype="solid"),
        axis.line.y=element_line(colour="black",size=0.5,linetype="solid"),
        text = element_text(size = rel(4.5)))

#Yay!  Graphs are done.  On to the analysis.

library(lmerTest)		#A package that gives p-values for the lmer summary and anova
#Limited interactions to make this manageable by my brain, but you can add more
#Original question was dist*strip, but we also want to know if the season affects the strip effect
#Other interactions seem excessive?
#NOTE: distance is being run as a continuous variable here
data.rand<-lmer(seeds~dist*strip+season*strip+(1|site/date),data=data)
#And the results are...
anova(data.rand)
summary(data.rand)
#But I want to know how big the effect was!
summary(data.rand)

####Lauren updates


data.rand<-lmer(seeds~dist*strip+season+(1|site/date),data=data)
#And the results are...
anova(data.rand)
summary(data.rand)


library(glmmTMB)
data$proportion <- data$seeds/50

mod1<-glmmTMB(proportion~dist*strip+season+(1|site/date),
                   data=subset(data, proportion < 1), 
                   family = binomial)


#And the results are...
summary(mod1)


##Do we want to consider distance as a multi-level factor (not a numerical value)?
##This may be important as we are dealing with non-linear relationships (but doesn't really change much)
#data.rand.fact<-lmer(seeds~dist.fact*strip+season*strip+(1|site/date),data=data)
##Results
#anova(data.rand.fact)
##Effect sizes
#summary(data.rand.fact)

## what about crop type???
mod2<-glmmTMB(proportion~dist*strip+season+crop+(1|site/date),
              data=subset(data, proportion < 1), 
              family = binomial)


#And the results are...
summary(mod2)

	