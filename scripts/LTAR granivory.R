rm(list=ls())

library(readxl)
library(tidyverse)

data<-read_csv("../data/Seed predation data.csv")	#Seed predation data saved in Lauren's Google Drive

##Convert character data to factors
	data$site<-as.factor(data$site)
	data$crop<-as.factor(data$crop)
	data$strip<-as.factor(data$strip)
	data$transect<-as.factor(data$transect)
	data$dist.fact<-as.factor(data$dist)

##Convert date to appropriate format
	data$date<-as.Date(data$date, "%m/%d/%y")

##Drop NAs
	data<-subset(data, seeds!="NA")

##Look at some summary stats by treatment
#Summarize
library(plyr)
	mean.data<- ddply(data, c("strip", "dist", "date"), summarise,
		mean = mean(seeds),
		upperCI = mean + sd(seeds) / sqrt(length(seeds))*qt(.975, length(seeds)-1),
		lowerCI = mean - sd(seeds) / sqrt(length(seeds))*qt(.975, length(seeds)-1)
	)

#Plot the summary stats
library(ggplot2)
pd <- position_dodge(2)
	ggplot(data = mean.data, aes(x=dist, y=mean, colour = strip, group = strip)) +
		geom_line(position=pd) +
		geom_point(position=pd)+
		xlab("Distance (m)") +
		ylab("Seeds remaining") +
		geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=2, position=pd) +
		facet_grid(rows = vars(date))

##Analysis (distance as continuous variable)
library(lmerTest)		#A package that gives p-values for the lmer summary and anova
	data.rand<-lmer(seeds~dist*strip+crop+season+(1|site/date),data=data)
	anova(data.rand)
	summary(data.rand)

##View least squared means to compare strip vs. no strip?
#Need to convert distance to factor and rerun analysis
	data.rand.fact<-lmer(seeds~dist.fact*strip+crop+season+(1|site/date),data=data)
	anova(data.rand.fact)
	summary(data.rand.fact)

#Plot lsmeans
library(lsmeans)
	lsmean.sum<-summary(lsmeans(data.rand.fact,pairwise~dist.fact*strip,adjust = "tukey", conf=0.95))
	lsmean.sum
	ggplot(data = lsmean.sum$lsmeans, aes(x=dist.fact, y=lsmean, colour = strip, group = strip)) +
		geom_line(position=position_dodge(0.2)) +
		geom_point(position=position_dodge(0.2))+
		xlab("Distance (m)") +
		ylab("Seeds remaining (lsmean)") +
		geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.2, position=position_dodge(0.2))

	