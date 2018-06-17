# analysis code in R of the 2018 June 15 parameter sweep

#-------------------------
#    ASSEMBLE DATA
#-------------------------

# create 'ModelList.txt' using, in a Linux terminal, "ls *dat > ModelList.txt" in the working directory
output.files<-read.csv("ModelList.txt",header=FALSE)
# make a list and pull each of the behaviorspace results into it, skipping 6 lines for the header
outfiles<-list()
for (i in 1:nrow(output.files))
	outfiles[[as.character(output.files[i,]) ]]<-read.csv(as.character(output.files[i,]),skip=6)
outfiles<-outfiles[!(names(outfiles) %in% c("RunSoftwareTests.dat"))] # remove software tests
outfiles<-outfiles[!(names(outfiles) %in% c("ShortRunForTesting.dat"))] # remove software tests

#check that you have all runs you expect and they have the same number of columns
for (i in 1:length(outfiles))
	print(nrow(outfiles[[i]]))
for (i in 1:length(outfiles))
	print(length(outfiles[[i]]))

# compile them into one table/data object
d<-outfiles[[1]]
for (i in 2:length(outfiles))
	d<-rbind(d,outfiles[[i]])

# add a total internal time variable, a burn-in variable, and a sustainability variable
d$total.internal.time<-d$model.setup.time+d$model.run.time
hist(d$total.internal.time)
hist(d$total.internal.time[d$total.internal.time>400])
d$sustainable<-d$years.gone==60
d$burn.in<-d$min.livestock.number==999999.0
write.csv(d,"Exp_2018_05_15.csv")

#because we only used two values of each of these variables, make them factors (categorical)
d$times.per.day.farmers.move.cows<-factor(d$times.per.day.farmers.move.cows)
d$how.long.to.store.grain<-factor(d$how.long.to.store.grain)
d$cow.proportion.to.save<-factor(d$cow.proportion.to.save)
d$key.resources<-factor(d$key.resources)
d$muonde.projects<-factor(d$muonde.projects)
d$subsidy<-gsub("\"","",d$subsidy,perl=TRUE)
d$subsidy.proportion<-paste(d$subsidy,d$cow.proportion.to.save,sep="-")
d$subsidy.proportion<-factor(d$subsidy.proportion, 
	levels=c("no-0","feed-0.7","feed-1","transport-0.7","transport-1"))
summary(d)


#-------------------------
# CHECK VAR DISTRIBUTIONS
#-------------------------

par(mfrow=c(4,5))
for(i in 15:33)
	hist(d[[names(d)[i]]],main=names(d)[i])
par(mfrow=c(1,1))

hist(d$proportion.crops)

par(mfrow=c(2,2))
plot(d$morans.i,jitter(d$clumpiness), col=cut(d$proportion.crops,breaks=8))
legend("topleft",legend=sort(unique(cut(d$proportion.crops,breaks=8))),col=sort(unique(cut(d$proportion.crops,breaks=8))),pch=rep(1,8),title="Proportion Crops")
plot(d$gearys.c,jitter(d$clumpiness),col=cut(d$proportion.crops,breaks=8))
plot(d$average.contiguous.crop.cluster.size,jitter(d$clumpiness),col=cut(d$proportion.crops,breaks=8))
plot(d$total.crop.perimeter,jitter(d$clumpiness),col=cut(d$proportion.crops,breaks=8))
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(d$morans.i)
hist(d$gearys.c)
hist(d$average.contiguous.crop.cluster.size)
hist(d$total.crop.perimeter)
par(mfrow=c(1,1))
#reasonably good distributions, though crop clump size has a spike at very small clumps.
#Moran's I and Geary's C seem to have almost inverse behavior here.

#-------------------------
#  LOGIC CHECKS
#-------------------------

plot(d$invincible.fences,d$max.percentage.harvest.eaten)

plot(d$subsidy.used~d$rainfall.type)

#Other variables to check the logic of?

#-------------------------
#   CALIBRATION CHECK
#-------------------------

# for calibration, we want to restrict the user-controlled input parameters to 'realistic' values.

cd<-d[ d$proportion.crops<=65 & d$proportion.crops>=40 & d$muonde.projects==0 & d$invincible.fences=="false" & (d$rainfall.type=="\"historical\"" | d$rainfall.type=="\"random\"") & d$times.per.day.farmers.move.cows==1 & d$subsidy!="no" & d$cow.proportion.to.save==0.7, ]
nrow(cd) # 57
cd<-cd[cd$years.gone==60,]
nrow(cd) # 10
summary(cd)

#calibration results:
#harvest  1153-4791 (too low)
#livestock 2-85 (ok, but kinda low)
#min-woodland 5.8-24.1 
#subsidy  mean 2,776,343 max 9,866,734 (kinda low)
#harvest-eaten 0.69-1.20

#-------------------------
# CHECK MUONDE THRESHOLDS
#-------------------------

summary(d[d$use.muonde.thresholds=="true",])
# none lasted longer than 2 years
table(d$use.muonde.thresholds,d$sustainable)
#none of the models using muonde thresholds made it to 60 years.

#-------------------------
#   SENSITIVITY CHECK
#-------------------------

dnm<-d[d$use.muonde.thresholds=="false",]
dnm<-dnm[dnm$min.livestock.number!=999999.0,] 
# too-few-cows 508, too-little-harvest 1764, too-little-woodland 290, finished: 1425

dnm$total.crop.perimeter2<-dnm$total.crop.perimeter*dnm$total.crop.perimeter

library(mgcv)
#testing the sensitivity of underlying parameters (along with management parameters)
sens.test.nm<-gam(sustainable~subsidy.proportion
	+times.per.day.farmers.move.cows+invincible.fences
	+key.resources+rainfall.type+muonde.projects
	+how.long.to.store.grain+total.mud.crop.perimeter
	+wood.to.build.fence.per.meter+termite.activity
	+hours.to.plough.ha+crop.growth.slope
	+zero.crop.growth.intercept+muonde.efficiency
	+woodland.growth.slope+cow.maintenance.energy.rate
	+cow.working.energy.per.hour+kcal.per.kg.of.browse
	+kcal.per.kg.of.crop+kcal.per.kg.of.cow+production.efficiency
	+catabolism.efficiency+min.cow.mass+max.cow.mass
	+calf.birth.mass+livestock.not.reproduction.rate.per.year
	+morans.i+total.crop.perimeter + total.crop.perimeter+total.crop.perimeter2
	+s(proportion.crops)+s(average.contiguous.crop.cluster.size),data=dnm,family="binomial")

summary(sens.test.nm)

expit<-function(x) {
exp(x)/(exp(x)+1)
}
plot(sens.test.nm,trans=expit)

#figure for ODD
png("ProportionCrops.png",res=300,width = 16, height = 8, units = 'in')
par(mfrow=c(1,2))
plot(sens.test.nm,trans=expit,ylab="Probability of Sustainability")
par(mfrow=c(1,1))
dev.off()

# (Intercept)                                 
# subsidy.proportionfeed-0.7               ***
# subsidy.proportionfeed-1                    
# subsidy.proportiontransport-0.7          ***
# subsidy.proportiontransport-1            ***
# times.per.day.farmers.move.cows1         ** 
# invincible.fencestrue                       
# key.resources10                          ***
# rainfall.type"extreme"                   ***
# rainfall.type"historical"                ***
# rainfall.type"random"                    ***
# rainfall.type"statistical-extreme"       ***
# rainfall.type"statistical-random"        ***
# muonde.projects10                           
# how.long.to.store.grain3                 ***
# total.mud.crop.perimeter                    
# wood.to.build.fence.per.meter               
# termite.activity                            
# hours.to.plough.ha                       .  
# crop.growth.slope                           
# zero.crop.growth.intercept                  
# muonde.efficiency                           
# woodland.growth.slope                    *  
# cow.maintenance.energy.rate                 
# cow.working.energy.per.hour                 
# kcal.per.kg.of.browse                       
# kcal.per.kg.of.crop                         
# kcal.per.kg.of.cow                          
# production.efficiency                       
# catabolism.efficiency                       
# min.cow.mass                                
# max.cow.mass                                
# calf.birth.mass                          .  
# livestock.not.reproduction.rate.per.year .  
# morans.i                                 *  
# total.crop.perimeter                     ***
# total.crop.perimeter2                    ***

# Approximate significance of smooth terms:
#                                           edf Ref.df Chi.sq  p-value    
# s(proportion.crops)                     6.241  7.333 133.16  < 2e-16 ***
# s(average.contiguous.crop.cluster.size) 5.316  6.372  29.17 8.49e-05 ***

#intercept plus parameter estimate for woodland growth slope
x<-1.84E+00+3.34E-01

100* (expit(x) - expit(1.84E+00))

#3.5 percent difference in sustainability for a 1-unit increase in the growth slope.
