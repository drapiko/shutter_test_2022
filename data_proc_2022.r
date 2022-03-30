# data processing for the resource test of pneumatic shutter actuator
#written by drapiko e march 2022
#first3 day position sensor calibration screwed 
#last two ok
##################################################################################
# DAY1
##################################################################################

t04 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test04.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
t04$Pos2<-t04$Pos2/5.5#applying position correction due to wrong saving
t04$Pos2<--((t04$Pos2*(-1.185))/5+4.265-3.08)/0.2+6 #applying sensor calibration factors

s=5206
e=70000

#plot(t04$Pos2[s:e])
#grid()
posmm<-t04$Pos2
i=1
j=1
posmmm<-NULL
for (i in s:e) {# picking flattops
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){#selecting only flattop values
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
i=1
j=1
posmmin<-NULL
for (i in s:e) {#picking flatbottoms
  if(abs(posmm[i]-posmm[i+1]<0.005)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
#plot(posmmm[40:50])
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade Position Histogram, mm", main = paste("Day1 09.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()
################################################################################
# DAY2 10.03.2022
###############################################################################
t05 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test05.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
t05$Pos2<-t05$Pos2/5.5
t05$Pos2<--((t05$Pos2*(-1.185))/5+4.265-3.08)/0.2+6
#plot(t05$Pos2[10005:10009])
#grid()
posmm<-t05$Pos2
s=1000
e=76300
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}

i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.005)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]


posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
#plot(t05$Pos2[10000:76000])
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade Position Histogram, mm", main = paste("Day2 10.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()
################################################################################
#DAY3 11.03.2022
################################################################################
t06 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test06.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
t06$Pos2<-t06$Pos2/5.5
t06$Pos2<--((t06$Pos2*(-1.185))/5+4.265-3.08)/0.2+6

#plot(t06$Pos2[1:64000])
posmm<-t06$Pos2
s=1000
e=64000
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}

i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.005)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day3 11.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()

################################################################################
#DAY4 14.03.2022
################################################################################

t07 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test07.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
t07$Pos2<--((t07$Pos2*(-1.185))/5+4.265-3.08)/0.2+6
#plot(t07$Pos2[1000:69000])
#grid()
posmm<-t07$Pos2

s=1000
e=69000
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}

i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.005)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day4 14.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()


################################################################################
#DAY5 15.03.2022
################################################################################

t08 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test08.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
t08$Pos2<--((t08$Pos2*(-1.185))/5+4.265-3.08)/0.2+6
posmm<-t08$Pos2
#plot(posmm)
s=100
e=52000
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
  }

i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.005)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day5 15.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()

##################################################
#DAY6 16.03.2022 cam
##################################################

t08$Pos1<-((1-t08$Pos1/5)*328)/52.27
#plot(t08$Pos1[3000:52000])

posmm<-t08$Pos1

s=2000
e=52000
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.01)&(posmm[i]>6.1)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}

i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.01)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm>6.14]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day6 CAM 15.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()


###########################################
#DAY6_1 16.03.2022
###########################################
t081 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test08_1.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))

t081$Pos2<--((t081$Pos2*(-1.185))/5+4.265-3.08)/0.2+6
posmm<-t081$Pos2
#plot(t081$Pos2[2020:2040])

s=100
e=28000
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
#hist(posmmm,breaks = 40,xlab = "Blade position, mm", main = "Position Histogram day6 16.03.2022")
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.005)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day6_1 16.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()

####################cam#########################333
############3######################################3333
t081$Pos1<-((1-t081$Pos1/5)*328)/52.27
#plot(t08$Pos1[3000:52000])

posmm<-t081$Pos1

i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
#hist(posmmm,breaks = 40,xlab = "Blade position, mm", main = "Position Histogram day6 16.03.2022")
i=1
j=1
posmmin<-NULL

for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.4&posmmm>0]-0.0922
posmmm<-posmmm[posmmm>6.15]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day6_1 CAM 16.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()


#############################################################
### DAY7
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

t011 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test011.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
#t011$Pos2<-((t011$Pos2*(-1.185))/5+4.265-3.08)/0.2
t011$Pos2<--((t011$Pos2*(-1.185))/5+4.265-3.08)/0.2+6
#t011$Pos2<-(t011$Pos2/5)*6
posmm<-t011$Pos2
#plot(t011$Pos2[1:62000])
grid()

s=25000
e=62500
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.01)&(posmm[i]>4.9)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
#hist(posmmm,breaks = 50,xlab = "Blade position, mm", main = "Position Histogram day7 22.03.2022")
i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day7 22.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()

###############################################################################33333
#################### DAY7 CAM
###############################################################################34
t011$Pos1<-((1-t011$Pos1)*328)/52.27
plot(t011$Pos1[24005:24025])
posmm<-t011$Pos1
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.01)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
#hist(posmmm,breaks = 40,xlab = "Blade position, mm", main = "Position Histogram day7 cam 22.03.2022")
i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}

posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm>6.08]
posmmm<-posmmm[posmmm<6.22]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day7 CAM 22.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()

################################################################################33333
###############day8
##################################################################################3
t012 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test012.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
t012$Pos2<--((t012$Pos2*(-1.185))/5+4.265-3.08)/0.2+6
#plot(t012$Pos2)
posmm<-t012$Pos2

s=2500
e=50000
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.01)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
#hist(posmmm,breaks = 50,xlab = "Blade position, mm", main = "Position Histogram day7 22.03.2022")

i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}
posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
posmmm<-posmmm[posmmm<6.5]
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day8 22.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()

############################################
###########day8 CAM
############################################
t012$Pos1<-((1-t012$Pos1)*328)/52.27
#plot(t012$Pos1[24005:24025])
posmm<-t012$Pos1
i=1
j=1
posmmm<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.01)&(posmm[i]>6)){
    print(posmm[i])
    print(i)
    posmmm[j]<-posmm[i+1]
    j=j+1
  }
}
#hist(posmmm,breaks = 40,xlab = "Blade position, mm", main = "Position Histogram day7 cam 22.03.2022")
i=1
j=1
posmmin<-NULL
for (i in s:e) {
  if(abs(posmm[i]-posmm[i+1]<0.05)&(posmm[i]<1)){
    print(posmm[i])
    print(i)
    posmmin[j]<-posmm[i+1]
    j=j+1
  }
}

posmmin<-posmmin[posmmin<0.21&posmmin>0]
posmmm<-posmmm[posmmm<6.7&posmmm>0]-mean(posmmin)
enorm_res<-EnvStats::enorm(posmmm, ci=TRUE)
hist(posmmm,prob=T,breaks = 30,xlab = "Blade position Histogram, mm", main = paste("Day8 CAM 22.03.2022","M=",enorm_res$parameters["mean"],"Sd=",enorm_res$parameters["sd"]))
rug(posmmm);lines(density(posmmm));grid()











#t013<-NULL
#ttp013<-NULL
#t013 <- read.delim2("E:/iter/h-alpha/испытания/2022/shutter/data/test013.txt" , dec=".",colClasses = c("Time"="character","Cycle"="numeric","Temperature"="numeric", "Preasure1"="numeric","Preasure2"="numeric","Pos1"="numeric","Pos2"="numeric","Vac"="numeric","Valv1"="numeric","Valv2"="numeric"))
#tt013<-c(t013$Time)
#ttp013<-as.POSIXct(tt013,format="%H:%M:%S")
#plot(ttp013[],t013$Temperature)
#plot(t013$Temperature[59000:74900], type = "l")

#tt32<-c(t32$Time)
#ttp32<-as.POSIXct(tt32,format="%H:%M:%S")
