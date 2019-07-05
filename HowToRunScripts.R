#### accumulated all required code, not script freindly

#Step 1, read in file shortcuts and functions from functin script

#Step 2, creating vector of unique ideas to be used to construct futher data.frame
string <- c(NA)
for(n in 1:length(data)){
  string <-c(string,data[[n]])
}
string <- string[-1]
length <- c()
for(n in 1:180){
  length[n] <- length(data[[n]])
}
sum(length)
length(string)
unique <- unique(string)
OGunique <- unique
scanned <- c()
for(n in 1:length(string)){
  if(str_detect(string[n],"[:alpha:]")){
    scanned <- c(scanned,string[n])
  }
}
unique <- unique(scanned)

#Step 3... Profit

#Step 4, create a vector denoting presence/abscence and make a data frame of it. ID , Event #, Trap #, Lat/Long, Bottle direction
Found <- c()
for (d in 1:180){
  for(n in 1:length(unique)){
    ifelse(length(grep(unique[n],data[[d]]))>=1,Found <- c(Found,1),Found <- c(Found,0))
  }
}

ID <- rep(unique,times=180)
Bottle <- rep(rep(c("N","E","S","W"),each = length(unique)),times=5*9)
Event <- rep(c(1:9),each = (length(unique)*20))
LongX <- rep(rep(c(traps[[1]][[1]][1],traps[[2]][[1]][1],traps[[3]][[1]][1],traps[[4]][[1]][1],traps[[5]][[1]][1]), each=length(unique)*4),times=9)
LatY <- rep(rep(c(traps[[1]][[2]][1],traps[[2]][[2]][1],traps[[3]][[2]][1],traps[[4]][[2]][1],traps[[5]][[2]][1]), each=length(unique)*4),times=9)
Trap <- rep(rep(c(1,2,3,4,5), each=length(unique)*4),times=9)
basic <- data.frame("ID" = c(ID), "LongX" = c(LongX), "LatY" = c(LatY), "Event" = c(Event), "Trap" = c(Trap), "Bottle" = Bottle, "Found" = c(Found))

#Step 5, read in vector containing trap/event appearence rate, inlcude code for this at bottom but takes too long to run regualy
Repeat <- read.table("C:\\Users\\Julia\\Documents\\AGM Field Map\\S19\\repeatdata.txt")
basic <- cbind(basic[,1:6],Repeat,basic[,7])
names(basic)[names(basic)=="x"] <- "Repeat"
names(basic)[names(basic)=="basic[, 7]"] <- "Found"

#Step 6, build the models
mod1 <- lm(Found~ LongX+LatY+as.factor(Event)+as.factor(Repeat), data= basic) #basic regression
mod2 <- glm(Found~ LongX+LatY, family = binomial, data = basic) #glm logic link, only location variables, AIC = 249646
mod3 <- glm(Found~ LongX+LatY, family = binomial(link = "probit"), data = basic) #glm logic link, only location variables, AIC = 249648
mod4 <- glm(Found~ LongX+LatY+Event, family = binomial, data = basic) #glm logic link, location  and event variables, AIC = 246367
mod5 <- glm(Found~ LongX+LatY+as.factor(Event), family = binomial, data = basic) #glm logic link, location  and event (as.facotr) variables, AIC = 245864
mod6 <- glm(Found~ LongX+LatY+as.factor(Event)+Repeat, family = binomial, data = basic) #glm logic link, location, event (as.facotr) and Repeat variables, AIC = 139459
mod7 <- glm(Found~ LongX+LatY+as.factor(Event)+as.factor(Repeat), family = binomial, data = basic) #glm logic link, location, event (as.facotr) and Repeat (as.factor) variables, AIC = 132435
mod8 <- glm(Found~ LongX+LatY+as.factor(Repeat), family = binomial, data = basic) #glm logic link, location and Repeat (as.factor) variables, AIC = 132504

#Step 7, Create Training and Testing data sets and perform downsampling or upsampling
library(caret) 
DataPartition <- createDataPartition(basic$Found, p=0.7, list = F)  
TrainingData <- basic[DataPartition, ]
TestingData <- basic[-DataPartition, ]
TrainingData$Found <- as.factor(TrainingData$Found)
DownSampled <- downSample(x = TrainingData[,1:7],y = TrainingData$Found, yname="Found")
UpSampled <- upSample(x = TrainingData[,1:7],y = TrainingData$Found, yname="Found")

#Step 8, Redo selected models using the trainign data (down and up smapled versions)
mod2DS <- glm(Found~ LongX+LatY, family = binomial, data = DownSampled) 
mod2US <- glm(Found~ LongX+LatY, family = binomial, data = UpSampled) 
mod5DS <- glm(Found~ LongX+LatY+as.factor(Event), family = binomial, data = DownSampled) 
mod5US <- glm(Found~ LongX+LatY+as.factor(Event), family = binomial, data = UpSampled) 
mod7DS <- glm(Found~ LongX+LatY+as.factor(Event)+as.factor(Repeat), family = binomial, data = DownSampled)
mod7US <- glm(Found~ LongX+LatY+as.factor(Event)+as.factor(Repeat), family = binomial, data = UpSampled)

#Step 9, use these models on testing data to precit and then compare accuracy
PredMod2DS <- predict(mod2DS, newdata = TestingData, type = "response")
PsAsMod2DS <- ifelse(PredMod2DS>=0.5,1,0)
mean(PsAsMod2DS == TestingData$Found)
PredMod2US <- predict(mod2US, newdata = TestingData, type = "response")
PsAsMod2US <- ifelse(PredMod2US>=0.5,1,0)
mean(PsAsMod2US == TestingData$Found)
PredMod5DS <- predict(mod5DS, newdata = TestingData, type = "response")
PsAsMod5DS <- ifelse(PredMod5DS>=0.5,1,0)
mean(PsAsMod5DS == TestingData$Found)
PredMod5US <- predict(mod5US, newdata = TestingData, type = "response")
PsAsMod5US <- ifelse(PredMod5US>=0.5,1,0)
mean(PsAsMod5US == TestingData$Found)
PredMod7DS <- predict(mod7DS, newdata = TestingData, type = "response")
PsAsMod7DS <- ifelse(PredMod7DS>=0.5,1,0)
mean(PsAsMod7DS == TestingData$Found)
PredMod7US <- predict(mod7US, newdata = TestingData, type = "response")
PsAsMod7US <- ifelse(PredMod7US>=0.5,1,0)
mean(PsAsMod7US == TestingData$Found)

#Step 10, Build the preditor variable vectors requirred for extrapilation

cand <- c()
pull <- c()
count <- c()
Repeat <- c()
for (n in 1:nrow(basic)){
  cand <- which(basic$ID == basic$ID[n])
  if(n <= 66400){
    pull <- cand[1:20]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  } else if (n <= 66400*2){
    pull <- cand[21:40]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }else if (n <= 66400*3){
    pull <- cand[41:60]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }
  else if (n <= 66400*4){
    pull <- cand[61:80]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }
  else if (n <= 66400*5){
    pull <- cand[81:100]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }
  else if (n <= 66400*6){
    pull <- cand[101:120]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  } else if (n <= 66400*7){
    pull <- cand[121:140]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }else if (n <= 66400*8){
    pull <- cand[141:160]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }else if (n <= 66400*9){
    pull <- cand[161:80]
    count[1] <- ifelse(sum(basic$Found[c(pull[1:4])])>=1,1,0)
    count[2] <- ifelse(sum(basic$Found[c(pull[5:8])])>=1,1,0)
    count[3] <- ifelse(sum(basic$Found[c(pull[9:12])])>=1,1,0)
    count[4] <- ifelse(sum(basic$Found[c(pull[13:16])])>=1,1,0)
    count[5] <- ifelse(sum(basic$Found[c(pull[17:20])])>=1,1,0)
    Repeat[n] <- sum(count)
  }
}

  
  
  
  


