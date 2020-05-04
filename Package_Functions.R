
library(stringr)
library(data.table)
library(caret)
library(viridisLite)

SetupFun <- function(Border_Coordinates, Trap_Coordinates,  Number_of_Sampling_Events = NULL, Slam_trap = FALSE){
  if(!is.data.frame(Border_Coordinates) && !is.matrix(Border_Coordinates)){
    stop("Border Coordinates need to be stored as either a dataframe or a matrix.", call.= F)
  } else {
    BC <- c(Border_Coordinates[1,1],Border_Coordinates[1,2]) 
    for (n in 2:nrow(Border_Coordinates)) {
      BC <- c(BC,Border_Coordinates[n,1],Border_Coordinates[n,2])
    }
  }
  if(!is.data.frame(Trap_Coordinates) && !is.matrix(Trap_Coordinates)){
    stop("Trap Coordinates need to be stored as either a dataframe or a matrix.", call. = F)
  } else {
    TC <- c(Trap_Coordinates[1,1],Trap_Coordinates[1,2]) 
    for (n in 2:nrow(Trap_Coordinates)){
      TC <- c(TC,Trap_Coordinates[n,1],Trap_Coordinates[n,2]) 
    }
  }
  Number_of_Traps <- length(TC)/2
  if (is.null(Number_of_Sampling_Events)||Number_of_Sampling_Events <= 0){
    Number_of_Sampling_Events <- 1
  }
  storage <- list(BC, TC, Number_of_Sampling_Events, Number_of_Traps, Slam_trap)
  return(storage)
}
Formatting_mBraveFun <- function(File, ID, Suffix_Symbol, Slam_trap = F) {
  if (!("runName" %in% names(File) && "bin_uri" %in% names (File))){
    stop("The correct fields have not been downloaded from mBrave", call. =  F)
  }
  split <- strsplit(File$runName, "[)]")
  g <- which(grepl(ID,split[[1]]))
  seperated <- as.data.frame(matrix(0,1,4))
  for (n in 1:length(split)){
    split[[n]] <- str_sub(split[[n]][g], str_locate(split[[n]],ID)[1,2] + 1)
    seperated[n,1] <- str_sub(split[[n]],str_locate_all(split[[n]], "[0-9]+")[[1]][,1],str_locate_all(split[[n]], "[0-9]+")[[1]][,2])[1]
    seperated[n,2] <- str_sub(split[[n]],str_locate_all(split[[n]], "[0-9]+")[[1]][,1],str_locate_all(split[[n]], "[0-9]+")[[1]][,2])[2]
    if(n == round(length(split)*0.5,0)){show("25%")}
    if(n == length(split)){show("50%")}
    if(Slam_trap) {
      if (length(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) == 1){
        if(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2]) == "N"){
          seperated[n,3] <- 1
        } else if(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2]) == "E"){
          seperated[n,3] <- 2
        } else if(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2]) == "S"){
          seperated[n,3] <- 3
        } else if(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2]) == "W"){
          seperated[n,3] <- 4
        }         
        seperated[n,4] <- File$bin_uri[n]
      } else if (length(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) >= 2) {
        if(grep("N", str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) == 1 ){
          seperated[n,3] <- 1
        } else if(grep("A",str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) == 1){
          seperated[n,3] <- 2
        } else if(grep("U",str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) == 1){
          seperated[n,3] <- 3
        } else if(grep("W", str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) == 1){
          seperated[n,3] <- 4
        }           
        seperated[n,4] <- File$bin_uri[n]
      } else if (length(str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2])) == 1){
        if(str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2]) == "n"){
          seperated[n,3] <- 1
        } else if(str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2]) == "e"){
          seperated[n,3] <- 2
        } else if(str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2]) == "s"){
          seperated[n,3] <- 3
        } else if(str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2]) == "w"){
          seperated[n,3] <- 4
        }         
        seperated[n,4] <- File$bin_uri        
      } else if (length(str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2])) >= 2){
        if(grep("n", str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2])) == 1 ){
          seperated[n,3] <- 1
        } else if(grep("a",str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2])) == 1){
          seperated[n,3] <- 2
        } else if(grep("U",str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2])) == 1){
          seperated[n,3] <- 3
        } else if(grep("W", str_sub(split[[n]], str_locate_all(split[[n]], "[a-z]+")[[1]][,1], str_locate_all(split[[n]], "[a-z]+")[[1]][,2])) == 1){
          seperated[n,3] <- 4
        }           
        seperated[n,4] <- File$bin_uri[n]        
      } else{
        stop("Bottle location on SLAM Trap has not been doccumented correctly. Please use E, East, e or east in the run$Name, directly after number related to sampling event ", call. =  F)
      }
    } else {
      seperated[n,3] <- File$bin_uri[n]
    }
  }
  event_lables <- as.numeric(sort(unique(seperated[,2])))
  trap_lables <- as.numeric(sort(unique(seperated[,1])))
  ready <- data.frame()
  if(Slam_trap){
    ready <- as.data.frame(matrix(0,1,length(event_lables)*length(trap_lables)*4))
    for(n in 1:length(split)){
      col <- (which(as.numeric(seperated[n,2]) == event_lables)-1)*length(trap_lables)*4 + (which(as.numeric(seperated[n,1]) == trap_lables)-1)*4 + seperated[n,3]
      ready[colSums(!is.na(ready))[col] + 1,col] <- seperated[n,4] 
      if(n == 1){ready <- ready[-1,]}
      if(n == round(length(split)*0.5,0)){show("75%")}
      if(n == length(split)){show("100%")}
    }      
  } else {
    ready <- as.data.frame(matrix(0,1,length(event_lables)*length(trap_lables)))
    for(n in 1:length(split)){
      col <- (which(as.numeric(seperated[n,2]) == event_lables)-1)*length(trap_lables) + which(as.numeric(seperated[n,1]) == trap_lables)
      ready[colSums(!is.na(ready))[col] + 1,col] <- seperated[n,3] 
      if(n == 1){ready <- ready[-1,]}
      if(n == round(length(split)*0.5,0)){show("75%")}
      if(n == length(split)){show("100%")}
    }
  }
  return(ready)  
}
DataFun <- function(File, Setup) {
  if (!is.data.frame(File)){
    stop("Sampling data must be stored as a dataframe.", call. = F)
  }
  if (Setup[[5]] &&  ncol(File) != (Setup[[4]]*4*Setup[[3]])){
    stop("Data provided for file does not match the parameters used in setup", calli.=F)
  } else if (!(Setup[[5]]) && ncol(File) != (Setup[[4]]*Setup[[3]])) {
    stop("Data provided for file does not match the parameters used in setup", calli.=F)
  }  
  trap <- list(c(1))
  for(n in 1:ncol(File)){
    trap[[n]] <- (File[,n])
    trap[[n]] <- as.character(trap[[n]])
    trap[[n]] <- ifelse(trap[[n]]=="",NA,trap[[n]])
    trap[[n]] <- trap[[n]][which(!is.na(trap[[n]]))]
  }
  return(trap)
}
ConversionFun <- function(Setup,km = F, Poles = F){
  Distance <- function(y1,y2,x1,x2, kmD = km){
    varR <- 6371e3
    radians <- function(d) {
      d * pi / 180
    }
    lat1 <- radians(y1)
    lat2 <- radians(y2)
    latdif <- radians((y2 - y1))
    londif <- radians((x2-x1))
    equ1 <- (sin((latdif/2)))^2 + cos(lat1) * cos(lat2) * (sin((londif/2)))^2
    equ2 <- 2 * atan2(sqrt(equ1),sqrt(1-equ1))
    if (!(kmD)){
      return(varR*equ2)
    } else {
      return(varR*equ2/1000)
    }
  }
  k <- 2
  xcorners <- c()
  for (x in 1:(length(Setup[[1]])/2)){
    xcorners[x] <- Setup[[1]][k]
    k <- k+2
  }
  k <- 1
  ycorners <- c()
  for (y in 1:(length(Setup[[1]])/2)){
    ycorners[y] <- Setup[[1]][k]
    k <- k+2
  }
  if(Poles) {
    if(ycorners[1] > 0){
      ### Calculates length of x axis (starts at LMX goes to RMX along the line of BMY) ###
      LMX <- 180
      BMY <- min(ycorners)
      DX <- round(Distance(BMY,BMY,180,0,km=km),0)
      ### Calculates length of y axis (starts at BMY goes to TMY along the line of LMX) ###     
      DY <- DX
    } else {
      LMX <- 180
      BMY <- max(ycorners)     
      DX <- round(Distance(BMY,BMY,180,0,km=km),0)
      DY <- DX
    } 
  } else {
    BMY <- min(ycorners)
    LMX <- min(xcorners)
    DX <- round(Distance(min(ycorners),min(ycorners),min(xcorners),max(xcorners),km=km),0)
    DY <- round(Distance(min(ycorners),max(ycorners),min(xcorners),min(xcorners),km=km),0)
  }
  plot_outline <- c()
  for (k in 1:length(xcorners)){
    plot_outline <- c(plot_outline, round(Distance(BMY,BMY,LMX,xcorners[k],km=km),0),round(Distance(BMY,ycorners[k],LMX,LMX,km=km),0))
  }
  k <- 2
  xcorners <- c()
  for (x in 1:(length(Setup[[2]])/2)){
    xcorners[x] <- Setup[[2]][k]
    k <- k+2
  }
  k <- 1
  ycorners <- c()
  for (y in 1:(length(Setup[[2]])/2)){
    ycorners[y] <- Setup[[2]][k]
    k <- k+2
  }
  trap_locations <- c()
  for (k in 1:length(xcorners)){
    trap_locations <- c(trap_locations, round(Distance(BMY,BMY,LMX,xcorners[k],km=km),0),round(Distance(BMY,ycorners[k],LMX,LMX,km=km),0))
  }
  return(list(plot_outline,trap_locations))
}
MasterDataFrameFun <- function(Data,Setup,Conversion,Repeat=T,Model_Type){
  if(Model_Type=="Binomial"){
    Found <- c()
    Unique <- unique(unlist(Data))
    if(Setup[[5]]){
      total <- length(Data)*length(Unique)
      k <- 1
      for (d in seq(1,length(Data),by=4)){
        for(n in 1:length(Unique)){
          Found[(k-1)*length(Unique)+n] <- ifelse(length(grep(Unique[n],c(Data[[d]], Data[[d+1]], Data[[d+2]], Data[[d+3]])))>=1,1,0)
          if(length(Found) == round(total/4*0.25,0)){show("25%")}
          if(length(Found) == round(total/4*0.5,0)){show("50%")}
          if(length(Found) == round(total/4*0.75,0)){show("75%")}
          if(length(Found) == total/4){show("100%")}
          ID <- rep(Unique,length(Data)/4)
        }
        k <- k+1
      }
    } else {
      total <- length(Data)*length(Unique)
      for (d in 1:length(Data)){
        for(n in 1:length(Unique)){
          Found[(d-1)*length(Unique)+n] <- ifelse(length(grep(Unique[n],Data[[d]]))>=1,1,0)
          if(length(Found) == round(total*0.25,0)){show("25%.")}
          if(length(Found) == round(total*0.5,0)){show("50%")}
          if(length(Found) == round(total*0.75,0)){show("75%")}
          if(length(Found) == total){show("100%")}
        }
      }
      ID <- rep(Unique,length(Data))
    }
    if(Repeat){
      show("Repeat calculated for Event:")
      Repeat <- rep(NA,times=length(ID))
      Grouping <- seq(0,by=length(Unique),length.out = Setup[[4]])
      Check <- function(x){
        y <- which(Unique==ID[x])
        sum(Found[(Grouping+(length(Unique)*(M-1)))+y])
      }  
      sort <- matrix(1:length(ID),ncol=Setup[[3]]) 
      for(M in 1:Setup[[3]]){
        Repeat[sort[,M]] <- sapply(sort[,M],Check)
        show(M)
      }
    }else{
      Repeat <- rep(NA,times=length(ID))
    }
    
    Event <- rep(c(1:Setup[[3]]),each = ((length(Unique)*Setup[[4]])))
    LongX <- rep(Conversion[[2]][seq(1,length(Conversion[[2]])-1,by=2)], each=length(Unique),times=Setup[[3]])
    LatY <- rep(Conversion[[2]][seq(2,length(Conversion[[2]]),by=2)],each=length(Unique),times=Setup[[3]])
    Trap <- rep(c(1:Setup[[4]]),each=length(Unique),times=Setup[[3]]) 
    Frame <- data.frame("ID" = c(ID), "LongX" = c(LongX), "LatY" = c(LatY), "Event" = c(Event), "Trap" = c(Trap), "Repeat" = c(Repeat), "Found" = c(Found))
    return(Frame)
  } else if(Model_Type=="Linear"){
    if(Setup[[5]]){
      string <- seq(1,length=Setup[[4]]*Setup[[3]],by=4)
      Found <- NA
      for(n in string){
        Found <- c(Found,length(unique(c(Data[[n]],Data[[n+1]],Data[[n+2]],Data[[n+3]]))))       
      }
      Found <- Found[-1]
    } else{
      Found <-  sapply(Data,length)
    }
    LongX <- rep(Conversion[[2]][seq(1,length(Conversion[[2]])-1,by=2)],times=Setup[[3]])
    LatY <- rep(Conversion[[2]][seq(2,length(Conversion[[2]]),by=2)],times=Setup[[3]])
    Event <- rep(1:Setup[[3]],each=Setup[[4]])
    Frame <- data.frame("LongX" = c(LongX), "LatY" = c(LatY), "Event" = c(Event),"Found" = c(Found))
    return(Frame)
  } else {
    stop("Currently the only model types supported are 'Binomial' and 'Linear'", call.= F)
  }
}  
DownSampleFun <- function(Dataframe,p=0.7){
  DataPartition <- createDataPartition(Dataframe$Found, p=p, list = F)
  TrainingData <- Dataframe[DataPartition, ]
  TestingData <- Dataframe[-DataPartition, ]
  TrainingData$Found <- as.factor(TrainingData$Found)
  DownSampled <- downSample(x = TrainingData[,1:(ncol(Dataframe)-1)],y = TrainingData$Found, yname="Found")
  Export <- list(DownSampled,TrainingData,TestingData)
  return(Export)
}
AccuracyFun <- function(Model,Testdata){
  pre <- predict(Model, newdata = Testdata, type = "response")
  out <- ifelse(pre>=0.5,1,0)
  return(mean(out == Testdata$Found))
}
BorderFun <- function(Conversion){
  Outline <- Conversion[[1]] + 1
  county <- c(seq(2,length(Outline),by=2),2)
  countx <- c(seq(1,length(Outline),by=2),1)
  ally <- c()
  allx <- c()
  ROUND <- function(x){
    y <- round(x)
    if(y-x <= 0.5){
      return(y)
    } else {
      retunr(y-1)
    }
  }
  for(k in 1:(length(Outline)/2)){
    currenty <- (Outline[county[k]]:Outline[county[k+1]])
    currentx <- (Outline[countx[k]]:Outline[countx[k+1]])
    if(length(currenty) > length(currentx)){
      ally <- c(ally,currenty)
      currentx <- seq(Outline[countx[k]],Outline[countx[k+1]],length.out = length(Outline[county[k]]:Outline[county[k+1]]))
      allx <- c(allx,currentx)
      allx <- sapply(allx,ROUND)
    } else if(length(currenty)<length(currentx)){
      allx <- c(allx,currentx)
      currenty <- seq(Outline[county[k]],Outline[county[k+1]],length.out = length(Outline[countx[k]]:Outline[countx[k+1]]))
      ally <- c(ally,currenty)
      ally <- sapply(ally,ROUND)
    }else{
      ally <- c(ally,currenty)
      allx <- c(allx,currentx)
    }
  }
  First <- cbind(allx,ally)
  maxy <- max(First[,2]) 
  maxx <- max(First[,1])
  Second <- matrix(NA,1,2)
  for(y in 1:maxy){
    for(x in 1:maxx){
      Yones <- First[which(First[,1]==x),2]
      if(length(which(Yones>=y))>=1 && length(which(Yones<=y))>=1){
        Xones <- First[which(First[,2]==y)]
        if(length(which(Xones>=x))>=1 && length(which(Xones<=x))>=1){
          Second <- rbind(Second,c(x,y))
        }else{
          NULL
        }
      } else {
        NULL
      }
    }
    if(y == round(lenght(maxy)*0.25,0)){show("25%.")}
    if(y == round(lenght(maxy)*0.5,0)){show("50%")}
    if(y == round(lenght(maxy)*0.75,0)){show("75%")}
    if(y == lenght(maxy)){show("100%")}  
  }
  Final <- rbind(First,Second[-1,])
  return(Final)
}
PlotFUN <- function(Border, Setup, Dataframe, Model, Model_Type, Subset=NA){
  plotholder <- list()
  if(is.na(Subset)){ Subset <- 1:Setup[[3]] } 
  if(Model_Type=="Binomial"){  
    for(E in Subset){
      matrix <- matrix(NA,nrow=max(Border[,2]),ncol=max(Border[,1]))
      show(E)
      if(unique(!is.na(Dataframe$Repeat))){
        Unique <- as.character(Dataframe[1:(nrow(Dataframe)/Setup[[3]]/Setup[[4]]),1])
        Repeat <- Dataframe$Repeat[1:length(Unique)]
        for(n in 1:nrow(Border)){
          newdata <- data.frame("LongX" = rep(Border[n,1],time=length(Unique)),
                                "LatY" = rep(Border[n,2],time=length(Unique)),
                                "Event" = rep(E,time=length(Unique)),
                                "Repeat" = Repeat) 
          matrix[Border[n,2],Border[n,1]] <- sum(ifelse(predict(Model,newdata=newdata,type="response")>=0.5,1,0))
          if(n == round(nrow(Border)*0.25,0)){show("25%.")}
          if(n == round(nrow(Border)*0.5,0)){show("50%")}
          if(n == round(nrow(Border)*0.75,0)){show("75%")}
          if(n == nrow(Border)){show("100%")} 
        }
        plotholder[[E]] <- matrix
      } else {
        for(n in 1:nrow(Border)){
          newdata <- data.frame("LongX" = rep(Border[n,1],time=length(Unique)),
                                "LatY" = rep(Border[n,2],time=length(Unique)),
                                "Event" = rep(E,time=length(Unique))) 
          matrix[Border[n,2],Border[n,1]] <- sum(ifelse(predict(Model,newdata=newdata,type="response")>=0.5,1,0))
          if(n == round(nrow(Border)*0.25,0)){show("25%.")}
          if(n == round(nrow(Border)*0.5,0)){show("50%")}
          if(n == round(nrow(Border)*0.75,0)){show("75%")}
          if(n == nrow(Border)){show("100%")}
        }
        plotholder[[E]] <- matrix
      }
    }
  } else if(Model_Type== "Linear") {
    for(E in Subset){
      matrix <- matrix(NA,nrow=max(Border[,2]),ncol=max(Border[,1]))
      show(E)
      for(n in 1:nrow(Border)){
        newdata <- data.frame("LongX" = Border[n,1],
                              "LatY" = Border[n,2],
                              "Event" = E)
        matrix[Border[n,2],Border[n,1]] <- round(predict(Model,newdata=newdata),0)
        if(n == round(nrow(Border)*0.25,0)){show("25%.")}
        if(n == round(nrow(Border)*0.5,0)){show("50%")}
        if(n == round(nrow(Border)*0.75,0)){show("75%")}
        if(n == nrow(Border)){show("100%")}
      }
      plotholder[[E]] <- matrix
    }
  } else {
    stop("Currently the only model types supported are 'Binomial' and 'Linear'", call.= F)
  }
  return(plotholder)
}
PDFOutFun <- function(Plots,Colours=NA,Gradient.Interval=10,Legend=T,Scale.Interval=NA,Title=NA,File.Name="Output") {
  mycol <- colorRampPalette(Colours)
  top <- max(unlist(Plots)[!is.na(unlist(Plots))])
  ID <- seq(0,top,Gradient.Interval)
  if(is.na(Colours)){
    ramp <- data.frame("ID"=ID, "col"= viridis(length(ID),direction = -1))
  }else{
    if(ID[length(ID)]<top){ID <-c(ID,(ID[length(ID)]+Gradient.Interval))}
    ramp <- data.frame("ID"=ID, "col"= mycol(length(ID))[1:length(ID)], stringsAsFactors=F)
  }
  for(n in 1:length(Plots)){
    pdf(file=paste(File.Name,"_",n,".pdf",sep=""))
    data <- Plots[[n]]
    par(mar=c(0,0,0,0))
    plot(NULL, ylim=c(0, (nrow(data)+(nrow(data)/15))), xlim=c(0, ncol(data)), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
    for (m in 1:ncol(data)){
      for(i in 1:nrow(data)){
        if (is.na(data[i,m])){
          NULL
        } else {
          wichColor <- max(which(data[i,m]>ramp$ID))
          colour <- paste(ramp$col[wichColor])
          rect(m-0.5, i-0.5, m+0.5, i+0.5, col=colour, border=colour)
        }
      }
    }
    if(Legend){
      if(ncol(data)/length(ID) < 1){
        S <- ncol(data)/length(ID)
      } else if (length(ID < (0.25*ncol(data)))){
        S = (0.25*ncol(data))/length(ID)
      } else {
        S <- 1
      }       
      for(i in 1:length(ramp$col)){
        rect(0.4+i*S, (nrow(data)+(nrow(data)/45)), (0.4+S)+i*S, (nrow(data)+(nrow(data)*2/45)), col=paste(ramp$col[i]), border=paste(ramp$col[i]))
      }
      if(is.na(Scale.Interval)){
        scale <-round(seq(0,ID[length(ID)],length.out = 5),0)
      } else {
        scale <-round(seq(0,ID[length(ID)],Scale.Interval),0)
      }
      
      lines <- seq((0.4+S),((0.4+S)+length(ramp$col)*S), length.out = length(scale))
      k <- 1
      for (i in lines){
        text(i, nrow(data)+(nrow(data)*2/45), labels=scale[k], srt = 90, adj=0, cex=(0.75))
        lines(x = c(i,i), y = c(nrow(data)+(nrow(data)/45), nrow(data)+(nrow(data)*2/45)))
        k <- k+1
      }
      if(!is.na(Title)){
        title <- paste(Title,", ",n,"/",length(Plots),sep="")
      }
      text(x = (ncol(data)/2), y = (nrow(data)+(nrow(data)*3/45)), labels=c(title), adj=c(0.5,0), cex=((0.75)))
    }
    dev.off()
  }
}
     
            
            
            
