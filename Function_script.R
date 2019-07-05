### here is the file path for coor files, run at startup to save time,
Field_Coors <- "C:\\Users\\Julia\\Documents\\AGM Field Map\\Currently Using\\Current\\Field Data\\AGAW\\AGAW_coords_field.csv"
Trap_Coors <- "C:\\Users\\Julia\\Documents\\AGM Field Map\\Currently Using\\Current\\Field Data\\AGAW\\AGAW_coords_trap.csv"
CSV <- "C:\\Users\\Julia\\Documents\\AGM Field Map\\Currently Using\\Current\\Field Data\\AGAW\\AGAW_All.csv"
TSV <- "C:\\Users\\Julia\\Downloads\\All-4.tsv" 
library(stringr)
library(data.table)
File <-  as.data.frame(fread(TSV))

#######################################################################################
#
### anything surronded in # (like this line) is personal notes and ony included in this coding script not in offical version 7 ###
#====================================================================================== 
## Function 1 - Setup
#======================================================================================
#
# The first step no matter what, both boarder and trap coordinates should be uploaded in decimal degrees as either a data frame or matrix (Lat, Long), 
# if no value is given for number of weeks will default as one. The order the trap coordinates are inputed should correspond to the order the data is 
# in regarding traps. Ie. The first pair of coordinates in the list of trap coordiates should corespond to the first trap whos results are reported in 
# the data sheet.If Slam trap is not set to True will assume deafutl malasis trap was used.  

Setup <- function(Boarder_Coordinates, Trap_Coordinates,  Number_of_Sampling_Events = NULL, Slam_trap = FALSE){
### Produces a warning if BC can not be read otherwises turns them into a vector ###
  if(!is.data.frame(Boarder_Coordinates) && !is.matrix(Boarder_Coordinates)){
    stop("Boarder Coordinates need to be stored as either a dataframe or a matrix.", call.= F)
  } else {
    BC <- c(Boarder_Coordinates[1,1],Boarder_Coordinates[1,2]) 
    for (n in 2:nrow(Boarder_Coordinates)) {
      BC <- c(BC,Boarder_Coordinates[n,1],Boarder_Coordinates[n,2])
    }
  }
### Ditto for TC ###  
  if(!is.data.frame(Trap_Coordinates) && !is.matrix(Trap_Coordinates)){
    stop("Trap Coordinates need to be stored as either a dataframe or a matrix.", call. = F)
  } else {
    TC <- c(Trap_Coordinates[1,1],Trap_Coordinates[1,2]) 
    for (n in 2:nrow(Trap_Coordinates)){
            TC <- c(TC,Trap_Coordinates[n,1],Trap_Coordinates[n,2]) 
    }
  }
### Calculate Number of traps ###  
  Number_of_Traps <- length(TC)/2
### if number of sampling events is set as one or left blank then 1 is assigned to the variable ###  
  if (is.null(Number_of_Sampling_Events)||Number_of_Sampling_Events <= 0){
    Number_of_Sampling_Events <- 1
  }
  storage <- list(BC, TC, Number_of_Sampling_Events, Number_of_Traps, Slam_trap)
  return(storage)
}

# Save the results from the function "setup" to an object
setup <- Setup(Boarder_Coordinates = read.csv(Field_Coors), Trap_Coordinates = read.csv(Trap_Coors), Number_of_Sampling_Events = 9, Slam_trap = T)

#====================================================================================== 
## Function 2 - Formatting from mBrave
#======================================================================================
#
# This function is for use speficlly with TSV outputs produced by mBRrave. It returns a data frame where each column contains all 
# the speciemen found in a single trap. Left to right the traps should be in the order their coordients were uploaded in during set up.
# If it is a Slam trap and therefor multiple bottles associated with a singular trap. The colunms should be in the order of, north bottle,
# east bottle, south bottle, west bottle, then repeat for the next trap. If there are multiple sampling events, repeat this layout 
# chronolocially for each event. 
#
# Set ID as a character vector that represnts the repeated portion of the labeling scheme for sample location. This string should immedeatly be
# followed by trap #,-,event #,direction (if applicable), (ex. if ID = AGAS, the location tag associated with a sample may be "AGAS4-05E") 
# Suffix symbol identifys the text symbol (ie. ',' '_' ')' '#' etc.) that appears directly after the location tag, seperating it from 
# additional compenents of the runName
# Ex.If a file contains the follow as runNames "L#17AGAS1-14W)CCDB-S5-0099)CBGMB-00047", "L#17AGAS1-14S)CCDB-S5-0099)CBGMB-00047", 
# "L#17AGAS4-05E)CCDB-S5-0077)CBGMB-00034" ID would be set as "AGAS" and Suffix Symbol as ")". 

Formatting_mBrave <- function(File, ID, Suffix_Symbol, Slam_trap = F) {
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
    if(n == round(length(split)*0.25,0)){show("25% data pulled.")}
    if(n == round(length(split)*0.5,0)){show("50% data pulled.")}
    if(n == round(length(split)*0.75,0)){show("75% data pulled.")}
    if(n == length(split)){show("100% data pulled.")}
    if(Slam_trap) {
      if (length(str_sub(split[[n]], str_locate_all(split[[n]], "[A-Z]+")[[1]][,1], str_locate_all(split[[n]], "[A-Z]+")[[1]][,2])) == 1){
### this is the case where it is named E or East, either way only first capital letter gets used so if East ast will be dropped anyways ###
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
### this is the case where it is named EAST ###
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
### this is when e is used ###
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
### this is when east is used ###{
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
### going to build seperated into data frame "ready" might be able to remove step of creating seperated eventually ###
  event_lables <- as.numeric(sort(unique(seperated[,2])))
  trap_lables <- as.numeric(sort(unique(seperated[,1])))
  ready <- data.frame()
  if(Slam_trap){
    ready <- as.data.frame(matrix(0,1,length(event_lables)*length(trap_lables)*4))
    for(n in 1:length(split)){
      col <- (which(as.numeric(seperated[n,2]) == event_lables)-1)*length(trap_lables)*4 + (which(as.numeric(seperated[n,1]) == trap_lables)-1)*4 + seperated[n,3]
      ready[colSums(!is.na(ready))[col] + 1,col] <- seperated[n,4] 
      if(n == 1){ready <- ready[-1,]}
      if(n == round(length(split)*0.25,0)){show("25% formatted.")}
      if(n == round(length(split)*0.5,0)){show("50% formatted.")}
      if(n == round(length(split)*0.75,0)){show("75% formatted.")}
      if(n == length(split)){show("100% formatted")}
    }      
  } else {
    ready <- as.data.frame(matrix(0,1,length(event_lables)*length(trap_lables)))
    for(n in 1:length(split)){
      col <- (which(as.numeric(seperated[n,2]) == event_lables)-1)*length(trap_lables) + which(as.numeric(seperated[n,1]) == trap_lables)
      ready[colSums(!is.na(ready))[col] + 1,col] <- seperated[n,3] 
      if(n == 1){ready <- ready[-1,]}
      if(n == round(length(split)*0.25,0)){show("25% formatted.")}
      if(n == round(length(split)*0.5,0)){show("50% formatted.")}
      if(n == round(length(split)*0.75,0)){show("75% formatted.")}
      if(n == length(split)){show("100% formatted")}
    }
  }
return(ready)  
}

### takes about 16 minutes to run ~84000 rows. (Runs at a ~ speed of 5250/min or 87/sec) ###
old <- Sys.time()
# A data frame has been created that has the correct layout to run thrught the rest of the code
formatted <- Formatting_mBrave(File=File,ID = "AGAS", Suffix_Symbol = ")", Slam_trap = T)
show(Sys.time() - old)

#====================================================================================== 
## Function 3 - Data
#======================================================================================
#
# Once a data frame (or matrix) has been created that fits the layout requirements outlined 
# in function 2 (sort by samplign event, then trap, then direction).
### Unsure what the final format will be. Will for sure be a single file. Most likley a .tsv. However for now I will use a .csv file. ###
### Eventually this function may be able to be conmbined with function 1. ###


Data <- function(File, Setup) {
  if (!is.data.frame(File)){
    stop("Sampling data must be stored as a dataframe.", call. = F)
  }
  if (Setup[[5]] && ncol(File) != (Setup[[4]]*4*Setup[[3]])){
    stop("Data provided for file does not match the parameters used in setup", calli.=F)
  } else if (!(Setup[[5]]) && ncol(File) != (Setup[[4]]*Setup[[3]])) {
    stop("Data provided for file does not match the parameters used in setup", calli.=F)
  }  
  trap <- list(c(1))
  for(n in 1:ncol(File)){
    trap[[(n)]] <- (File[,n])
    trap[[(n)]] <- as.character(trap[[(n)]])
    trap[[(n)]] <- trap[[(n)]][!(is.na(trap[[(n)]]))]
    trap[[(n)]] <- trap[[(n)]][trap[[(n)]]!=""]
  }
  return(trap)
}

### next time copy data function into version 7 and clean up nameing scheme of version 7 (ie when object has caps or not)
data <- Data(File = read.csv(CSV), Setup = setup)

#====================================================================================== 
## Function 4 - Distance Conversions
#======================================================================================
#
# The function will be used to convert sets of decimal degrees coordinates into a distance vector in future calculations.
# The distance vectors will be in metres unless km is set to true, then switches to kilometres. 

Distance <- function(y1,y2,x1,x2, km = F){
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
  if (!(km)){
    return(varR*equ2)
  } else {
    return(varR*equ2/1000)
  }
}

# There is no object that intial needs to be created. The function must be read in so that it can be used in additional functions.

#====================================================================================== 
## Function 5 - Conversion
#======================================================================================
#
# Convert coordinates into (X,Y) points that can be plotted onto a 2D graph therefor assigning 0,0 as the south west corner.

Conversion <- function(Setup, Poles = F){
### Seperating the corners on the plot ###
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
### Pulling out crtical points form these corners ###
### Need to define the area we will lay the array over to perform calculations. Will use terms "top, bottom, left, right". ###
### Almost always these will be synonymous with "north, south, west, east" respectively, but not always so will keep it simple with 2D terms. ###    
  if(Poles) {
   if(ycorners[1] > 0){
### Calculates length of x axis (starts at LMX goes to RMX along the line of BMY) ###
     LMX <- 180
     BMY <- min(ycorners)
     DX <- round(Distance(BMY,BMY,180,0),0)
### Calculates length of y axis (starts at BMY goes to TMY along the line of LMX) ###     
     DY <- DX
   } else {
     LMX <- 180
     BMY <- max(ycorners)     
     DX <- round(Distance(BMY,BMY,180,0),0)
     DY <- DX
   } 
### Using this solution to solve for the poles is computationally intensive but because the liklihood of this senerio happening is low. Not worth 
### finding a better solution. ###
  } else {
### Calculates length of x axis (starts at LMX goes to RMX along the line of BMY) ###
    BMY <- min(ycorners)
    LMX <- min(xcorners)
    DX <- round(Distance(min(ycorners),min(ycorners),min(xcorners),max(xcorners)),0)
### Calculates length of y axis (starts at BMY goes to TMY along the line of LMX) ###
    DY <- round(Distance(min(ycorners),max(ycorners),min(xcorners),min(xcorners)),0)
  }
### Now need to convert other corners into distances relative to the bottom left corner of the newly defined plot  
  plot_outline <- c()
  for (k in 1:length(xcorners)){
    plot_outline <- c(plot_outline, round(Distance(BMY,BMY,LMX,xcorners[k]),0),round(Distance(BMY,ycorners[k],LMX,LMX),0))
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
    trap_locations <- c(trap_locations, round(Distance(BMY,BMY,LMX,xcorners[k]),0),round(Distance(BMY,ycorners[k],LMX,LMX),0))
  }
  return(list(DX,DY,plot_outline,trap_locations))
}


conversion <- Conversion(Setup = setup)

#====================================================================================== 
## Function 6 - Array Placment 
#======================================================================================
#
# Takes the the converted quarentents and uses them to build an array as well as being to populate it with known abundance valueas. 

Array_placement <- function(Setup, Outlines, Data){
  all_matrix <- list()
  traps <- list()
  if (Setup[[5]]){
    ### If slam traps were used ###
    for(m in 1:Setup[[3]]){
      all_matrix[[m]] <- matrix(0,Outlines[[2]],Outlines[[1]])
      for (k in 1:Setup[[4]]){
        all_matrix[[m]][Outlines[[4]][(2*k)]+1,Outlines[[4]][(2*k-1)]] <- length(Data[[((m-1)*4*Setup[[4]])+k*4-3]])
        all_matrix[[m]][Outlines[[4]][(2*k)],Outlines[[4]][(2*k-1)]+1] <- length(Data[[((m-1)*4*Setup[[4]])+k*4-2]])
        all_matrix[[m]][Outlines[[4]][(2*k)]-1,Outlines[[4]][(2*k-1)]] <- length(Data[[((m-1)*4*Setup[[4]])+k*4-1]])
        all_matrix[[m]][Outlines[[4]][(2*k)],Outlines[[4]][(2*k-1)]-1] <- length(Data[[((m-1)*4*Setup[[4]])+k*4]])
        if(m == 1){
          ### since this is a vector not a matrix coordinate, coordinates have been recorded as x,y (col,row) just for simplicity ###        
          traps[[k]]<- list(Outlines[[4]][(2*k-1)],Outlines[[4]][(2*k)],c(1:4)+(4*(k-1)))
        }
      }
    }
  } else {
    ### NOt a slam trap ###  
    for(m in 1:Setup[[3]]){
      all_matrix[[m]] <- matrix(0,Outlines[[2]],Outlines[[1]])
      for (k in 1:Setup[[4]]){
        all_matrix[[m]][Outlines[[4]][(2*k)]+1,Outlines[[4]][(2*k-1)]] <- length(Data[[((m-1)*Setup[[4]])+k]])
        all_matrix[[m]][Outlines[[4]][(2*k)],Outlines[[4]][(2*k-1)]+1] <- length(Data[[((m-1)*Setup[[4]])+k]])
        all_matrix[[m]][Outlines[[4]][(2*k)]-1,Outlines[[4]][(2*k-1)]] <- length(Data[[((m-1)*Setup[[4]])+k]])
        all_matrix[[m]][Outlines[[4]][(2*k)],Outlines[[4]][(2*k-1)]-1] <- length(Data[[((m-1)*Setup[[4]])+k]])
        if(m == 1){
          ### since this is a vector not a matrix coordinate, coordinates have been recorded as x,y (col,row) just for simplicity ###        
          traps[[k]]<- list(Outlines[[4]][(2*k-1)],Outlines[[4]][(2*k)],c(1:4)+(4*(k-1)))
        }
      }
    }
  }
  sort_y <- c()  
  for(T in 1:Setup[[4]]){
    sort_y[T] <- traps[[T]][[2]]  
  }
### top to bottom (NtoS) ###  
  TtoB <- sort(sort_y,decreasing =TRUE)
  TtoBmatrix <- matrix(NA, Setup[[4]]+1, 5)
  for (r in 1:5){
    for (k in 1:5){
      if (TtoB[r] == traps[[k]][[2]]){
        TtoBmatrix[r,] <- c(traps[[k]][[3]], NA)
      }
    }
  }
  sort_x <- c()  
  for(T in 1:Setup[[4]]){
    sort_x[T] <- traps[[T]][[1]]  
  }
### right to left (east to west) ###  
  RtoL <- sort(sort_x,decreasing =TRUE)  
  RtoLmatrix <- matrix(NA, Setup[[4]]+1, 5)
  for (r in 1:Setup[[4]]){
    for (k in 1:Setup[[4]]){
      if (RtoL[r] == traps[[k]][[1]]){
        RtoLmatrix[r,] <- c(traps[[k]][[3]], NA)
      }
    }
  }
  return(list(all_matrix,traps,TtoBmatrix,RtoLmatrix))
}


# The object returned from this function is a two part list. For simplicity it is recommened to segrate them into their own objects. As done below
plots <- Array_placement(Setup = setup, Outlines = conversion, Data = data)[[1]]
### this is a nested list,[[trap #]],[[x coor, y coor, assigned numbers]] it is a lis of lits containing which arbitrauilay labels the bottles associated with each trap ###
traps <- Array_placement(Setup = setup, Outlines = conversion, Data = data)[[2]]
# I am going to name the object containing my direction matrices using north, south, east and west, not top, bottom, right and left because
# conceptually easier, but note techincally is still refereing to positions in an array which does not have geographical directions  
NtoSmatrix <- Array_placement(Setup = setup, Outlines = conversion, Data = data)[[3]] 
EtoWmatrix <- Array_placement(Setup = setup, Outlines = conversion, Data = data)[[4]] 



#====================================================================================== 
## Function >7 Newly created functions
#======================================================================================
#
# Functions I have made to make life easier but havent fully noted yet
Unique <- function(Data, Formatted){
  string <- c(NA)
  for(n in 1:length(Data)){
    string <- c(string,Data[[n]])
  }
  string <- string[-1]
  length <- c()
  for(n in 1:ncol(Formatted)){
    length[n] <- length(Data[[n]])
  }
  if(sum(length) == length(string)) {
    show("Match")
  }
  scanned <- c()
  for(n in 1:length(string)){
    if(str_detect(string[n],"[:alpha:]")){
      scanned <- c(scanned,string[n])
    }
  }
  return(unique(scanned))
}

DataFrame <- function(Data,Formatted,Unique,Setup,Conversion){
  Found <- c()
  for (d in 1:ncol(Formatted)){
    for(n in 1:length(Unique)){
      ifelse(length(grep(Unique[n],Data[[d]]))>=1,Found <- c(Found,1),Found <- c(Found,0))
    }
    if(d == round(ncol(Formatted)*0.25,0)){show("25% formatted.")}
    if(d == round(ncol(Formatted)*0.5,0)){show("50% formatted.")}
    if(d == round(ncol(Formatted)*0.75,0)){show("75% formatted.")}
    if(d == ncol(Formatted)){show("100% formatted")}
  }
  ID <- rep(Unique,ncol(Formatted))
  if(Setup[[5]]){
    Bottle <- rep(rep(c("N","E","S","W"),each = length(Unique)),times=(ncol(Formatted)/4))
    Event <- rep(c(1:Setup[[3]]),each = ((length(Unique)*Setup[[4]]*4)))
    LongX <- rep(rep(Conversion[[4]][seq(1,length(Conversion[[4]])-1,by=2)], each=length(Unique)*4),times=Setup[[3]])
    LatY <- rep(rep(Conversion[[4]][seq(2,length(Conversion[[4]]),by=2)], each=length(Unique)*4),times=Setup[[3]])
    Trap <- rep(rep(c(1:Setup[[4]]), each=length(Unique)*4),times=Setup[[3]])
    return(data.frame("ID" = c(ID), "LongX" = c(LongX), "LatY" = c(LatY), "Event" = c(Event), "Trap" = c(Trap), "Bottle" = Bottle, "Found" = c(Found)))
  } else {
    Event <- rep(c(1:Setup[[3]]),each = ((length(Unique)*Setup[[4]])))
    LongX <- rep(Conversion[[4]][seq(1,length(Conversion[[4]])-1,by=2)],times=Setup[[3]])
    LatY <- rep(Conversion[[4]][seq(2,length(Conversion[[4]]),by=2)],times=Setup[[3]])
    Trap <- rep(c(1:Setup[[4]]),times=Setup[[3]]) 
    return(data.frame("ID" = c(ID), "LongX" = c(LongX), "LatY" = c(LatY), "Event" = c(Event), "Trap" = c(Trap), "Found" = c(Found)))
  }
}

Repeat <- function(Unique,Dataframe,Setup,Formatted){
  RepMat <- matrix(0,Setup[[3]],length(Unique))
  for (E in 1:Setup[[3]]){
    if(Setup[[5]]){
      for(U in 1:length(Unique)){
        for(t in 1:5){
          sum <- sum(Dataframe[seq(((E-1)*(nrow(Dataframe)/Setup[[3]]))+U,length.out = (4*Setup[[4]]),by=length(Unique)),][seq(to=(t*4),length.out = 4),]$Found)
          if(sum>=1){
            RepMat[E,U] <- RepMat[E,U]+1
          }
        }
      }
    } else {
      for(U in 1:length(Unique)){
        sum <- sum(DataFrame[seq(((E-1)*(nrow(DataFrame)/Setup[[3]]))+U,length.out = 5,by=length(Unique)),]$Found)
        if(sum>=1){
          RepMat[E,U] <- sum
        }
      }
    }
    show(E)
  }
  Repeat <- c()
  for(n in 1:Setup[[3]]){
    Repeat <- c(Repeat, rep(RepMat[n,],times=(ncol(Formatted)/Setup[[3]])))
  }
  List <- list(Repeat,RepMat)
  return(List)
}

Outline <- function(Conversion){
  county <- c(seq(2,length(Conversion[[3]]),by=2),2)
  countx <- c(seq(1,length(Conversion[[3]]),by=2),1)
  ally <- c()
  allx <- c()
  for(k in 1:(length(county)-1)){
    ally <- c(ally,Conversion[[3]][county[k]]:Conversion[[3]][county[k+1]])
    allx <- c(allx,seq(Conversion[[3]][countx[k]],Conversion[[3]][countx[k+1]],length.out = length(Conversion[[3]][county[k]]:Conversion[[3]][county[k+1]])))
  }
  allx <- round(allx,0)
  building <- list()
  building[[max(ally)]] <- NA
  for(n in 1:length(ally)){
    building[[ally[n]]] <- c(building[[ally[n]]],allx[n])
  }
  building[[max(ally)]] <- building[[max(ally)]][-1]
  for(n in 1:length(building)){
    if(sum(duplicated(building[[n]]))>=1){
      building[[n]] <- building[[n]][-which(duplicated(building[[n]]))]
    }
    building[[n]] <- sort(building[[n]])
  }
  rows <- list()
  rows[[length(building)+1]] <- NA
  for(n in 1:length(building)){
    if(length(building[[n]])==1){
      rows[[n]] <- building[[n]]
    } else if (length(building[[n]])==2){
      rows[[n]] <- seq(building[[n]][1],building[[n]][2],by = 1)
    } else {
      for(c in 1:(length(building[[n]])/2)){
        rows[[n]] <- c(rows[[n]],
                       seq(building[[n]][seq(1,length(building[[n]])-1,by=2)[c]],
                           building[[n]][seq(2,length(building[[n]]),by=2)[c]],
                           by=1))
      }
    }
  }
  rows[[length(building)+1]] <- NULL
  return(rows)
}

Plot <- function(Outline, Setup, Unique, Repeat, Conversion, Model){
  start <- Sys.time()
  plotholder <- list()
  for(E in 1:Setup[[3]]){
    matrix <- matrix(NA,nrow=Conversion[[2]],ncol=Conversion[[1]])
    for(Y in 1:length(Outline)){
      for(X in 1:length(Outline[[Y]])){
        newdata <- data.frame("LongX" = rep(Outline[[Y]][X],time=length(Unique)),
                              "LatY" = rep(Y,time=length(Unique)),
                              "Event" = rep(E,time=length(Unique)),
                              "Repeat" = Repeat[[2]][E,]) 
        matrix[Y,Outline[[Y]][X]] <- sum(ifelse(predict(Model,newdata=newdata)>=0.5,1,0))
      }
      if(Y==round(length(Outline)*0.25,0)){show("25%")}
      if(Y==round(length(Outline)*0.25,0)){show(Sys.time())}
      if(Y==round(length(Outline)*0.5,0)){show("50%")}
      if(Y==round(length(Outline)*0.5,0)){show(Sys.time())}
      if(Y==round(length(Outline)*0.75,0)){show("75%")}
      if(Y==round(length(Outline)*0.75,0)){show(Sys.time())}
    }
    plotholder[[E]] <- matrix
    show(E)
  }
  return(plotholder)
}
