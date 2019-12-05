{
# for boarder coor start by entering south most coordinate then go clockwise around
Boarder_Coordinates <- c(43.516249, -80.174524, 43.523066, -80.183899, 43.525190, -80.180960, 43.524411, -80.179947, 43.524015, -80.180215, 43.523403, -80.179373, 43.524001, -80.178214, 43.518812, -80.171054)
Trap_Coordinates <- c(43.52436, -80.1822, 43.52155, -80.17493, 43.51786, -80.17234, 43.51939, -80.17886, 43.52046, -80.17691)
Number_of_Traps = 5
Number_of_Weeks = 9
Week_names <- c(2,5,7,8,9,10,11,14,15)
plot_name <- ""
field_name <- "AGAW"
col=c("red4", "red", "white") 
#### now into codingsection
#x coordinate represented x by longitude and y by latitude  
#### convert coordinates from lat and long into y and x
k <- 2
xcorn <- c()
for (x in 1:(length(Boarder_Coordinates)/2)){
  xcorn[x] <- Boarder_Coordinates[k]
  k <- k+2
}
k <- 1
ycorn <- c()
for (y in 1:(length(Boarder_Coordinates)/2)){
  ycorn[y] <- Boarder_Coordinates[k]
  k <- k+2
}
#### Identify critical points 
SMX <- xcorn[1] #south most x
SMY <- ycorn[1] #south most y
WMX <- xcorn[which(xcorn == min(xcorn))] #west most x
WMY <- ycorn[which(xcorn == min(xcorn))] #west most y
origin <- c(WMX,SMY) #this is now long lat but makes more sense for matrixing
### change lat log to intergals 
distance <- function(y1,y2,x1,x2){
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
  return(varR*equ2)
}

DX <- round(distance(SMY,SMY,WMX,max(xcorn)),0)
DY <- round(distance(SMY,max(ycorn),WMX,WMX),0)

convertedX <- c()
convertedY <- c()
for (k in 1:length(xcorn)){
  convertedX[k] <- round(distance(SMY,SMY,WMX,xcorn[k]),0)
  convertedY[k] <- round(distance(SMY,ycorn[k],WMX,WMX),0)
}
Corners <- c()
for (k in 1:length(xcorn)){
  Corners <- c(Corners, convertedX[k],convertedY[k])
}
### now working with trap coordinates same process
k <- 2
xcoor <- c()
for (x in 1:(length(Trap_Coordinates)/2)){
  xcoor[x] <- Trap_Coordinates[k]
  k <- k+2
}
k <- 1
ycoor <- c()
for (y in 1:(length(Trap_Coordinates)/2)){
  ycoor[y] <- Trap_Coordinates[k]
  k <- k+2
}
convertedX <- c()
convertedY <- c()
for (k in 1:length(xcoor)){
  convertedX[k] <- round(distance(SMY,SMY,WMX,xcoor[k]),0)
  convertedY[k] <- round(distance(SMY,ycoor[k],WMX,WMX),0)
}
coor <- c()
for (k in 1:length(xcoor)){
  coor <- c(coor, convertedX[k],convertedY[k])
}
### now two location vectors, corners outlines field coor is for traps
#### this is where.csv are imported... will auto promt when files run as a whole
max <- c()
min <- c()
complete <- list()
Input <- list()
for (W in 1:Number_of_Weeks){
  file <-read.csv(file.choose())
  Input[[W]] <- file
}
### processign data off of cvs
for (W in 1:Number_of_Weeks){
  Data <- as.data.frame(Input[[W]])
  C <- 1
  filler <- c(1)
  bottles <- list(filler)
### Assign BINs to a list in the order they are on the input file (this is N1, E1, S1, W1...NT, ET, ST, WT)
  while (C <=(4*Number_of_Traps)){
    bottles[[(C)]] <- (Data[,C])
    creator <- (Data[,C])
    creator <- as.character(creator)
    creator <- creator[creator!=""]
    bottles[[(C)]] <- creator
    C <- C+1
  }
### creates blank matrix to begin adding info to  
  matrix <- matrix(NA,DY,DX)
### Placing Traps on matrix , 4 squares per trap, assigning total number of BINs identified per square 
  for (k in 1:Number_of_Traps){
    matrix[coor[(2*k)]+1,coor[(2*k-1)]] <- length(bottles[[k*4-3]])
    matrix[coor[(2*k)],coor[(2*k-1)]+1] <- length(bottles[[k*4-2]])
    matrix[coor[(2*k)]-1,coor[(2*k-1)]] <- length(bottles[[k*4-1]])
    matrix[coor[(2*k)],coor[(2*k-1)]-1] <- length(bottles[[k*4]])
  }
### creats the list Traps which contains location of middle of each trap (x,y) there is no number there as well as 4 numbers saying which bottles are associated with that trap
    Traps <- list(filler)
  for (k in 1:Number_of_Traps){
    creator <- list(c(coor[2*k-1],coor[2*k]),c(k*4-3,k*4-2,k*4-1,k*4))
    Traps[[(k)]] <- creator  
  } 
##### this sort statement may need to be added in other places
### isolates the y matrix coordinate for traps
  sort_y <- c(NA)  
  for(T in 1:Number_of_Traps){
    sort_y <- c(sort_y,Traps[[T]][[1]][2])  
  }
  sort <- sort_y[-1]  
### Creates a vector with the matrix y coordinates of the traps where north most trap is at the top, south most bottom
  NtoS <- sort(sort,decreasing =TRUE)
### Creates a matrix where each row is a vector of 4 bottles associated with a trap   
  NtoSmatrix <- matrix(NA, Number_of_Traps+1, 5)
  for (r in 1:Number_of_Traps){
    for (k in 1:Number_of_Traps){
      if (NtoS[r] == Traps[[k]][[1]][2]){
        NtoSmatrix[r,] <- c(Traps[[k]][[2]], NA)
      }
    }
  }
### isolates the x matrix coordinate for traps
  sort_x <- c(NA)  
  for(T in 1:Number_of_Traps){
    sort_x <- c(sort_x,Traps[[T]][[1]][1])  
  }
  sort <- sort_x[-1]  
### Creates a vector with the matrix x coordinates of the traps where east most trap is at the top, west most bottom 
  EtoW <- sort(sort,decreasing =TRUE)  
  EtoWmatrix <- matrix(NA, Number_of_Traps+1, 5)
  for (r in 1:Number_of_Traps){
    for (k in 1:Number_of_Traps){
      if (EtoW[r] == Traps[[k]][[1]][1]){
        EtoWmatrix[r,] <- c(Traps[[k]][[2]], NA)
      }
    }
  }
#### vectors contain which bottle number is associate with which direction  
  north <- c(seq(1,Number_of_Traps*4,by=4))
  east <- c(seq(2,Number_of_Traps*4,by=4))
  south <- c(seq(3,Number_of_Traps*4,by=4))
  west <- c(seq(4,Number_of_Traps*4,by=4))
  
###### Here begings calculations
## start with working on east bottles 
## a matrix that records which bottle associations have already been done, so not have to repeat when switching directions
  keep_track <- matrix(NA,1,2)
  ### creating a requirred function
  eastlocator <- function(n){
    ### the east bottle column of the east west matrix is scanned for the location of the nth bottle in the east vector  
    for (t in 1:Number_of_Traps){   
      if (EtoWmatrix[t,2] == east[n]){
        ### the row number for that bottle is assigned as its E/W position (the higher the row # the further west the bottle)      
        EtoWposition <- t
      }
    }
    ### the east bottle column of the north south matrix is scanned for the location of the nth bottle in the east vector  
    for (t in 1:Number_of_Traps) {      
      if (NtoSmatrix[t,2] == east[n]){
        ### the row number for that bottle is assigned as its N/S position (the higher the row # the further south the bottle)      
        NtoSposition <- t
      }
    }
    ### now that the position of the nth east bottle is know, must identify the closest bottle north east from it
    using <- c(NA)
    i <- 1
    ### checks if there is even is a trap north    
    while (NtoSposition-1 >= 1){
      ### first candidate nort of east[n]    
      value <- NtoSmatrix[NtoSposition-i,4] #first one north of it
      ### the location of the trap 1 north of east[n] on the east west matrix    
      locator <- which(EtoWmatrix==value, arr.in=TRUE) 
      repeat{
        ### indicates there is no trap that meets the criteria, it may not be the northrn most trap but all those are west of it      
        if (NtoSposition-i < 1){ 
          using[1] <- NA
          using[2] <- NA
          break
        } 
        ###checks if  candidate bottle is east of east[n], if true than its position on both direction matrixes are recorded
        else if (locator[1,1] < EtoWposition){ 
          using[1] <- value
          using[2] <- EtoWmatrix[locator[1,1],3]
          break 
        } 
        else { 
          ###cloest northern trap didnt work so try second closest northern trap
          i <- i + 1 # try the next trap north 
          value <- NtoSmatrix[NtoSposition-i,4] #the new investigated value
          locator <- which(EtoWmatrix==value, arr.in=TRUE) #new location
        }
      }
      break
    }
    ### this bottle is on the further north trap so no point looking for something north east of it       
    if (NtoSposition-i < 1){ 
      using[1] <- NA
      using[2] <- NA
    } 
    ### Now want to locate the closest bottle south east of east[n]  
    i <- 1
    ### checks if there is even is a trap south  
    while (NtoSposition+i <= Number_of_Traps) {
      value <- NtoSmatrix[NtoSposition+i,4] # first one south of it
      locator <- which(EtoWmatrix==value, arr.in=TRUE) #how far west this south one is located 
      repeat{
        ### indicates there is no trap that meets the criteria, it may not be the southern most trap but all those are west of it      
        if (NtoSposition+i > Number_of_Traps){ 
          using[3] <- NA
          using[4] <- NA
          break
        } 
        ###checks if  candidate bottle is east of east[n], if true than its position on both direction matrixes are recorded
        else if (locator[1,1] < EtoWposition){
          using[3] <- value
          using[4] <- EtoWmatrix[locator[1,1],1]
          break 
        } 
        ### cloest southern trap didnt work so try second closest southern trap      
        else { 
          i <- i + 1
          value <- NtoSmatrix[NtoSposition+i,4]
          locator <- which(EtoWmatrix==value, arr.in=TRUE)
        }
      }
      break
    }  
    ### this bottle is on the further south trap so no point looking for something south east of it   
    if (NtoSposition+i > Number_of_Traps){ 
      using[3] <- NA
      using[4] <- NA
    }    
###function complete return created vector, each 2 bottles are to be examine from each trap to coresponding to the side of the target trap facing the first trap, 
###(ie. if trap 2 is identified as beening the traget trap, using would be assigned 8 and 7 as they are the west and south bottle (respectively) which is the sides closest to orginal trap) )  
    return(using)
  }
  ### creating requirred function
  plottingeast_2 <- function(Bottles1,Bottles2){
    if (is.na(Bottles2)){
      ### there is no appropriate bottle so nothing to be done    
      return(matrix)
    } else {
      ### finding the matrix coordinates for Bottle1    
      for (r in 1:Number_of_Traps){
        if (Bottles1 %in% Traps[[r]][[2]]){
          y1 <- Traps [[r]][[1]][[2]]
          x1 <- Traps [[r]][[1]][[1]] + 1
        }
      }
      ### finding matrix cordinates for Bottle2    
      adjuster <- which(using==Bottles2, arr.in=TRUE)
      if(adjuster == 1){
        y <- 0
        x <- -1
      } else if (adjuster == 2){
        y <- -1
        x <- 0
      } else if (adjuster == 3){
        y <- 0
        x <- -1
      } else {
        y <- 1
        x <- 0
      } 
      for(g in 1:Number_of_Traps) { 
        if (Bottles2 %in% Traps[[g]][[2]]){
          y2 <- Traps [[g]][[1]][[2]]+y
          x2 <- Traps [[g]][[1]][[1]]+x
        }
      }
      ### using regression modle funtion on R to take advanatage that with only two points regression line and line of best fit are the same
      mod <- lm(c(y1,y2)~c(x1,x2))
      ### a vector outlining the column path that has to be taken across the matrix to connect the two bottles    
      vectorx <- seq((x1),(x2), by=((x2-x1)/max((y2-y1),(x2-x1)))) 
      vectory <- NA
      ### a vector outlining the row path that has to be taken across the matrix to connect the two bottles       
      for (x in vectorx){
        vectory <- c(vectory, mod$coefficients[2]*x + mod$coefficients[1])
      }
      vectorx <- round(vectorx,0)
      vectory <- round(vectory[-1],0)
      ### using regression modle funtion on R to take advanatage that with only two points regression line and line of best fit are the same
      ### thus determining the shortest distancebetween the two bottles
      mod_plot <- lm(c(matrix[y1,x1],matrix[y2,x2])~c(1,length(vectorx)))
      ### use this line of best fit to plot on matrix, uses the assumption that if a BIN is located in both traps it also will be observed in the space between them
      ### here the assumption that the BINS not found in both traps decay linearly is also used    
      for (k in 1:length(vectorx)){
        if (vectory[k]==y2 & vectorx[k]==x2) {
          NULL
        } else if(vectory[k]==y1 & vectorx[k]==x1 ) {
          NULL
        } else {
          if (is.na(matrix[vectory[k],vectorx[k]])){
            matrix[vectory[k],vectorx[k]] <- mod_plot$coefficients[1]+mod_plot$coefficients[2]*k 
          } else {
            matrix[vectory[k],vectorx[k]] <- (matrix[vectory[k],vectorx[k]] + (mod_plot$coefficients[1]+mod_plot$coefficients[2]*k))/2
          }
        }
      }
      return(matrix)
    }
  }   
  for (n in 1:length(east)){
### first two are row column location of north east trap, second two are row column for south east trap    
    using <- eastlocator(n)
    for (u in 1:length(using)){
### first caluclation is added to the matrix 
      matrix <- plottingeast_2(east[n],using[u]) 
### once two bottles have been connect, it is recored so as not to waste time repeating      
      keep_track <- rbind(keep_track, c(east[n],using[u]))
    }
  }
  
### Repeat process for west facing bottles
### same steps as for east, minor adjustments made  to reflect location change 
  westlocator <- function(n){
    for (t in 1:Number_of_Traps){         
      if (EtoWmatrix[t,4] == west[n]){
        EtoWposition <- t
      }
    }
    for (t in 1:Number_of_Traps) {      
      if (NtoSmatrix[t,4] == west[n]){
        NtoSposition <- t
      }
    }
   
    using <- c(NA)
    i <- 1
    while (NtoSposition-1 >= 1){
      value <- NtoSmatrix[NtoSposition-i,2] 
      locator <- which(EtoWmatrix==value, arr.in=TRUE) 
      repeat{
        if (NtoSposition-i < 1){ 
          using[1] <- NA
          using[2] <- NA
          break
        } else if (locator[1,1] > EtoWposition){ 
          using[1] <- value
          using[2] <- EtoWmatrix[locator[1,1],3]
          break 
        } else { 
          i <- i + 1 
          value <- NtoSmatrix[NtoSposition-i,2] 
          locator <- which(EtoWmatrix==value, arr.in=TRUE) 
        }
      }
      break
    }
    if(NtoSposition-i < 1){
      using[1] <- NA
      using[2] <- NA
    }
    i <- 1
    while (NtoSposition+i <= Number_of_Traps) {
      value <- NtoSmatrix[NtoSposition+i,2] 
      locator <- which(EtoWmatrix==value, arr.in=TRUE) 
      repeat{
        if (NtoSposition+i > Number_of_Traps){ k
          using[3] <- NA
          using[4] <- NA
          break
        } else if (locator[1,1] > EtoWposition){ 
          using[3] <- value
          using[4] <- EtoWmatrix[locator[1,1],1]
          break 
        } else { 
          i <- i + 1
          value <- NtoSmatrix[NtoSposition+i,2]
          locator <- which(EtoWmatrix==value, arr.in=TRUE)
        }
      }
      break
    }  
    if (NtoSposition+i > Number_of_Traps){ 
      using[3] <- NA
      using[4] <- NA
    }    
    return(using)
  }
### same steps as for east, minor adjustments made  to reflect location change 
  plottingwest_2 <- function(Bottles1,Bottles2){
    if (is.na(Bottles2)){
      return(matrix)
    } else {
      for (r in 1:Number_of_Traps){
        if (Bottles1 %in% Traps[[r]][[2]]){
          y1 <- Traps [[r]][[1]][[2]]
          x1 <- Traps [[r]][[1]][[1]] - 1
        }
      }
      adjuster <- which(using==Bottles2, arr.in=TRUE)
      if(adjuster == 1){
        y <- 0
        x <- 1
      } else if (adjuster == 2){
        y <- -1
        x <- 0
      } else if (adjuster == 3){
        y <- 0
        x <- 1
      } else {
        y <- 1
        x <- 0
      }  
      for(g in 1:Number_of_Traps) { 
        if (Bottles2 %in% Traps[[g]][[2]]){
          y2 <- Traps [[g]][[1]][[2]]+y
          x2 <- Traps [[g]][[1]][[1]]+x
        }
      }
      mod <- lm(c(y2,y1)~c(x2,x1))
      vectorx <- seq((x2),(x1), by=((x1-x2)/max((y1 -y2),(x1 - x2)))) #add back x2+1
      vectory <- NA
      for (x in vectorx){
        vectory <- c(vectory, mod$coefficients[2]*x + mod$coefficients[1])
      }
      vectory <- round(vectory[-1],0)
      vectorx <- round(vectorx,0)
      mod_plot <- lm(c(matrix[y2,x2],matrix[y1,x1])~c(1,length(vectorx)))
      for (k in 1:length(vectorx)){
        if (vectory[k]==y2 & vectorx[k]==x2) {
          NULL
        } else if(vectory[k]==y1 & vectorx[k]==x1 ) {
          NULL
        } else {
          if (is.na(matrix[vectory[k],vectorx[k]])){
            matrix[vectory[k],vectorx[k]] <- mod_plot$coefficients[1]+mod_plot$coefficients[2]*k
          } else {
            matrix[vectory[k],vectorx[k]] <- (matrix[vectory[k],vectorx[k]] + (mod_plot$coefficients[1]+mod_plot$coefficients[2]*k))/2
          }
        }
      }
      return(matrix)
    }
  } 
### same as done above for east
### however also checking to make sure this combintations hasnt been doe before, if so skips it
  for (n in 1:length(west)){
    using <- westlocator(n)
    locator <- which(keep_track==west[n], arr.in=TRUE)
    if(length(locator[,1]) < 1){
      for (u in 1:length(using)){
        matrix <- plottingwest_2(west[n],using[u])
        keep_track <- rbind(keep_track, c(west[n],using[u]))
      }
    } else {
      loc_vec <- c(locator[1:(length(locator[,1])),1])
      for (u in 1:length(using)){
        locator2 <- which(keep_track==using[u], arr.in=TRUE)
        if (length(locator2[,1]) < 1){
          matrix <- plottingwest_2(west[n],using[u]) 
          keep_track <- rbind(keep_track, c(west[n],using[u]))
        } else {
          loc_vec2 <- c(locator2[1:(length(locator2[,1])),1])
          if (sum(loc_vec %in% loc_vec2) >= 1){ #if this is true, means has already been plotted
            NULL
          } else {
            matrix <- plottingwest_2(west[n],using[u])
            keep_track <- rbind(keep_track, c(west[n],using[u]))
          }
        } 
      } 
    } 
  } 
  
  
### steps are repeated twice more for north and south traps
  northlocator <- function(n){
    for (t in 1:Number_of_Traps){         
      if (EtoWmatrix[t,1] == north[n]){
        EtoWposition <- t
      }
    }
    for (t in 1:Number_of_Traps) {      
      if (NtoSmatrix[t,1] == north[n]){
        NtoSposition <- t
      }
    }

    using <- c(NA)
    i <- 1
    while (EtoWposition-1 >= 1){
      value <- EtoWmatrix[EtoWposition-i,3] 
      locator <- which(NtoSmatrix==value, arr.in=TRUE) 
      repeat{
        if (EtoWposition-i < 1){ 
          using[1] <- NA
          using[2] <- NA
          break
        } else if (locator[1,1] < NtoSposition){ 
          using[1] <- value
          using[2] <- NtoSmatrix[locator[1,1],4]
          break 
        } else { 
          i <- i + 1 
          value <- EtoWmatrix[EtoWposition-i,3] 
          locator <- which(NtoSmatrix==value, arr.in=TRUE) 
        }
      }
      break
    }
    if(EtoWposition-i < 1){
      using[1] <- NA
      using[2] <- NA
    }
    i <- 1
    while (EtoWposition+i <= Number_of_Traps) {
      value <- EtoWmatrix[EtoWposition+i,3] 
      locator <- which(NtoSmatrix==value, arr.in=TRUE) 
      repeat{
        if (EtoWposition+i > Number_of_Traps){ 
          using[3] <- NA
          using[4] <- NA
          break
        } else if (locator[1,1] < NtoSposition){ 
          using[3] <- value
          using[4] <- NtoSmatrix[locator[1,1],2]
          break 
        } else { 
          i <- i + 1
          value <- EtoWmatrix[EtoWposition+i,3]
          locator <- which(NtoSmatrix==value, arr.in=TRUE)
        }
      }
      break
    }  
    if (EtoWposition+i > Number_of_Traps){ 
      using[3] <- NA
      using[4] <- NA
    }    
    return(using)
  }
  plottingnorth_2 <- function(Bottles1,Bottles2){
    if (is.na(Bottles2)){
      return(matrix)
    } else {
      for (r in 1:Number_of_Traps){
        if (Bottles1 %in% Traps[[r]][[2]]){
          y1 <- Traps [[r]][[1]][[2]] + 1
          x1 <- Traps [[r]][[1]][[1]] 
        }
      }
      adjuster <- which(using==Bottles2, arr.in=TRUE)
      if(adjuster == 1){
        y <- -1
        x <- 0
      } else if (adjuster == 2){
        y <- 0
        x <- -1
      } else if (adjuster == 3){
        y <- -1
        x <- 0
      } else {
        y <- 0
        x <- 1
      }  
      for(g in 1:Number_of_Traps) { 
        if (Bottles2 %in% Traps[[g]][[2]]){
          y2 <- Traps [[g]][[1]][[2]]+y
          x2 <- Traps [[g]][[1]][[1]]+x
        }
      }
      if (x1 < x2){
        mod <- lm(c(y1,y2)~c(x1,x2))
        vectorx <- seq((x1+1),(x2-1), by=((x2-x1)/max((y2-y1),(x2-x1))))
        vectory <- NA
        mod_plot <- lm(c(matrix[y1,x1],matrix[y2,x2])~c(1,length(vectorx)))
      } else {
        mod <- lm(c(y2,y1)~c(x2,x1))
        vectorx <- seq((x2+1),(x1-1), by=((x1-x2)/max((y1-y2),(x1-x2)))) 
        vectory <- NA
        mod_plot <- lm(c(matrix[y2,x2],matrix[y1,x1])~c(1,length(vectorx)))
      }
      for (x in vectorx){
        vectory <- c(vectory, mod$coefficients[2]*x + mod$coefficients[1])
      }
      vectory <- round(vectory[-1],0)
      vectorx <- round(vectorx,0)
      for (k in 1:length(vectorx)){
        if (vectory[k]==y2 & vectorx[k]==x2) {
          NULL
        } else if(vectory[k]==y1 & vectorx[k]==x1 ) {
          NULL
        } else {
          if (is.na(matrix[vectory[k],vectorx[k]])){
            matrix[vectory[k],vectorx[k]] <- mod_plot$coefficients[1]+mod_plot$coefficients[2]*k
          } else {
            matrix[vectory[k],vectorx[k]] <- (matrix[vectory[k],vectorx[k]] + (mod_plot$coefficients[1]+mod_plot$coefficients[2]*k))/2
          }
        }
      }
      return(matrix)
    }
  } 
  for (n in 1:length(north)){
    using <- northlocator(n)
    locator <- which(keep_track==north[n], arr.in=TRUE)
    if(length(locator[,1]) < 1){
      for (u in 1:length(using)){
        matrix <- plottingnorth_2(north[n],using[u])
        keep_track <- rbind(keep_track, c(north[n],using[u]))
      }
    } else {
      loc_vec <- c(locator[1:(length(locator[,1])),1])
      for (u in 1:length(using)){
        locator2 <- which(keep_track==using[u], arr.in=TRUE)
        if (length(locator2[,1]) < 1){
          matrix <- plottingnorth_2(north[n],using[u]) 
          keep_track <- rbind(keep_track, c(north[n],using[u]))
        } else {
          loc_vec2 <- c(locator2[1:(length(locator2[,1])),1])
          if (sum(loc_vec %in% loc_vec2) >= 1){
            NULL 
          } else {
            matrix <- plottingnorth_2(north[n],using[u])
            keep_track <- rbind(keep_track, c(north[n],using[u]))
          }
        } 
      } 
    } 
  } 
  
### South
  southlocator <- function(n){
    for (t in 1:Number_of_Traps){         
      if (EtoWmatrix[t,3] == south[n]){
        EtoWposition <- t
      }
    }
    for (t in 1:Number_of_Traps) {      
      if (NtoSmatrix[t,3] == south[n]){
        NtoSposition <- t
      }
    }
    
    using <- c(NA)
    i <- 1
    while (EtoWposition-1 >= 1){
      value <- EtoWmatrix[EtoWposition-i,1] 
      locator <- which(NtoSmatrix==value, arr.in=TRUE) 
      repeat{
        if (EtoWposition-i < 1){ 
          using[1] <- NA
          using[2] <- NA
          break
        } else if (locator[1,1] > NtoSposition){ 
          using[1] <- value
          using[2] <- NtoSmatrix[locator[1,1],4]
          break 
        } else { 
          i <- i + 1 
          value <- EtoWmatrix[EtoWposition-i,1] 
          locator <- which(NtoSmatrix==value, arr.in=TRUE) 
        }
      }
      break
    }
    if(EtoWposition-i < 1){
      using[1] <- NA
      using[2] <- NA
    }
    i <- 1
    while (EtoWposition+i <= Number_of_Traps) {
      value <- EtoWmatrix[EtoWposition+i,1] 
      locator <- which(NtoSmatrix==value, arr.in=TRUE) 
      repeat{
        if (EtoWposition+i > Number_of_Traps){ 
          using[3] <- NA
          using[4] <- NA
          break
        } else if (locator[1,1] > NtoSposition){ 
          using[3] <- value
          using[4] <- NtoSmatrix[locator[1,1],2]
          break 
        } else { 
          i <- i + 1
          value <- EtoWmatrix[EtoWposition+i,1]
          locator <- which(NtoSmatrix==value, arr.in=TRUE)
        }
      }
      break
    }  
    if (EtoWposition+i > Number_of_Traps){
      using[3] <- NA
      using[4] <- NA
    }    
    return(using)
  }
  plottingsouth_2 <- function(Bottles1,Bottles2){
    if (is.na(Bottles2)){
      return(matrix)
    } else {
      for (r in 1:Number_of_Traps){
        if (Bottles1 %in% Traps[[r]][[2]]){
          y1 <- Traps [[r]][[1]][[2]] - 1
          x1 <- Traps [[r]][[1]][[1]] 
        }
      }
      adjuster <- which(using==Bottles2, arr.in=TRUE)
      if(adjuster == 1){
        y <- 1
        x <- 0
      } else if (adjuster == 2){
        y <- 0
        x <- -1
      } else if (adjuster == 3){
        y <- 1
        x <- 0
      } else {
        y <- 0
        x <- 1
      }  
      for(g in 1:Number_of_Traps) { 
        if (Bottles2 %in% Traps[[g]][[2]]){
          y2 <- Traps [[g]][[1]][[2]]+y
          x2 <- Traps [[g]][[1]][[1]]+x
        }
      }
      if (x1 < x2){
        mod <- lm(c(y1,y2)~c(x1,x2))
        vectorx <- seq((x1+1),(x2-1), by=((x2-x1)/max((y2-y1),(x2-x1))))
        vectory <- NA
        mod_plot <- lm(c(matrix[y1,x1],matrix[y2,x2])~c(1,length(vectorx)))
      } else {
        mod <- lm(c(y2,y1)~c(x2,x1))
        vectorx <- seq((x2+1),(x1-1), by=((x1-x2)/max((y1-y2),(x1-x2)))) 
        vectory <- NA
        mod_plot <- lm(c(matrix[y2,x2],matrix[y1,x1])~c(1,length(vectorx)))
      }
      for (x in vectorx){
        vectory <- c(vectory, mod$coefficients[2]*x + mod$coefficients[1])
      }
      vectory <- round(vectory[-1],0)
      vectorx <- round(vectorx,0)
      if (vectory[k]==y2 & vectorx[k]==x2) {
        NULL
      } else if(vectory[k]==y1 & vectorx[k]==x1 ) {
        NULL
      } else {
        if (is.na(matrix[vectory[k],vectorx[k]])){
          matrix[vectory[k],vectorx[k]] <- mod_plot$coefficients[1]+mod_plot$coefficients[2]*k 
        } else {
          matrix[vectory[k],vectorx[k]] <- (matrix[vectory[k],vectorx[k]] + (mod_plot$coefficients[1]+mod_plot$coefficients[2]*k))/2
        }
      }
      return(matrix)
    }
  }    
  for (n in 1:length(south)){
    using <- southlocator(n)
    locator <- which(keep_track==south[n], arr.in=TRUE)
    if(length(locator[,1]) < 1){
      for (u in 1:length(using)){
        matrix <- plottingwest_2(south[n],using[u])
        keep_track <- rbind(keep_track, c(south[n],using[u]))
      }
    } else {
      loc_vec <- c(locator[1:(length(locator[,1])),1])
      for (u in 1:length(using)){
        locator2 <- which(keep_track==using[u], arr.in=TRUE)
        if (length(locator2[,1]) < 1){
          matrix <- plottingsouth_2(south[n],using[u]) 
          keep_track <- rbind(keep_track, c(south[n],using[u]))
        } else {
          loc_vec2 <- c(locator2[1:(length(locator2[,1])),1])
          if (sum(loc_vec %in% loc_vec2) >= 1){ 
            NULL
          } else {
            matrix <- plottingsouth_2(north[n],using[u])
            keep_track <- rbind(keep_track, c(south[n],using[u]))
          }
        } 
      } 
    } 
  } 

### implimented all assumptions, time to use regression    
  # are now good to use regression to estimate, have it test al AIC variables to decide what to go with
  
  values <- NA
  S <- 1
  F <- DX
### converts matrix into vecotr, alignign rows end to end  
  for (k in 1:DY){
    values[S:F] <- matrix[k, ]
    S <- S + DX
    F <- F + DX
  }
### two vectors that represnt the matrix coordinates for each entry in values  
  coorX <- c(rep(1:DX,times=DY))
  coorY <- c(rep(1:DY, each = DX))
### values are modeled using theor coordinaes in matrix    
### multipel models are devloped and AIC values examined to chose "best one"  
### parabolic, no interaction
  mod1 <- lm(values~coorX+coorY+I(coorX^2)+I(coorY^2))
  AIC(mod1)
### parabolic, with interaction
  mod2 <- lm(values~coorX+coorY+I(coorX^2)+I(coorY^2)+coorX*coorY)
  AIC(mod2)
### cubic, no interaction
  mod3 <- lm(values~coorX+coorY+I(coorX^2)+I(coorY^2)+I(coorX^3)+I(coorY^3))
  AIC(mod3)
### cubic, with interactions
  mod4 <- lm(values~coorX+coorY+I(coorX^2)+I(coorY^2)+I(coorX^3)+I(coorY^3)+coorX*coorY)
  AIC(mod4)
### AIC values are sorted smallest to largest
  sort <- sort(c(AIC(mod1),AIC(mod2),AIC(mod3),AIC(mod4)))
### The lowest AIC is identified and the corrisponding model is used to fill in the matrix
  if (AIC(mod1)==sort[1]){
    predict <- matrix(NA,DY,DX)
    for (y in 1:DY){
      for (x in 1:DX){
        predict[y,x] <- mod1$coefficients[1]+mod1$coefficients[2]*x+mod1$coefficients[3]*(y)+mod1$coefficients[4]*(x^2)+mod1$coefficients[5]*(y^2)
      }
    }
  } else if (AIC(mod2)==sort[1]){
    predict <- matrix(NA,DY,DX)
    for (y in 1:DY){
      for (x in 1:DX){
        predict[y,x] <- mod2$coefficients[1]+mod2$coefficients[2]*x+mod2$coefficients[3]*(y)+mod2$coefficients[4]*(x^2)+mod2$coefficients[5]*(y^2)+mod2$coefficients[6]*(x*y)
      }
    }
  } else if (AIC(mod3)==sort[1]){
    predict <- matrix(NA,DY,DX)
    for (y in 1:DY){
      for (x in 1:DX){
        predict[y,x] <- mod3$coefficients[1]+mod3$coefficients[2]*x+mod3$coefficients[3]*(y)+mod3$coefficients[4]*(x^2)+mod3$coefficients[5]*(y^2)+mod3$coefficients[6]*(x^3)+mod3$coefficients[7]*(y^3)
      }
    }
  } else {
    predict <- matrix(NA,DY,DX)
    for (y in 1:DY){
      for (x in 1:DX){
        predict[y,x] <- mod4$coefficients[1]+mod4$coefficients[2]*x+mod4$coefficients[3]*(y)+mod4$coefficients[4]*(x^2)+mod4$coefficients[5]*(y^2)+mod4$coefficients[6]*(x^3)+mod4$coefficients[7]*(y^3)+mod4$coefficients[8]*(x*y)
      }
    }
  }
### the matrix is rounded to whole numbers since the units are rganisms so decimal does not make sense  
  complete[[W]] <- round(predict,0)
### will appear on screen that this week has been completed  
  show(paste("completed", W))
} 
### once all the weeks have been predicted all matrixes stored on list completed  
### now need to trim square matrix into field shape

### create vector lisiting all x matrix coordinates of field corners   
k <- 1
xcoor <- c()
for (x in 1:(length(Corners)/2)){
  xcoor[x] <- Corners[k]
  k <- k+2
}
xcoor <- c(xcoor) ###### note to me, check if needed
### create vector lisiting all x matrix coordinates of field corners   
k <- 2
ycoor <- c()
for (y in 1:(length(Corners)/2)){
  ycoor[y] <- Corners[k]
  k <- k+2
}
ycoor <- c(ycoor)


### vectors defining all coordinates east of the most north point and west of north most point
xcoor_west <- xcoor[1:which(ycoor==max(ycoor))]
xcoor_east <- c(xcoor[1], rev(xcoor[which(ycoor==max(ycoor)):length(xcoor)])) 
ycoor_west <- ycoor[1:length(xcoor_west)]
ycoor_east <- c(ycoor[1], rev(ycoor[which(ycoor==max(ycoor)):length(ycoor)])) 

# this for loop wil di all the west coordinantes   
wait <- FALSE
### A dataframe that records which colunms in each row on the west side are not actually part of the field boundaries 
MBW <-  data.frame(row = c(NA), start = c(NA), stop = c(NA)) #MB = master boarder
for(k in 1:(length(xcoor_west)-1)){
  if (ycoor_west[k+1] < ycoor_west[k] & xcoor_west[k+1] < xcoor_west[k]){
    y <- seq(ycoor_west[k],ycoor_west[k+1],by=-1)
    seq <- seq(nrow(MBW), by=-1, length.out= length(y))
    MBW[seq[length(seq)]:seq[1],2] <- rev(round(seq(xcoor_west[k],xcoor_west[k+1],length.out=length(y)),0))
  } else if (ycoor_west[k+1] < ycoor_west[k] & xcoor_west[k+1] > xcoor_west[k]){ 
    y <- seq(ycoor_west[k],ycoor_west[k+1],by=-1)
    seq <- seq(nrow(MBW), by=-1, length.out= length(y))
    wait <- TRUE
    y_w <- y
    seq_w <- seq
  } else if (wait == TRUE){
    y <- seq(ycoor_west[k],ycoor_west[k+1],by=1)
    segment <- data.frame(row = y, start = rep(1, times= length(round(seq(xcoor_west[k],xcoor_west[k+1],length.out=length(y)),0))), stop = round(seq(xcoor_west[k],xcoor_west[k+1],length.out=length(y)),0))
    segment[1:length(y_w),2] <- seq_w
    MBW <- rbind(MBW, segment)
    wait <- FALSE
  } else {
    y <- seq(ycoor_west[k],ycoor_west[k+1],by=1)
    segment <- data.frame(row = y, start = rep(1, times= length(round(seq(xcoor_west[k],xcoor_west[k+1],length.out=length(y)),0))), stop = round(seq(xcoor_west[k],xcoor_west[k+1],length.out=length(y)),0))
    MBW <- rbind(MBW, segment)
  }   
}
MBW <- MBW[-1,]
### A dataframe that records which colunms in each row on the east side are not actually part of the field boundaries 
wait <- FALSE 
MBE <-  data.frame(row = c(NA), start = c(NA), stop = c(NA)) #MB = master boarder
for(k in 1:(length(xcoor_east)-1)){
  if (ycoor_east[k+1] < ycoor_east[k] & xcoor_east[k+1] > xcoor_east[k]){
    y <- seq(ycoor_west[k],ycoor_west[k+1],by=-1)
    seq <- round(seq(xcoor_east[k], xcoor_east[k+1],length.out= length(y)),0)
    MBE[(1+nrow(MBE)-length(seq)):nrow(MB),3] <- rev(seq)
  } else if (ycoor_east[k+1] < ycoor_east[k] & xcoor_east[k+1] < xcoor_east[k]) { 
    y <- seq(ycoor_east[k],ycoor_east[k+1],by=-1)
    seq <- round(seq(xcoor_east[k], xcoor_east[k+1],length.out= length(y)),0)
    wait <- TRUE
    y_e <- y
    seq_e <- seq
  } else if (wait ==TRUE) {
    y <- seq(ycoor_east[k],ycoor_east[k+1],by=1)
    segment <- data.frame(row = y, start = round(seq(xcoor_east[k],xcoor_east[k+1],length.out=length(y)),0), stop = DX)
    segment[1:length(y_e),3] <- rev(seq_e)
    MBE <- rbind(MBE, segment)
    wait <- FALSE
  } else {
    y <- seq(ycoor_east[k],ycoor_east[k+1],by=1)
    segment <- data.frame(row = y, start = round(seq(xcoor_east[k],xcoor_east[k+1],length.out=length(y)),0), stop = rep(DX, times= length(round(seq(xcoor_east[k],xcoor_east[k+1],length.out=length(y)),0))))
    MBE <- rbind(MBE, segment)
  }   
}
MBE <- MBE[-1,]
### replaces parts of matrix that aren't in field with NA
for (W in 1:Number_of_Weeks){
  for (k in 1:nrow(MBW)){
    complete[[W]][MBW$row[k],MBW$start[k]:MBW$stop[k]] <- NA
  }
  for (k in 1:nrow(MBE)) {
    complete[[W]][MBE$row[k],MBE$start[k]:MBE$stop[k]] <- NA
  }
}
### processing is finsihed, now is code for making pdf of heat maps

### scans number values across all the weeks to find over max and min 
vector <- c()
v <- 1
for(W in 1:Number_of_Weeks){
  working <- complete[[W]]
  for(l in 1:length(working)){
    if (is.na(working[l])!=TRUE){
      vector[v] <- working[l]
      v <- v+1
    }
  }
}
max <- max(vector)
min <- min(vector)
  
  checker <- round((min/100),0)
  checker2 <- checker - (min/100)
  
  if (checker2 == 0){
    lowest_hundred <- checker2
  } else if (checker2 < 0){
    checker2 <- checker2 * (-1)
    lowest_hundred <- ((1 - checker2)*100) + min
  } else {
    lowest_hundred <- (checker2*100) + min
  }
  
  
  checker <- round((max/100),0)
  checker2 <- checker - (max/100)
  if (checker2 == 0){
    highest_hundred <- checker2
  } else if (checker2 < 0){
    checker2 <- checker2 * (-1)
    highest_hundred <- max - (checker2*100) 
  } else {
    highest_hundred <- max - (1 - checker2)*100 
  }
  
  scale <- rev(seq(lowest_hundred,highest_hundred, by = 100))

  if (max - highest_hundred < 50){ # if max seen is under 50 away from nearest hundred, cap scale at *50
    scale <- c((scale[1] + 50), scale)
    H50 <- TRUE #high 50 (highest vakue contains 50)
  } else { # its over 50 away so cap with nearest hundred 
    scale <- c(scale[1]+100, scale)
    H50 <- FALSE
  }
  if(lowest_hundred - min > 50){ # min  is over 50 away form nearets hundred so end scale on **0
    scale <- c(scale,scale[length(scale)]-100)
    L50 <- FALSE
  } else { # end with 50
    scale <- c(scale,scale[length(scale)]-50)
    L50 <- TRUE
  }
  
  #sets up colours, can be done once 
  ORIGscipen <- getOption("scipen") 
  options(scipen=10)
  mycol <- colorRampPalette(col)
  ramp <- data.frame("ID"=rev(seq(scale[length(scale)], scale[1], 1)), "col"=mycol(length(seq(scale[length(scale)], scale[1], 1)))[1:length(seq(scale[length(scale)], scale[1], 1))], stringsAsFactors=F)
 
  L <- round(DX * .25/length(ramp$col),2)
  if (H50==TRUE & L50==TRUE){
    lines <- c(0.5, seq((L*50+0.5), length.out = (length(scale)-2), by = L*100))
    lines <- c(lines, lines[length(lines)]+L*50)
  } else if (H50==TRUE){
    lines <- c(0.5, seq((L*50+0.5), length.out = (length(scale)-1), by = L*100))
  } else if (L50==TRUE){
    lines <- seq(0.5, length.out = (length(scale)-1), by = L*100)
    lines <- c(lines, lines[length(lines)]+L*50)
  } else {
    lines <- seq(0.5, length.out = length(scale), by = L*100)
  }
 
  
### repeated    
  for (W in 1:Number_of_Weeks){
    write.csv(complete[[W]],"out.csv") #export working matrix, will need ot be in loop
    input <- read.csv("out.csv", stringsAsFactors=F)
    input <- input[,-c(1)]
    data <- input
  
    
    #sets up naming, must be re run every run through 
    if (plot_name != ""){ 
      out = paste(field_name,"_week",Week_names[W],"_",plot_name,".pdf", sep="")
      title <- paste("Week",Week_names[W],plot_name, sep=" ")
    } else {
      out = paste(field_name,"_week",Week_names[W],".pdf", sep="")
      title <- paste("Week",Week_names[W], sep=" ")
    }
    
    if(out!=""){
      pdf(out)
    }
    #creates the plot, must be rerun every run through 
    par(mar=c(0,0,0,0))
    plot(NULL, ylim=c(0, (DY+10)), xlim=c(0, DX), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
    for (m in 1:DX){
      for(i in 1:DY){
        if (is.na(data[i,m])){
          NULL
        } else {
          wichColor <- which(data[i,m]>ramp$ID)[1]
          colour <- paste(ramp$col[wichColor])
          rect(m-0.5, i-0.5, m+0.5, i+0.5, col=colour, border=colour)
        }
      }
    }
    
    #this is the part that must get repeated every time
    for(i in 1:length(ramp$col)){
      rect(0.4+i*L, DY+20, (0.4+L)+i*L, DY+12.5, col=paste(ramp$col[i]), border=paste(ramp$col[i]))
    }
    
    k <- 1
    for (i in lines){
      text(i, DY+9, labels=scale[k], srt=90, adj=1, cex=0.6)
      lines(x = c(i,i), y = c(DY+11.5, DY+13.5))
      k <- k+1
    }
    text((DX/2)-50, DY+9, labels=title, adj=0, cex=1)
    #ending
    if(out!=""){
      dev.off()
    }
    show(paste("round", W, "completed")) 
  }  
}



