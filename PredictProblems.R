

## Playing around with predict

currentR <- predict(Model,newdata=newdata,type="response")
currentL <- predict(Model,newdata=newdata,type="link")
currentT <- predict(Model,newdata=newdata,type="terms")
coefficients(Model)
head(currentR)
head(newdata)

value <-  -1.2544995003 + -0.0002606295*757 + -0.0008790872*1 + 2.1774816480
pnorm(value)

Y <- 1
X <- 1
E <- 1
newdata <- data.frame("LongX" = rep(Boarder[[Y]][X],time=length(Unique)),
                      "LatY" = rep(Y,time=length(Unique)),
                      "Event" = rep(E,time=length(Unique)),
                      "Repeat" = Repeat) 
matrix[Y,Outline[[Y]][X]] <- sum(ifelse(predict(Model,newdata=newdata,type="response")>=0.5,1,0))
