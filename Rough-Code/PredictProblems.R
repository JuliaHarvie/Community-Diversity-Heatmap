

## Playing around with predict

currentR <- predict(Model,newdata=newdata,type="response")
currentL <- predict(Model,newdata=newdata,type="link")
currentT <- predict(Model,newdata=newdata,type="terms")
coefficients(Model)
head(currentL)
head(newdata)

value <-  -1.2544995003 + -0.0002606295*757 + -0.0008790872*1 + 2.1774816480 #matches currentL
pnorm(value)

Y <- 1
X <- 1
E <- 1
newdata <- data.frame("LongX" = rep(Boarder[[Y]][X],time=length(Unique)),
                      "LatY" = rep(Y,time=length(Unique)),
                      "Event" = rep(E,time=length(Unique)),
                      "Repeat" = Repeat) 
matrix[Y,Outline[[Y]][X]] <- sum(ifelse(predict(Model,newdata=newdata,type="response")>=0.5,1,0))

help(glm)

Cars <- datasets::mtcars

model <- glm(vs~wt+disp,data=Cars,family=binomial)
summary(model)
newdata = data.frame(wt = 2.1, disp = 180)
predict(model, newdata, type="response")
predict(model, newdata, type="link")
qnorm(0.2361081)
pnorm(-1.174136,0.5,2)
