#Step 1: SetupFun

#### Will requirre two preloaded Datasets here
DataSetOne <- read.csv("/Users/julia/Documents/Projects/AgMonitoringSDM/AGAWFiles/AGAW_coords_field.csv")
DataSetTwo <- read.csv("/Users/julia/Documents/Projects/AgMonitoringSDM/AGAWFiles/AGAW_coords_trap.csv")
  
setup <- SetupFun(Border_Coordinates = DataSetOne, Trap_Coordinates = DataSetTwo,
                  Number_of_Sampling_Events = 9, Four_Headed_Trap = T)

# Optional Step: Formatting mBrave 
# would like to remove this from offical package and just have output as a preloaded Dataset

library(data.table)
library(stringr)
File <-  as.data.frame(fread("/Users/julia/Documents/Projects/AgMonitoringSDM/AGAWFiles/AGAW.tsv"))
formatted <- Formatting_mBraveFun(File=File,ID = "AGAW", Suffix_Symbol = ")", Four_Headed_Trap = T)
write.csv(formatted, "SamplingData.csv")
remove.packages("data.table")
remove.packages("stringr")

#Step 2: DataFun

### Will require a third preloaded Dataset
DataSetThree <- read.csv("/Users/julia/Documents/Projects/AgMonitoringSDM/AGAWFiles/SamplingData.csv")
data <- DataFun(File = DataSetThree, Setup = setup)


#Step 3: ConversionFun

conversion <- ConversionFun(Setup = setup,km = F, Poles = F)


#Step 4A: MasterDataFrame Linear

masterdataframeA <- MasterDataFrameFun(Data = data, Setup = setup, Conversion = conversion,
                                       Repeat=T, Model_Type =  "Linear")

#Step 4B: MasterDataFrame Binomial
masterdataframeB <- MasterDataFrameFun(Data = data, Setup = setup, Conversion = conversion,
                                      Repeat=T, Model_Type =  "Binomial")


#Step 5A: DownSampleFun
# A preporation step to assits in model building which will come next

library(caret)
library(data.table)
downsampledA <- DownSampleFun(Dataframe = masterdataframeA)

#Step 5B: DownSampleFun
# A preporation step to assits in model building which will come next

library(caret)
library(data.table)
downsampledB <- DownSampleFun(Dataframe = masterdataframeB)


#Step 6: Modeling
#### instead of using a package specific function it is intended users will use lm, glm or 
#### any other r modeling function they feel is appropriate. Regression analysis and
#### the testing of multiple models before ocntinuing down the pipeline. The lack of restrictions at this 
#### step is to encourage the user to find the bets fitting model for their specific data. 

LinearModel <- lm(Found~ LongX+LatY+Event, data= downsampledA[[1]])
BinomialModel <- glm(Found~ LongX+LatY+as.factor(Event)+as.factor(Repeat), family = binomial, data = downsampledB[[1]])

#Step 7 AccuracyFUN
#### Note to me this step needs more clairfication on how I want to use it

AccuracyFun(LinearModel,downsampledA[[3]])
AccuracyFun(BinomialModel,downsampledB[[3]])

#Step 8: BorderFUN

#There may be an issue with the percent indicator here

Border <- BorderFun(Conversion = conversion)

plot(Border)






