data = read.csv("assign4.csv",header = ,sep=",")
head(data)
names(data)
summary(data)
install.packages("pacman")
pacman::p_load(lars,caret)
library(lars)
library(pacman)

wine_data = read.csv("winequality-red.csv")

x <- as.matrix(wine_data[-12])
y <- wine_data[,12]
#Backwared feature selection with recursive 
#feature selection 
ctrl <- rfeControl(method = "repeatedcv",
           repeats = 5, 
           verbose = TRUE,
           functions = lmFuncs)
rfe <- rfe(x,y,
           sizes = c(1:11),
           rfeControl = ctrl)
rfe
x <- as.matrix(wine_data[rfe$optVariables])

#Conventional Stepwise regression 

stepwise <- lars(x,y,type = "stepwise")

#Stage wise but with better generalizability 

forward <- lars(x,y, type = "forward.stagewise")

# Least angle regression 

lar <- lars(x,y, type="lar")

#LASSO 
lasso <- lars(x,y, type="lasso")

#Comparison of models 

r2comp <- c(stepwise$R2[6], forward$R2[6],
            lar$R2[6], lasso$R2[6])
names(r2comp) <- c("stepwise","forward","lar","lasso")
r2comp

