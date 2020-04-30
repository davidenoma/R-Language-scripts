

# From text -------------------------------------------------------------------------

data_comma = read.csv(file = "data_comma.csv", stringsAsFactors=F, sep=",")
data_semicolon = read.csv(file = "data_semicolon.csv", stringsAsFactors=F, sep=";")
data_tab = read.csv(file = "data_tab.csv", stringsAsFactors=F, sep="\t")






# Datasets package ------------------------------------------------------------------
library(datasets)
data() # displays all available data sets





# Loading variables -----------------------------------------------------------------
# Firstly save variable to .RData file
ChickWeight_rdata = ChickWeight
save(ChickWeight_rdata, file="ChickWeight.RData")
rm(list = ls())
load("ChickWeight.RData")





# Reading from excel ----------------------------------------------------------------
library(XLConnect) # make sure you have already installed it

wb = loadWorkbook("data_excel.xlsx")
data_excel = readWorksheet(wb, sheet = 1, region = "A1:D21")



