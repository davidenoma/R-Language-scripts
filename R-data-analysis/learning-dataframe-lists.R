#First creation of a data frame
data.frame(name="John",surname="Smith")

#Create datafrom with vector column values. 
data1 = data.frame(name=c("David","Job"),surname=c("Enoma","Welch"))
class(data1)

nrow(data1)
ncol(data1)

#to join data frames 
data2 = data.frame(name="Peter",surname="Bishop")
data = rbind(data1,data2)

data = cbind(data,height=c(11,123,33))

rbind(data1,data2,data1)

data[,c(1,2)]

data[,1:2]

data[-1]

#Subsetting on columnnames is possible
#The first argument below is the column name


data[c("1","2"),c("name","height")]

subset(data,name=="David")
subset(data, height <= 100)

colnames(data)
rownames(data)

#We can also rename the column and rownames 
rownames(data) = c("first","second","third")
colnames(data) = c("First_name","Surname","Height")

##THIS SECTION DEALS WITH THE LIST DATATYPE
person1 = list(name="john",age=10)
person1[[1]]

cars_john = c("hinda","Ferrari")
person = list(name="Johh", surname="Wayo", cars = cars_john)
person$cars[2] == "Ferrari"
person[c(1,2)]

#Adding new elements to list 
person1[["height"]] = 180
person1

#Element of any class can be added 

person1[["ChildrenNumber"]] = 2

data_john = data.frame(ChildrenName= c("Tom","Kate"),age=(4))
data_john["weight"] = c("89 kg", "66 kg")
data_john["height"] = 899

##<More complicated example 
person2 = list(Name="Tim", Surname= "Cook")
all_people = list(first=person1,second=person2)

all_people[["first"]][["Name"]]



##This is the section of the lists
person1 = list(Name="John", Surname = "Smith")
person["name"]

##Some work with Data Frames
data1 = data.frame(State="Texas",Temperature="56.4")
data2 = read.csv("3-7 data.csv")
data = rbind(data2,data1)
data[7,"Temperature"]

data[data[,"State"]=="Colorado","Temperature"] = 47
data [data[,2]=="Colorado","Temperature"] = 54

data[data[,"State"]=="Colorado",]
temp = data[,"Temperature"]

temp = data[,"Temperature"]
  temp_celcius = (temp - 32)/1.8
data = cbind(data,Temperature_C = temp_celcius)

colnames(data)
colnames(data)[2] = "Temperature_F"


