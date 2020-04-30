a = seq(5,8,length.out = 10)
a
class(a)
length(a)

a = 1:4
a == 3
a < 2
a <=2

#Converting text characters into numeric
#text_vec = c(1,3)
text_vec = c("1","3")
numeric_vec = as.numeric(text_vec)

#String functions
names_vec = c("Toyota","Mercedes","Ford")
#Returns index of that string in the given array
which(names_vec == "Mits")

#Splitting a string 
splitted = strsplit(names_vec[0]," ")
#Vector Boolean 
vec = 1:3
vec_bool = (vec > 0)  & (vec < 4)
#Check if element is in a vector
4 %in% 1:3
fruits = c("orange","banana","pineapple","")
"orang" %in% fruits
c("potate","orange") %in% fruits
