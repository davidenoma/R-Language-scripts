

# Syntax ------------------------------------------------------------------
# FunctionName = function(argument1, argument2, ...) {
#
# 'R code here'
#
# return(results) # Return calculation results
# }
#
# Calling function - FunctionName(argument1, argument2, ...)



# First function ----------------------------------------------------------

SumTwo = function(first_num, second_num) {
  
  result = first_num + second_num
  
  return(result)
}


# Using function
SumTwo(first_num = 2, second_num = 4)




# Another example ---------------------------------------------------------
# Create function, which would create person variable -  
# list with name, surname, age, birth year

CreatePerson = function(name, surname, age) {
  
  current_time = Sys.time() # get exact time
  current_year = format(current_time, "%Y") # extract current year
  birth_year = as.numeric(current_year) - age
  
  person = list(Name = name, Surname = surname, Age = age, 
                Birth_year = birth_year)
  
  return(person)
}

person1 = CreatePerson(name = "John", surname = "Smith", age = 30)
person2 = CreatePerson(name = "Tim", surname = "Ferris", age = 24)


# Various equivalent options to call a function
CreatePerson(name = "John", surname = "Smith", age = 30)
CreatePerson(age = 30, surname = "Smith", name = "John")
CreatePerson("John", "Smith", 30)
CreatePerson("John", surname = "Smith", 30)
