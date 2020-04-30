

# First assignment --------------------------------------------------------
# Create calculator function. 
# 2 arguments provided. Output should be data.frame with +,-,*,/ operations
# Output should be data.frame with operation results

Calculator = function(a, b) {
  # Create data frame with 4 columns and assign results to them
  result = data.frame(sum = a+b, subtraction = a-b, 
                      multiplication = a*b, division = a/b)
  
  return(result)
}

# Call function with arguments to check if everything works
# Remember to source function firstly
Calculator(c(2, 3), c(3, 5))





# Second assignment -------------------------------------------------------
# Write function, which would take 2 vectors
# number - vector of numbers, 
#     power - vector of numbers by which number should be raised
# Think about full setup yourself - 
#     there are multiple options to do same job!

power = function(a, power) {
  
  result = c()
  for (index in 1:length(a)) {
    result[index] = a[index]^power[index]
  }
  
  return(result)
}

# Calling the function
a = power(c(1:3), c(2,2,3))
