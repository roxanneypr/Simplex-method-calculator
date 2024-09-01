# Name: Roxanne Ysabel Resuello
# Date: November 20, 2022 - 11:00pm
# Exercise 08: An R script that implements quadratic spline interpolation


# Function for GaussJordan Elimination
GaussJordanElimination <- function(vmat){
  
  
  # Variable declaration
  
  numOfVariables <- nrow(vmat)
  #variables <- AugCoeffMatrix(inputList)$variables
  
  augCoeffMatrix <- vmat
  
  for(i in 1:(numOfVariables)){
    
    # Find pivot row
    if(is.na(which(augCoeffMatrix[i:numOfVariables,i] == max(abs(augCoeffMatrix[i:numOfVariables,i])))[1])){
      pivotRowIndex <- which(augCoeffMatrix[i:numOfVariables,i] == (max(abs(augCoeffMatrix[i:numOfVariables,i]))) * -1)[1] + i-1
    } else{
      pivotRowIndex <- which(augCoeffMatrix[i:numOfVariables,i] == max(abs(augCoeffMatrix[i:numOfVariables,i])))[1] + i-1
    }
    pivotRow <- augCoeffMatrix[pivotRowIndex, ]
    
    # If pivot element is 0 return NA, else swap pivot row to ith row
    if(pivotRow[i] == 0){
      return(NA)
    } else {
      augCoeffMatrix[pivotRowIndex,] <- augCoeffMatrix[i,]
      augCoeffMatrix[i,] <- pivotRow
    }
    
    pivotColumn <- augCoeffMatrix[,i]
    pivotElement <- augCoeffMatrix[i,i]
    temp <- pivotRow
    result<-pivotRow
    
    #Normalizes pivot row
    for(index in 1:length(pivotRow)){
      augCoeffMatrix[i, index] = round(pivotRow[index] / pivotElement, digits = 4)
    }
    
    pivotRow <- augCoeffMatrix[i,]
    
    # Normalizes remaining rows and performs elimination then update augCoeffMatrix with new values
    for(j in 1:(numOfVariables)){
      if(i != j){
        multiplier = pivotColumn[j]
        
        for(k in 1:length(pivotRow)){
          temp[k] = round(multiplier * pivotRow[k], digits = 4)
          augCoeffMatrix[j,k] = round(augCoeffMatrix[j, k] - temp[k], digits = 4)
          
        }
        
      }
    }
  }
  
  # Gets the solution set from the matrix
  x <- c(1:numOfVariables)
  for(i in 1:numOfVariables){
    x[i] = augCoeffMatrix[i, ncol(augCoeffMatrix)]
  }
  return(x)
}


#
# START OF EXERCISE 8
#

# Function for quadratic spline interpolation
poly.qsi <- function(data, x){
  # If the length of two vectors in the data is not equal, return NA
  if(length(data[[1]]) != length(data[[2]])){
    return(NA)
  }
  
  # Variable declaration
  n <- length(data[[1]]) - 1
  numOfEquations <- 3*n
  listOfEquations <- c()
  mat <- matrix(0, nrow = numOfEquations-1, ncol = numOfEquations+1)
  #print(mat)
  
  # Internal knots
  for(i in 2:n){
    eq = paste(data[[1]][i]^2, "a", i-1, " + ", data[[1]][i], "b", i-1, " + 1c", i-1, " = ", data[[2]][i], sep = "")
    eq2 = paste(data[[1]][i]^2, "a", i, " + ", data[[1]][i], "b", i, " + 1c", i, " = ", data[[2]][i], sep = "")
    
    listOfEquations <- append(listOfEquations, eq)
    listOfEquations <- append(listOfEquations, eq2)
  }
  
  # End points/External knots
  eq3 = paste(data[[1]][1]^2, "a", 1, " + ", data[[1]][1], "b", 1, " + 1c", 1, " = ", data[[2]][1], sep = "")
  eq4 = paste(data[[1]][n+1]^2, "a", n, " + ", data[[1]][n+1], "b", n, " + 1c", n, " = ", data[[2]][n+1], sep = "")
  listOfEquations <- append(listOfEquations, eq3)
  listOfEquations <- append(listOfEquations, eq4)
  
  # Equal 1st derivative at the interior knots
  for(i in 2:n){
    eq5 = paste(data[[1]][i]*2, "a", i-1, " + 1b", i-1, " + -", data[[1]][i]*2 ,"a", i, " + -1b", i, " = 0", sep = "")
    listOfEquations <- append(listOfEquations, eq5)
  }
  #print(listOfEquations)
  # For each generated equations, plot the coefficients of the term in the appropriate index of the matrix
  for(i in 1:length(listOfEquations)){
    splitted = strsplit(listOfEquations[i], " = ")
    mat[i, ncol(mat)] = as.double(splitted[[1]][length(splitted[[1]])])
    
    coeff = strsplit(splitted[[1]][1], " \\+ ")
    
    # Get the coefficient for each term in the equation
    for(j in 1:length(coeff[[1]])){
      term = coeff[[1]][j]
      aterm <- unlist(gregexpr('a', term))[1]
      bterm <- unlist(gregexpr('b', term))[1]
      cterm <- unlist(gregexpr('c', term))[1]
      
      # Checks if the variable is an a, b, or c, then plot it in the matrix
      if(aterm != -1){
        splittedterm <- strsplit(term, "a")
        index <- (strtoi(splittedterm[[1]][2]) - 1) * 3 + 1
        mat[i, index] = as.double(splittedterm[[1]][1])
      } else if(bterm != -1){
        splittedterm <- strsplit(term, "b")
        index <- (strtoi(splittedterm[[1]][2]) - 1) * 3 + 2
        mat[i, index] = as.double(splittedterm[[1]][1])
      } else {
        splittedterm <- strsplit(term, "c")
        index <- (strtoi(splittedterm[[1]][2]) - 1) * 3 + 3
        mat[i, index] = as.double(splittedterm[[1]][1])
      }
    }
  }
  
  # Gets the solutions set using GaussJordan elimination
  solutionset <- GaussJordanElimination(mat[,-1])
  
  if (length(solutionset) == 1) {
    if (is.na(solutionset)){
      return(NA)
    }
  }
  
  solutionset <- append(solutionset, 0, 0)
  
  # List of quadratic equations
  qsi.fxns <- list()
  
  # Setups the equations
  for(i in 1:n){
    quadraticEq <- paste("function(x) ", solutionset[(i-1)*3+1], " * x^2 + ", solutionset[(i-1)*3+2], " * x^1 + ", solutionset[(i-1)*3 + 3], sep = "")
    qsi.fxns <- append(qsi.fxns, eval(parse(text = quadraticEq)))
  }
  
  result <- NA
  
  # Checks if the x value is within the intervals
  for(i in 1:n){
    if(x >= data[[1]][i] && x <= data[[1]][i+1]){
      result <- qsi.fxns[[i]](x)
    }
  }
  
  # Returns the list of function and the estimated value
  return(list(qsi.fxns = qsi.fxns, y = result))
}


# Test Cases
# x <- c(3.0, 4.5, 7.0, 9.0)
# y <- c(2.5, 1.0, 2.5, 0.5)
# data1 <- list(x,y)
# 
# print(poly.qsi(data1, 5))
# print(poly.qsi(data1, 8))
# print(poly.qsi(data1, 8.5))
# print(poly.qsi(data1, 20))
# print(poly.qsi(data1, 3.0))
# 
# x <- c(3.0, 4.5, 7.0, 9.0, 11.0, 13.0)
# y <- c(2.5, 1.0, 2.5, 0.5, 4.0, 4.6)
# data2 <- list(x,y)
# 
# poly.qsi(data2,10)
# poly.qsi(data2,12)





