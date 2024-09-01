# Name: Roxanne Ysabel Resuello
# Date: November 20, 2022 - 11:00pm
# Exercise 09: An R script that implements simplex method

# Function that computes for the final tableau
getFinalTab <- function(tableau){
  # Variable initialization
  numRow <- nrow(tableau)
  numCol <- ncol(tableau)
  
  # returns NA if there is no negative value in the last row
  if(min(tableau[numRow,])>=0){
    print("No negative")
    return(NA)
  }

  # While there is a negative value in the last row, continue iteration
  while (TRUE) {
    
    # If the minimimum value in the last row is positive, stop iteration
    if(min(tableau[numRow,])>=0){
      break
    }
    
    # Set pivot column
    pColIndex<-which.min(tableau[numRow,])
    pCol<-tableau[,pColIndex]
    
    # Compute test ratio and get the smallest TR
    testRatio <- c(0:(numRow-1))
    smallestTRIndex <- 0
    
    for(i in 1:numRow){
      if(pCol[i] < 1 || tableau[i,numCol] < 1){
        testRatio[i] = -1
      } else {
        testRatio[i] = tableau[i,numCol]/pCol[i]
      }
    }
    
    lowestTR <- which(testRatio > 0)
    lowestTRvalues <- testRatio[lowestTR]
    lowestTR2 <- which.min(lowestTRvalues)
    
    # Set the pivot row using the smallest tr
    pRowIndex <- which(testRatio == lowestTRvalues[lowestTR2])[1]
    pivotRow <- tableau[pRowIndex,]
    
    # Get pivot element
    pe <- tableau[pRowIndex, pColIndex]
    
    # if pivot element is NA, return NA
    if(is.na(pe)){
      return(NA)
    }

    #Normalize pivot row and update the tableau
    npr <- pivotRow/pe
    tableau[pRowIndex,] = npr

    # Do elimination
    for(i in 1:nrow(tableau)){
      if(i != pRowIndex){
        c <- tableau[i,pColIndex]
        tableau[i,] = tableau[i,] - (npr * c)
      }
    }
    
  }
  return(tableau)
}

# Function that implement simplex method
simplex <- function(tableau, isMax, problem){
  # If isMax is true, do maximization, else, do minimization
  if(isMax == TRUE){
    # get final tableau
    final.tableau <- getFinalTab(tableau)
    
    # If final tableau is NA, return NA
    if(length(final.tableau) == 1 && is.na(final.tableau)){
      return(NA)
    }
    
    # initialize the basic solution
    basic.solution <- final.tableau[1, -ncol(tableau)]
 
    # Gets the value of the basic solution from the final tableau
    for(i in 1:(ncol(final.tableau)-1)){
      rowIndex <- which(final.tableau[,i] == 1)
      if(length(rowIndex) == 0){
        basic.solution[i] = 0
      } else{
        basic.solution[i] <- final.tableau[rowIndex, ncol(tableau)]
      }
    }
    
  } else{
    # get final tableau
    final.tableau <- getFinalTab(tableau)
    
    # If final tableau is NA, return NA
    if(length(final.tableau) == 1 && is.na(final.tableau)){
      return(NA)
    }
    
    # Gets the value of the basic solution from the final tableau
    basic.solution <- final.tableau[nrow(final.tableau), -ncol(tableau)]
    basic.solution[length(basic.solution)] = final.tableau[nrow(final.tableau), ncol(final.tableau)]
    
  }
  
  # If problem is true, return an additional matrix that represents the number of items to be shipped
  # else, only return the final tableau, basic solution, and optimum value
  if(problem == TRUE){
    # Set up the matrix that represents the number of item to be shipped using the values from the basic solution
    dvalue <- matrix(c(0:14), nrow = 3, dimnames = list(c("DEN", "PHO", "DAL"), c("SAC", "SL", "ALB", "CHI", "NYC")))
    
    for (i in 1:3) {
      for (j in 1:5) {
        dvalue[i,j] = basic.solution[8+(5*(i-1))+j]
      }
    }
    return(list(final.tableau = final.tableau, basic.solution = basic.solution, opt.val = as.vector(basic.solution[length(basic.solution)]), shipping.num = dvalue))
  }else{
    return(list(final.tableau = final.tableau, basic.solution = basic.solution, opt.val = as.vector(basic.solution[length(basic.solution)])))
  }
  
}

# # Maximization Test Cases
# initialTab <- matrix(c(7, 10, 1, 0, -150, 11, 8, 0, 1, -175, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,0,0,0,1,77,80,9,6,0), nrow = 5, dimnames = list(NULL,c("X1", "X2", "S1", "S2", "S3", "S4", "Z", "Solution")))
# simplex(initialTab, TRUE, FALSE)
# 
# #Minimization Test Cases
# names = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "Z", "Solution")
# 
# # Test Case 1
# initialTab1 <- matrix(c(-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,-1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,-1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,6,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,4,0,-1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,6,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,5,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,4,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3,0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,6,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,3,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,4,0,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,5,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,5,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,9,310,260,280,-180,-80,-200,-160,-220,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),byrow = TRUE, nrow = 16, dimnames = list(NULL,names))
# print(initialTab1)
# simplex(initialTab1, FALSE, TRUE)
# 
# # Test Case 2
# initialTab2 <- matrix(c(-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,-1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,7,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,8,-1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,9,0,-1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,6,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,7,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,8,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,9,0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,10,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,3,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,5,0,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,7,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,11,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,13,200,200,200,-100,-100,-100,-100,-100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),byrow= TRUE, nrow = 16, dimnames = list(NULL,names))
# simplex(initialTab2, FALSE, TRUE)
# 
# # Test Case 3
# initialTab3 <- matrix(c(-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,-1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,-1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,31,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,35,-1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,33,0,-1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,26,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,24,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,23,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,25,0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,27,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,11,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,13,0,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,15,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,20,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,17,1400,400,200,-431,-332,-350,-450,-400,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),byrow = TRUE, nrow = 16, dimnames = list(NULL,names))
# simplex(initialTab3, FALSE, TRUE)
# 
# # Test Case 4
# initialTab4 <- matrix(c(-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,5,0,-1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,5,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,5,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,5,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,5,0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,5,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,5,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,5,0,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,5,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,5,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,5,100,100,100,-20,-20,-20,-20,-20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),byrow = TRUE, nrow = 16,dimnames = list(NULL, names) )
# simplex(initialTab4, FALSE, TRUE)
# 
# # Test Case 5
# initialTab5 <- matrix(c(-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,-1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,-1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,31,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,35,-1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,33,0,-1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,26,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,24,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,23,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,25,0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,27,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,11,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,13,0,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,15,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,20,0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,17,50,50,50,-20,-25,-90,-60,-70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),byrow = TRUE, nrow = 16,dimnames = list(NULL, names) )
# simplex(initialTab5, FALSE, TRUE)
