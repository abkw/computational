
#First question
x1<-1/3
x2<-1/4

if(x1-x2==1/12){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
}
#The result of the calculation of (x1-x2) is 0.5, and that is becuse the result of x1 (1/3) will be approximated to (0.3333333) 
#but it's not its actual value because it has infinite threes, As for the reuslt of x2 it will be 0.25  
#1/12 will be appoximated to a longer digits because it's very small compared to 1/3, due to underflow


x1<-1
x2<-1/2
if(x1-x2==1/2){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
}

#In this example because the numbers could be represented in a limited fractional part, that is why the devision here will result 
#in a limited fractional part real number.



#Second questioin:
calculateDrivative= function(x){
  epsilon <- 10^-15
  derivative <- (x + epsilon - x)/epsilon
  return(derivative)
}

#When x = 1, the value optained is:
calculateDrivative(1)

#When x = 100000, the value optained is:
calculateDrivative(100000)

#The true values should be 1 for both x = 1, and x = 100000
#for the x = 1 when adding 1 + 10^-15, it approximate to the nearest possible value
#for the x = 100000, the value of 10^-15 is goingn to be ignored, and it's going to be 100000 - 100000, 
#that is why the result is zero


#Question three

#1 writing the function
myvar = function(x){
  n = length(x)
  xSquare <- x^2
  # sumXallSquare <-
  variance = (1/(n-1))*(sum(xSquare)-(1/n)*(sum(x))^2)
  return(variance)
}

#second function
actual_value <- c()
var_value <- c()
myvar = function(x){
  options(digits = 22)
  yvalue = c()
  for(n in 1:length(x)){
    newX <- x[1:n]
    xSquare <- newX^2
    varianceValue = (1/(n-1))*(sum(xSquare)-(1/n)*(sum(newX))^2)
    actualVarianceValue = var(newX)
    var_value <<- c(var_value, varianceValue)
    actual_value <<- c(actual_value, actualVarianceValue)
    #3: computing the difference
    y <- varianceValue - actualVarianceValue
    yvalue <- c(yvalue,y)
  }
  plot(1:length(x),yvalue, col = "blue", type = "l", xlab = "Number of Variables", ylab = "Calculated Variance Difference")
}
#2: generating the vector
options(digits = 22)

RNGversion(min(as.character(getRversion()),"3.6.2"))
set.seed(12345, kind = "Mersenne-Twister", normal.kind = "Inversion")

xValue = rnorm(10000,mean = 10^8, sd = 1)
#Calling the function with our generated vector
myvar(xValue)
plot(1:10000, 
     y = var_value,
     col = "blue",
     type = "l",
     pch=19,
     xlab = "Number of Variables",
     ylab = "Variance",
     main = "Calculated vs Actual Variances")
lines(1:10000,
      y = actual_value,
      col = "red",
      type = "l", 
      pch=18, 
      lty=2)
legend("topleft",
       legend=c("Calculated Variance", "Actual Variance"),
       col=c("blue", "red"),
       lty=1:2,
       cex=0.8)

# plot(1:100, y = var_value[1:100], col = "blue", type = "l", xlab = "variables number", ylab = "variance", main = "Calculated vs Actual Variance")
# lines(1:100, y = actual_value[1:100], col = "red", type = "l")
#Comparint the functio with the built in function:
#There is a slight difference in variance between the myvar function and the built in
#function (var()), this difference is between (4,-4), and considering the size of our
#numbers, this difference could be considered very slight. The actual variance value
#should be around "1".

#The function behaves differently when using a large vector, for instance, when the total number
#in the vector is 10 numbers, the calculated variance and the actual variance (the variance
#when using the var() function) are both the same, but when using a vector with multiple
#large numbers, then the sign bit can be treated as a high order bit and will result in overflow.


#4: implementing a formula giving the same result as var()

myvar3 <- function(x){
  yvalue = c()
  actual_y = c()
  for (n in 2:length(x)){
    newX <- x[1:n]
    newyvalue <- (sum((newX-mean(newX))^2)/(n-1))
    yvalue <- c(yvalue,newyvalue)
    actual_y <- c(actual_y, var(newX))
  }
  plot(1:9999, yvalue, col = "blue")
  lines(1:9999, actual_y, col = "red")
  legend("topleft", 
         legend = c("Calculated Variance", "Actual Variance"),
         col = c("blue", "red"),
         lty=1:2,
         cex=0.8)
  return(yvalue-actual_y)
}
RNGversion(min(as.character(getRversion()),"3.6.2"))
set.seed(12345, kind = "Mersenne-Twister", normal.kind = "Inversion")

xValue = rnorm(10000,mean = 10^8, sd = 1)
#Calling the function with our generated vector with myvar2 function
myvar3(xValue)


#by implementing the formula (sum((newX-mean(newX))^2)/n), the value of the calculated variance
#is almost the same as the value of the actual variance (the variance using var() function), the 
#difference is 0.00 up to the second number after the decimal point.


#4: linear algebra:
#4.1 importing data intor R
library(readxl)
data = read_xls("tecator.xls")

#4.2: solving A and b for the given dataset
x = as.matrix(cbind(1,data[,-length(data)]))
#x = as.matrix(data[,-length(data)])
y = as.matrix(data[,length(data)])
A = t(x)%*%x
b = t(x)%*%y

#4.3
solve(A,b)
# system is computationally singular, meaning the design matrix is not invertible [1], there are many highly
#correlated or identical variables, if we use ridge regression here, it makes the matrix always invertible 
#introducing a penalty, if some variables are identical they will receive the same weight, and we can also 
#use select the penalization parameter by cross-validation.[2]

#4.4 condition number
kappa(A)
#The condition number of a regular matrix is the product of the norm of the matrix and the norm of its inverse
#(or pseudo-inverse), and hence depends on the kind of matrix-norm [R help]. So the condition number of our
#matrix is so big (3757739140133945) and that explains the result of step 3.

#4.5: data scaling
data = read_xls("tecator.xls")

#4.5.1: solving A and b for the given dataset
datavalue = as.matrix(cbind(1,data[,-length(data)]))
x = scale(datavalue)
#x = as.matrix(data[,-length(data)])
y = scale(as.matrix(data[,length(data)]))
A = t(x)%*%x
b = t(x)%*%y

#4.5.2
kappa(A)

#4.5.3
#When the data is scaled then the condiion number goes to NaN and Inf, becuse the product of the norm of the 
#matrix is too small, the exponent in this case is the maximum + 1, and the mantissa is not equal to zero.

#References:
#[1]https://stats.stackexchange.com/questions/76488/error-system-is-computationally-singular-when-running-a-glm
#[2]https://stats.stackexchange.com/questions/250350/how-do-i-avoid-computationally-singular-matrices-in-r