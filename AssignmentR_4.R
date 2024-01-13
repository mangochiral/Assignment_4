# Vector ------------------------------------------------------------------

vect = c(1:50)
print(vect)

# Matrix from vector Vect -------------------------------------------------

A = matrix(data = vect, nrow = 5, ncol = 10)
print(A)

# Matrix with conditions --------------------------------------------------

B = c()
numB = c()
for (i in vect){
  if (i%%3 == 0 | i%%6 == 1){
    B = c(B,TRUE)
    numB = c(numB, i)
  }else{
    B = c(B,FALSE)
  }
}
print(B)
print(numB)
Bma = matrix(data = B, nrow = 5, ncol = 10)
numBma = matrix(data = numB, nrow = 5, ncol = 5)

# Hailstone function ------------------------------------------------------


hailstone <- function(x) {
  counting = 0
  while (TRUE){
    if (x != 1) {
      if (x %% 2 == 0) {
        x = x %/% 2
      } else {
        x = 3 * x + 1
      }
      counting = counting +1
    }else{
      return (counting)
    }
  }
}

newB = c(sapply(numBma,hailstone))
matrixB = matrix(data = newB, nrow = 5, ncol = 5)

# ISLR --------------------------------------------------------------------
install.packages('ISLR')
library(ISLR)
car = ISLR::Auto
print(colnames(car))
remove_datatype = subset(car, select = -c(name))
?range
col_name <- colnames(remove_datatype)
print(col_name)
car_mean <- c()
car_sd <- c()

# Loop through the clean Dataframe ------------------------------------------------------------


for (key in colnames(remove_datatype)) {
  cat('Range of', key, range(remove_datatype[[key]]), '\n')
  car_bycol <- mean(remove_datatype[[key]])
  car_mean <- c(car_mean, car_bycol)
  car_bycol_devs <- sd(remove_datatype[[key]])
  car_sd <- c(car_sd, car_bycol_devs)
}

# Mean --------------------------------------------------------------------


combine_mean <- matrix(data = car_mean, nrow =1, ncol = ncol(remove_datatype))

colnames(combine_mean) <- col_name

# Standard Deviation ------------------------------------------------------


combine_dev <-  matrix(data = car_sd, nrow =1, ncol = ncol(remove_datatype))

colnames(combine_dev) <- col_name


new_car_mean <- c()
new_car_sd <- c()
car_mpg <- remove_datatype[remove_datatype$mpg > mean(Auto$mpg), ]

# Loop through new dataframe with removed data less than previous mean of each column ------------------------------------------------------------


for (key in colnames(remove_datatype)) {
  new_car_bycol <- mean(car_mpg[[key]])
  new_car_mean <- c(new_car_mean, new_car_bycol)
  new_car_bycol_devs <- sd(car_mpg[[key]])
  new_car_sd <- c(new_car_sd, new_car_bycol_devs)
}

# New Mean --------------------------------------------------------------------

new_combine_mean <- matrix(data = new_car_mean, nrow =1, ncol = ncol(remove_datatype))

colnames(new_combine_mean) <- col_name

# New Standard Deviation ------------------------------------------------------

new_combine_dev <-  matrix(data = new_car_sd, nrow =1, ncol = ncol(remove_datatype))

colnames(new_combine_dev) <- col_name

###############################################################################################################################################

# Based on the output of Python script by generative AI, python has a list of complicated library to make and clean DataFrame, which needs expertise in python. 
# Whereas in R the operations in DataFrame with basic builtin library making it easy to use. However, due to python readability and flexibility in using conditions 
# makes it a choice of one stop shop programming language.
