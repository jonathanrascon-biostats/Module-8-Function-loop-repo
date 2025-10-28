#Module 8: functions and loops--------

#Gerenal layout of fucntions:
#my_function <- function(VARIABLE_NAME){
 # perform operations on VARIABLE_NAME and calculate VALUE
 # VALUE
#}

compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

t <- c(1:25)

ber.function <- function(x){
    (x*(x+1))/2
}

b_n <- vector(length = m)
for(i in 1:m){
  b_n[i] <- ber.function(i)
}

compare <- rep(0, m)
for (i in 1:m) {
  compare[i] <- ifelse(b_n[i] == s_n[i], 1, 0)
  sum.compare <- sum(compare)
}
# VECTORIZATION
#A vectorized function is a function that will apply the same operation on each 
#of the vectors.

sapply(t, ber.function)
#sapply returns a vector; it performed the ber.function to every element in t.

#Other functionals are apply, lapply, tapply, mapply, vapply, and replicate


#The return() function specifies the output that the function should return 
#after performing its task.

#create for loop for data set:

patients <- data.frame(
  weight = c(70, 80, 60, 90, 75), # in kg
  height = c(1.75, 1.80, 1.60, 1.85, 1.78) # in meters
)

#create for loop to obtian BMI: w/h^2 for each patient

for (i in 1:nrow(patients)) {
  BMI <- patients$weight[i]/(patients$height[i]^2)
    if(BMI <18.5){category <- "underweight"}
    else if(BMI>= 18.5 & BMI <= 24.9){category <- "normal"}
    else{category <- "overweight"}
  print(paste("Patient", i, "has BMI score", round(BMI, 2), "and is", category))
}

n <- nrow(patients)
ID <- rep(NA, n)
BMI <- rep(NA, n)
category <- rep(NA, n)

for(i in 1:nrow(patients)) {
  ID[i] <- i
  BMI[i] <- patients$weight[i]/(patients$height[i]^2)
  category[i] <- ifelse(BMI[i] > 24.9, "overweight", ifelse(BMI[i] < 18.5, "underweight", 
                                                   "normal"))
  print(paste("Patient", ID[i], "has BMI score", round(BMI[i], 2), "and is", category[i]))
}

data <- data.frame(cbind(ID, patients, BMI, category))

qf(.95, 35, 29)

