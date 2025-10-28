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

y <- factor(rbinom(100, 1, 0.3))
x <- rnorm(100, ifelse(y == 0, 0, 0.75))

#above we are creating a random dataset. the y object is a randomly assigned
#series of 1s and 0s. The x object is a randomly assigned two sample data set
#where the mean is defined by the ifelse statement. It is easy to imagine
#aslo assigning the sd in this way, or creating more conditions.

xy.random <- data.frame(cbind(y, x))
y
print(xy.random$y)
#WTF!!!

mean(x)

#while loop examples

arrivals <- c()
time <- 0
next_arrival <- rexp(1, rate = 3)

while(time+next_arrival <= 10){
  # Update list of arrivals and current time
  arrivals <- c(arrivals, next_arrival)
  time <- time + next_arrival

  # Generate the next arrival
  next_arrival <- rexp(1, rate = 3)
}

arrivals


#create loop: 2.3^x >= 100, find smallest integer x

test <- function(x){2.3^x}
i <- 1

while(test(i)<=100){
  i <- i+1
  value <- test(i)
  print(value)
}

numbers <- c(-10, 0, 5, -3, 8)

for (i in numbers) {
  ifelse(i > 0, print(paste(i, "is positive")), 
         ifelse(i <0, print(paste(i, "is negative")), 
              print(paste(i, "is zero"))))
}

#debugging code: use the browser() function OR place the options()
#function in the console: options(error=recovery), to turn off
#change options(error=NULL). HIT 0 to exit "options" mode.


#random walk while loop!!
x <- 0
n <- 0
data.n <- c()
data.x <- c()
while (n < 4) {
  x <- runif(1, min =-1, max=1)
  n <- n + x
  data.n <- c(data.n, n)
  data.x <- c(data.x, x)
}
length(data.x)

head(data.x, n=10)
head(data.n, n=10)
