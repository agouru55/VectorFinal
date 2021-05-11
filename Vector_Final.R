### Info for Ms. Morgan
# You can run this program as is. The matrix allYears will automatically print, which shows the population size of each age class for each year.
# To print the proportion matrix, just print A. 

# The code is written to have the user input reproduction and survival rates, population sizes, and number of years of growth.
# However, if you want to use some values for different types of population growth see below:
 
# *The number of years for each one can be 20+
# Exponential Growth:
# F1 = 0, F2 = 0.13, F3 = 0.42, S1 = 0.6, S2 = 0.75, S3 = 0.95, I1 = 0, I2 = 100, I3 = 100

# Exponential Decay:
# F1 = 0, F2 = 0, F3 = 0.45, S1 = 0.2, S2 = 0.36, S3 = 0.4, I1 = 0, I2 = 100, I3 = 100


library("viridis")

print("Enter values from 0-1 for reproduction and survival rates. Enter 0 if the value is not given. This code can be used for 3 different age classes.")

### Enter fecundity rate
F1 <- readline(prompt = "Reproduction rate of 1st age class: ")
F1 <- as.numeric(F1)

F2 <- readline(prompt = "Reproduction rate of 2nd age class: ")
F2 <- as.numeric(F2)

F3 <- readline(prompt = "Reproduction rate of 3rd age class: ")
F3 <- as.numeric(F3)

### Enter survival rate
S1 <- readline(prompt = "Survival rate of 1st age class: ")
S1 <- as.numeric(S1)

S2 <- readline(prompt = "Survival rate of 2nd age class: ")
S2 <- as.numeric(S2)

S3 <- readline(prompt = "Survival rate of 3rd age class: ")
S3 <- as.numeric(S3)

### Create Leslie/projection matrix
Leslie_matrix <- matrix(c(F1, F2, F3, S1, 0, 0, 0, S2, S3), nrow = 3, ncol = 3, byrow = TRUE)

### Specify initial abundances to create population matrix
I1 <- readline(prompt = "What is the initial size of the 1st age class? ")
I1 <- as.numeric(I1)
I2 <- readline(prompt = "What is the initial size of the 2nd age class? ")
I2 <- as.numeric(I2)
I3 <- readline(prompt = "What is the initial size of the 3rd age class? ")
I3 <- as.numeric(I3)
Initial_abundance <- c(I1, I2, I3)

### Loop n number of years
nYears <- readline(prompt = "Number of years: ")
nYears <- as.numeric(nYears)

allYears <- matrix(0,nrow = nrow(Leslie_matrix),ncol = nYears+1) # build a storage array for all abundances!
allYears[, 1] <- Initial_abundance  # set the year 0 abundance                                    
for(t in 2:(nYears+1)){   # loop through all years
  allYears[,t] <-  Leslie_matrix %*% allYears[, t-1]
}

print(allYears)

### Print the eigenvalue
M1 = allYears[, t, drop = FALSE]
M2 = allYears[, t-1, drop = FALSE]
M3 = M1/M2
lambda <- max(M3)
cat("The dominant eigenvalue is: ", lambda, "\n")
if (lambda > 1) {
  print('The population will grow exponentially.')
} else if (lambda < 1) {
  print('The population will decline exponentially.')
} else
print('The population is stable.')

### Find the point where proportions stabilize
q0 = allYears[1, , drop=FALSE]+allYears[2, , drop=FALSE]+allYears[3, , drop=FALSE]
q1 = allYears[1, , drop=FALSE]
q2 = allYears[2, , drop=FALSE]
q3 = allYears[3, , drop=FALSE]
r1 = q1/q0
r2 = q2/q0
r3 = q3/q0
R = rbind(r1, r2, r3)
A <- round(R, digits = 4) # this is the proportion matrix - print if you want
LR = A[, t, drop = FALSE]
SP <- min(which(sapply(1:ncol(A), function(i) all(LR==A[,i]))))
cat("The proportions stabilize at: ", SP, "years.\n")
cat("These proportions are:", A[1, nYears+1], "for the first age class,", A[2, nYears+1], "for the second age class, and", A[3,nYears+1], "for the third age class.")

### Plot
plot(1, 1, pch = 0, ylim = c(0, max(allYears)), xlim = c(0, nYears+1), xlab = "Years", ylab = "Population Abundance", main = "Population Growth of a Species", xaxt = "n")
cols <- viridis(ncol(Leslie_matrix))
for(s in 1:ncol(Leslie_matrix)){
  points(allYears[s,],col=cols[s],type="l",lwd=2) # plot out each life stage abundance, one at a time

}

### Plot Vanity
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
legend("topleft",col=cols,lwd=rep(2,ncol(Leslie_matrix)), legend=paste("Stage ",seq(1:ncol(Leslie_matrix))))  # put a legend on the plot


