# install packages

install.packages("MASS")
install.packages("plyr")
install.packages("reshape2")
install.packages("ggplot2")
library("MASS")
library("plyr")
library("reshape2")
library("ggplot2")

# Set a seed for repeatable plots
set.seed(12345)

# Calculates the covariance matrix sigma using a
# simplified version of the squared exponential function.

# Parameters:
#	X1, X2 = vectors
# 	l = the scale length parameter
# Returns:
# 	a covariance matrix
calcSigma <- function(X1,X2,l=1) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }
  return(Sigma)
}

# 1. Plot some sample functions from the Gaussian process

# Define the points at which we want to define the functions
x.star <- seq(-5,5,len=50)

# Calculate the covariance matrix
sigma <- calcSigma(x.star,x.star)

# Generate a number of functions from the process
n.samples <- 3
values <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  # Each column represents a sample from a multivariate normal distribution
  # with zero mean and covariance sigma
  values[,i] <- mvrnorm(1, rep(0, length(x.star)), sigma)
}
values <- cbind(x=x.star,as.data.frame(values))
values <- melt(values,id="x")

# Plot the result
fig1 <- ggplot(values,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=variable)) +
  theme_bw() +
  scale_y_continuous(lim=c(-2.5,2.5), name="output, f(x)") +
  xlab("input, x")

fig1

# 2. Now let's assume that we have some known data points

f <- data.frame(x=c(-4,-3,-1,0,2),
                y=c(-2,0,1,2,-1))

# Calculate the covariance matrices
# using the same x.star values as above
x <- f$x
k.xx <- calcSigma(x,x)
k.xxs <- calcSigma(x,x.star)
k.xsx <- calcSigma(x.star,x)
k.xsxs <- calcSigma(x.star,x.star)

f.star.bar <- k.xsx%*%solve(k.xx)%*%f$y
cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx)%*%k.xxs

# This time we'll plot more samples.  
n.samples <- 50
values <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  values[,i] <- mvrnorm(1, f.star.bar, cov.f.star)
}
values <- cbind(x=x.star,as.data.frame(values))
values <- melt(values,id="x")

# Plot the results including the mean function
# and constraining data points
fig2 <- ggplot(values,aes(x=x,y=value)) +
  geom_line(aes(group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=x.star,y=f.star.bar),colour="red", size=1) + 
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")

fig2

# 3. Now assume that each of the observed data points have some
# normally-distributed noise.

# The standard deviation of the noise
sigma.n <- 0.1

# Recalculate the mean and covariance functions
f.bar.star <- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%f$y
cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%k.xxs

# Recalulate the sample functions
values <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  values[,i] <- mvrnorm(1, f.bar.star, cov.f.star)
}
values <- cbind(x=x.star,as.data.frame(values))
values <- melt(values,id="x")

# Plot the result, including error bars on the observed points
gg <- ggplot(values, aes(x=x,y=value)) + 
  geom_line(aes(group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=x.star,y=f.bar.star),colour="red", size=1) + 
  geom_errorbar(data=f,aes(x=x,y=NULL,ymin=y-2*sigma.n, ymax=y+2*sigma.n), width=0.2) +
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")

gg
