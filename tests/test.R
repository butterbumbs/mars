### Load "mars" package
library("mars")

# Example 1:

set.seed(2026)
x1 <- 1:100
x2 <- rnorm(100)
x3 <- rnorm(100)
y <- pmax(0, x1 - 0.5)*pmax(0, x2 - 0.5)  + rnorm(100)
data <- data.frame(y,x1,x2,x3)
fit <- mars(y ~ ., data = data, control = mars.control(Mmax=4))
print(fit)
summary(fit)

newdata <- data[,-1] # drop response y
predict(fit,newdata)

anova(fit)

plot(fit)

# Example 2: Economic data
# Using ISLR data to find the best predictor of wage

Wage <- ISLR::Wage[,-10] # drop logwage or replace wage with logwage as the response

fit <- mars(wage ~. , Wage, control = mars.control(Mmax=6))
summary(fit)

anova(fit)

# Example 3: Housing data
# Using MASS data of housing prices

boston <- MASS::Boston
fit <- mars(medv ~ ., boston, control = mars.control(Mmax=6))
summary(fit)
print(fit)

plot(fit)
