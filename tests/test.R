### Load "mars" package
library(mars)
set.seed(2026)

# Example 1:

m <- mars(y~., marstestdata,control = list(Mmax = 2L, d = 3, trace = FALSE))
summary(m)
print(m)
predict(m, newdata = marstestdata)
anova(m)
plot(m)

#Example 2: Spotify data, predicting popularity of songs
#data from: https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset

fit <- mars(popularity ~. , spotify, control = mars.control(Mmax=4))
summary(fit)
anova(fit)

# Example 3: Airbnb prediction on rental price
## dataset from: https://www.kaggle.com/datasets/dgomonov/new-york-city-airbnb-open-data/

airbnb <- na.omit(airbnb)

fit <- mars(price ~. , airbnb, control = mars.control(Mmax=4))
summary(fit)
anova(fit)
plot(fit)


# Example 4: Pima Indians Diabetes (Pima) dataset, predict chance of diabetes for women
# data from: https://machinelearningmastery.com/standard-machine-learning-datasets-for-imbalanced-classification/

fit <- mars(X0.627 ~. , diabetes, control = mars.control(Mmax=4))
summary(fit)
predict(fit, newdata = diabetes)
anova(fit)
plot(fit)


