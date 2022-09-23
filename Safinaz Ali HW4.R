#Safinaz Ali
#Homework 4
#Team: Adeel Arshid & Victoria kKaradimas


dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 19) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

is.na(HHINCOME) <- which(HHINCOME == 9999999)
norm_FAMSIZE <- norm_varb(FAMSIZE)
norm_HHINCOME <- norm_varb(HHINCOME)
norm_ROOMS <- norm_varb(ROOMS)
norm_WATER <- norm_varb(COSTWATR)

# we are comparing household income to family size because we assume the more income you have, the more amount of family members that live with you.
#which increase the amount of rooms. We know that there aren't many heavy number bedroom apartments in urban areas like Manhattan.
#We assume that people with high bills for water will most likely being living in a home since apartments don't have to pay for water meaning they will be located in the rural areas like Staten Island. 

summary(norm_HHINCOME)
summary(norm_FAMSIZE)
summary(norm_ROOMS)
summary(norm_WATER)
xtabs(~ROOMS + FAMSIZE + borough_f)
xtabs(~COSTWATR + borough_f)

data_use_prelim <- cbind(norm_HHINCOME, norm_FAMSIZE, norm_ROOMS,norm_WATER)
data_use_prelim <- data.frame(data_use_prelim)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
borough0 <- complete.cases(data_use_prelim, borough_f)


set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)

model_ols1 <- lm(cl_data_n ~ train_data$norm_HHINCOME + train_data$norm_FAMSIZE + train_data$norm_ROOMS + train_data$norm_WATER)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_HHINCOME + train_data$norm_FAMSIZE + train_data$norm_ROOMS + train_data$norm_WATER)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

#With the results we learned that majority of people reside in Brooklyn who have only 2 people living in a 4 bedroom. 
#However when we look at the Manhattan data majority of people are residing in studio apartments or max a 3 bedroom apartment with only 1 person. 
#For the water cost i was able to see that majority of the Manhattan (537 people) pays $0 for water since many of them are in apartment. 
#However what was surprising was that Brooklyn is the borough that spends the most on the water bill (5945 people) with queens being a close second. 
#So my original hypothesis was wrong with assuming Staten Island will be the most people since they have more houses in the area but they were th least with 380 people


