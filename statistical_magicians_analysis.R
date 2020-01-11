

## 1: Does the number of 3 pointers **?
## 2: Did the number of minutes predici game score?
model_regression = lm(gmsc~mp, data = lj_season_1)
summary(model_regression)
## 3: Does the number if shots taken predict field goal percentage?
model_regression = lm(pts~mp, data = lj_season_1)
summary(model_regression)



##------ Titanic -------##
## 1: Is there an association between individual's sex and if someone survived?
chi_table = table(titanic_data[,c("sex","survived")])
chisq.test(chi_table)

## 2: Are the means the same for those who survived and the average fare?
t.test(fare~survived,data = titanic_data)

## 3: Are the mean ages the same whether or not someone survived?
t.test(age~survived,data = titanic_data)
