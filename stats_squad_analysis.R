

##------ Food Survey -------##
## 1: Is the mean of weight the same across healthy feeling? 
model_anova = aov(weight~ healthy_feeling_categ, data = food_coded_data)
summary(model_anova)


## 2: Is ideal diet the same between genders?
t.test(ideal_diet_coded~gender,data = food_coded_data)



## 3: Is weight the same between how many times per week someone goes out?
model_anova = aov(weight~eating_out, data = food_coded_data)
summary(model_anova)




##------ LeBron James Year 1 -------##
## 1: Can free throws be predicted from field goals?

model_regression = lm(stl~ft, data = lj_season_1)
summary(model_regression)



## 2: Does minutes played predict points scored?
model_regression = lm(pts~mp, data = lj_season_1)
summary(model_regression)



## 3: Does he score more points when he has more steals? 
model_regression = lm(pts~stl, data = lj_season_1)
summary(model_regression)









##------ Birth Weight -------##
## 1: Is the mean of birth weight the same across gender?
t.test(bwt~gender, data = king_county_data)

## 2: Are the mean of the baby's weight the same across all the races?
model_anova = aov(bwt~race, data = king_county_data)
summary(model_anova)

## 3: Does the gestation period predict birth weight?
model_regression = lm(bwt~gest, data = king_county_data)
summary(model_regression)







##------ Michael Jordan Year 1 -------##
## 1: Does rebounds predict points?

model_regression = lm(pts~trb, data = mj_season_1)
summary(model_regression)



## 2: Does minutes played predict points scored?
model_regression = lm(pts~mp, data = mj_season_1)
summary(model_regression)



## 3: Does he score more points when he has more steals? 
model_regression = lm(pts~stl, data = mj_season_1)
summary(model_regression)



##------ Titanic -------##
## 1: Is there an association between individual's sex and if someone survived?
chi_table = table(titanic_data[,c("sex","survived")])
chisq.test(chi_table)


## 2: Are the means the same for those who survived and the average fare?
t.test(fare~survived,data = titanic_data)


## 3: Are the mean ages the same whether or not someone survived?
t.test(age~survived,data = titanic_data)




##------ Kobe Byrant Year 3 -------##
## 1: Can field goals predict 3 point percentage?

model_regression = lm(x3p_percent~fg, data = kb_season_1)
summary(model_regression)



## 2: Can the number of steals predict points?
model_regression = lm(pts~stl, data = kb_season_1)
summary(model_regression)



## 3: Can minutes played predict points? 
model_regression = lm(pts~mp, data = kb_season_1)
summary(model_regression)




  
