library(tidyverse)
library(statcalpolypackage)


# usethis::use_data(ucla_data,gas_data,food_coded_data,overwrite = T)
## Update gas_data, ucla_data, food_coded_data, kb_season_3, sc_season_3
gas_data = gas_data %>%
  mutate(mpg =numbers_of_miles/gallons )

ucla_data = ucla_data %>%
  mutate(University_Rating = case_when(
    University_Rating == 1 ~ "A",
    University_Rating == 2 ~ "B",
    University_Rating == 3 ~ "C",
    University_Rating == 4 ~ "D",
    University_Rating == 5 ~ "E"
  ))

king_county_data = king_county_data %>%
  mutate(education_year_categ = case_when(
    education_year > 16 ~ "More than Bachelors",
    education_year == 16 ~ "Bachelors",
    education_year> 12 & education_year < 16 ~ "Associates",
    education_year == 12 ~ "High School Diploma",
    education_year < 12 ~ "Less than High School"
  ))

lj1 = nba %>%
  filter(Name =="LJ", Season == "season_1")
lj1$double_double = as.factor(lj1$double_double)
lj1$triple_double = as.factor(lj1$triple_double)


food_coded_data = food_coded_data %>% 
  mutate (healthy_feeling_categ = case_when(
    healthy_feeling <= 4 ~ "Low",
    healthy_feeling > 4 & healthy_feeling <= 7 ~ "Medium",
    healthy_feeling > 7 ~ "High"
  ) )


##------ Gas Data -------##
## 1: Are average miles per gallon the same for the city and on the highway? *c*
t.test(mpg~city_highway,data=gas_data)

## 2: Are average miles per gallon the same across gas type? *c* 
model_anova = aov(mpg~gas_type, data = gas_data)
summary(model_anova)

## 3: Are the average number of miles traveled the same across each season? 
model_anova = aov(numbers_of_miles~season, data = gas_data)
summary(model_anova)



##------ Birth Weight -------##
## 1: Are the means the same for education level and mother's weight gain? *c*
model_anova = aov(gain_weight~education_year_categ, data = king_county_data)
summary(model_anova)

## 2: Is there an association between the gender of
## the baby and the amount of weight gained by the mother? ** rephrase **
t.test(gain_weight~gender,data=king_county_data)

## 3: Are the average birth weight across race the same?
model_anova = aov(bwt~race, data = king_county_data)
summary(model_anova)



##------ LeBron James Year 1 -------##
## 1: Can free throws be predicted from field goals?
t.test(FG~double_double , data = lj1)
model_regression = lm(ft~fg, data = lj_season_1)
summary(model_regression)



## 2: Can free throws predict points? 
model_regression = lm(pts~ft, data = lj_season_1)
summary(model_regression)



## 3: Do three point attempts predict assists?
model_regression = lm(ast~x3pa, data = lj_season_1)
summary(model_regression)


##------ Steph Curry  Year 3 (double check) -------##
## 1: Can the number of 3-point attempts predict the number of assists?
model_regression = lm(ast~x3pa, data = sc_season_3)
summary(model_regression)


## 2: Can the number of steals predict the number of points?
model_regression = lm(pts~stl, data = sc_season_3)
summary(model_regression)



## 3: Can the number of minutes played predict the number of free throws made?
model_regression = lm(ft~mp, data = sc_season_3)
summary(model_regression)


##------ Kobe Year 1 -------##
## 1: Does minutes played predict field goal percentage?
model_regression = lm(fg~mp, data = kb_season_1)
summary(model_regression)


## 2: Does free throw attempts predict Kobe's points for the game?
model_regression = lm(pts~fta, data = kb_season_1)
summary(model_regression)


## 3: Does field goal attempts predict field goal percentage?
model_regression = lm(fg_percent~fta, data = kb_season_1)
summary(model_regression)




##------ Food Survey -------##
## 1: Is the mean of weight the same across healthy feeling? 
model_anova = aov(weight~ healthy_feeling_categ, data = food_coded_data)
summary(model_anova)


## 2: Does employment status have an association with eating out or eating at home?
chi_table = table(food_coded_data[,c("employment","eating_out")])
chisq.test(chi_table)


## 3: Does an individual's weight have any association with an individual's GPA?
model_regression = lm(gpa~weight, data = food_coded_data)
summary(model_regression)
