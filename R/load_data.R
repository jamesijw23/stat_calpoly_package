

setwd("C:/Users/james/OneDrive/Documents/Important_Files/Cal_Poly/Class_Material/STAT_252/winter_2019/grades/final_exhibition/data_sets")

air_quality_data = read.csv('air_quality_data.csv')
apple_stock_data = read.csv("apple_stock_data.csv")
completion_cleaning_data = read.csv("completion_cleaning_data.csv")
food_coded_data = read.csv("food_coded_data.csv")
gas_data = read.csv("gas_data.csv")
happiness_2017_data = read.csv("happiness_2017_data.csv")
king_county_data = read.csv("king_county_data.csv")
mj_season_1 = read.csv("mj_season_1.csv")
mj_shoe_data = read.csv("mj_shoe_data.csv")
nba_data = read.csv("nba_data.csv")
nfl_data = read.csv("nfl_data.csv")
personalized_music_taste_data = read.csv("personalized_music_taste_data.csv")
salaries_by_college_type_data = read.csv("salaries_by_college_type_data.csv")
spotify_data = read.csv("spotify_data.csv")
students_performance_data = read.csv("students_performance_data.csv")
titanic_data = read.csv("titanic_data.csv")



setwd("C:/Users/james/OneDrive/Documents/Important_Files/Cal_Poly/Class_Material/STAT_252/stat252_notes/rcode_and_data")
ucla_data =  read.csv('ucla_data.csv')


usethis::use_data(
  air_quality_data,
  apple_stock_data,
  completion_cleaning_data,
  food_coded_data,
  gas_data,
  happiness_2017_data,
  king_county_data,
  mj_season_1,
  mj_shoe_data,
  nba_data,
  nfl_data,
  personalized_music_taste_data,
  salaries_by_college_type_data,
  spotify_data,
  students_performance_data,
  titanic_data,
  ucla_data)
