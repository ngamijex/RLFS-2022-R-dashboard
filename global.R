# few data set manipulations 
labour_data %>%
  str()
labour_data %>%
  summary()
labour_data %>%
  head()

unemployment_rate_by_education_level %>%
  str()

Summary_labour_force_indicators_by_District %>%
  str()
Summary_labour_force_indicators_by_District %>%
  head()
# structure for selection input without states column
c1= Summary_labour_force_indicators_by_District %>%
  select(-"District", -"...1") %>%
  names()
# top 5 provinces  with high  unemployment rates

Summary_labour_force_indicators_by_District %>%
select(District,Unemployment_rate) %>%
  arrange(desc(Unemployment_rate)) %>%
head(5)

# top 5 provinces  with low  un employment rates states 

Summary_labour_force_indicators_by_District %>%
  select(District,Unemployment_rate) %>%
  arrange(Unemployment_rate) %>%
  head(5)


