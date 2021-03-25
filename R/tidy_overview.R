## -----------------------------------------------------------------------------
print("Console output has two pound symbols in front.")
x = 5 


## -----------------------------------------------------------------------------
library(tidyverse)


## ----message = FALSE, echo = FALSE--------------------------------------------
data_path = "data/diabetes.tsv"
diabetes = read_tsv(data_path)

## -----------------------------------------------------------------------------
diabetes


## ----echo = FALSE-------------------------------------------------------------
diabetes


## -----------------------------------------------------------------------------
c(1,2,3,4) %>% mean






## ----echo = FALSE-------------------------------------------------------------
boil = function(x) paste('boiled', x)
mash = function(x) paste('mashed,', x)
stick = function(x, where) paste(where, 'ed, ', x, sep = '')


## -----------------------------------------------------------------------------
input = "potatoes"

stick(mash(boil(input)), where = 'stew')


## -----------------------------------------------------------------------------
input = "potatoes"

stick(mash(boil(input)), where = 'stew')

input %>% boil() %>% mash() %>% stick(where = 'stew')


## ----eval = FALSE-------------------------------------------------------------
## input %>%
##   boil() %>%
##   mash() %>%
##   stick(where = 'stew')


## ----message = FALSE----------------------------------------------------------
data_path = "data/diabetes.tsv"
diabetes = read_tsv(data_path)



## ----echo = FALSE, eval = FALSE-----------------------------------------------
## # this is how you prepare the diabetes dataset
## library(MASS)
## diabetes = rbind(Pima.tr, Pima.te) %>%
##   as_tibble %>%
##   dplyr::rename(diabetic = type)
## write_tsv(diabetes, 'C:/Users/aghazi/tidy_overview/data/diabetes.tsv')
## write_tsv(diabetes, 'data/diabetes.tsv')
## # This is rbind(Pima.tr, Pima.te) using the two datasets from the R package MASS (not a tidyverse package)




## -----------------------------------------------------------------------------
diabetes %>% 
  filter(npreg == 0)


## -----------------------------------------------------------------------------
diabetes %>% 
  filter(bmi > 30)


## -----------------------------------------------------------------------------
diabetes %>% 
  select(diabetic, npreg, age)


## -----------------------------------------------------------------------------
diabetes %>% 
  select(8, 1, 7)


## -----------------------------------------------------------------------------
diabetes %>% 
  select(starts_with('b'), matches('diab'))


## -----------------------------------------------------------------------------
diabetes %>% arrange(bmi)


## -----------------------------------------------------------------------------
diabetes %>% arrange(desc(age))


## ----eval = FALSE-------------------------------------------------------------
## input_df %>%
##   mutate(col_name = col_values)


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(index = 1:532)


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(birth_year = 2021 - age)


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(birth_year = 2021 - age,
         is_mother = npreg > 0)


## -----------------------------------------------------------------------------
diabetes %>% 
  group_by(diabetic)


## -----------------------------------------------------------------------------
diabetes %>% 
  group_by(diabetic) %>% 
  summarise(mean_age = mean(age))


## -----------------------------------------------------------------------------
diabetes %>% 
  group_by(diabetic) %>% 
  summarise(mean_age = mean(age),
            group_size = n())


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(is_mother = npreg > 0) %>% 
  group_by(diabetic, is_mother) 


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(is_mother = npreg > 0) %>% 
  group_by(diabetic, is_mother) %>% 
  summarise(mean_age = mean(age))




## ----fig.height=3.2-----------------------------------------------------------
diabetes %>% 
  ggplot(mapping = aes(x = age, y = bp)) + 
  geom_point()


## ----fig.height=3.1-----------------------------------------------------------
diabetes %>% 
  mutate(is_mother = npreg > 0) %>% 
  ggplot(aes(is_mother, age)) + 
  geom_boxplot()


## ----echo = FALSE, out.width='75%', fig.align='center'------------------------
knitr::include_graphics('images/geoms.png')




## ----cache=TRUE---------------------------------------------------------------
nyt_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
state_covid = read_csv(nyt_url)


## -----------------------------------------------------------------------------
state_covid


## ----eval = FALSE-------------------------------------------------------------
## state_covid %>%
##   filter(state == "California") %>%
##   ggplot(aes(date, deaths)) +
##   geom_line()


## ----out.width="50%"----------------------------------------------------------
state_covid %>% 
  filter(state == "California") %>% 
  ggplot(aes(date, deaths)) +
  geom_line()


## -----------------------------------------------------------------------------
state_covid %>% 
  filter(state == "California", deaths > 0) 


## ----eval=FALSE---------------------------------------------------------------
## state_covid %>%
##   filter(state == "California") %>%
##   mutate(new_deaths = diff(deaths))


## ----error=TRUE---------------------------------------------------------------
state_covid %>% 
  filter(state == "California") %>% 
  mutate(new_deaths = diff(deaths))


## -----------------------------------------------------------------------------
state_covid %>% 
  filter(state == "California") %>% 
  mutate(new_deaths = c(deaths[1], diff(deaths))) 


## ----fig.height=3-------------------------------------------------------------
state_covid %>% 
  filter(state == "California") %>% 
  mutate(new_deaths = c(deaths[1], diff(deaths))) %>% 
  ggplot(aes(date, new_deaths)) +
  geom_point() 


## ----eval = FALSE-------------------------------------------------------------
## state_covid %>%
##   filter(state == "California") %>%
##   mutate(new_deaths = c(deaths[1], diff(deaths))) %>%
##   ggplot(aes(date, new_deaths)) +
##   geom_point() +
##   geom_smooth(method = "loess", span = .2) +
##   labs(title = "Daily new deaths in California",
##        y = "New Deaths",
##        caption = "Data from the New York Times: https://github.com/nytimes/covid-19-data") +
##   theme_bw()


## ----echo = FALSE, message = FALSE, warning = FALSE, out.width="90%"----------
state_covid %>% 
  filter(state == "California") %>% 
  mutate(new_deaths = c(deaths[1], diff(deaths))) %>% 
  ggplot(aes(date, new_deaths)) +
  geom_point() + 
  geom_smooth(method = "loess", span = .2) + 
  labs(title = "Daily new deaths in California",
       y = "New Deaths",
       caption = "Data from the New York Times: https://github.com/nytimes/covid-19-data") + 
  theme_bw()


## -----------------------------------------------------------------------------
state_covid %>% 
  group_by(state) %>% 
  mutate(new_deaths = c(deaths[1], diff(deaths))) %>% 
  summarise(state_max = max(new_deaths)) %>% 
  arrange(desc(state_max))


## -----------------------------------------------------------------------------
map(1:3, sqrt)


## -----------------------------------------------------------------------------
map(1:3, sqrt)

## ----eval = FALSE-------------------------------------------------------------
## map(datasets, ml_algorithm)


## -----------------------------------------------------------------------------
map_dbl(1:5, ~.x^2 + 1)
map_lgl(1:5, ~.x == 3)
map_chr(letters[1:5], ~paste0(rep(.x, 3), collapse=''))


## -----------------------------------------------------------------------------
formula_df = names(diabetes)[1:7] %>% 
  combn(m = 2) %>% 
  t %>% 
  as_tibble %>% 
  set_names(c('x1', 'x2')) %>% 
  mutate(formula = paste("diabetic ~ ", x1, " + ", x2, sep = ''))
formula_df


## ----eval = FALSE-------------------------------------------------------------
## formula_df %>%
##   mutate(glm_result = map(formula,
##                           ~glm(.x, data = diabetes, family = 'binomial')))


## ----eval = FALSE-------------------------------------------------------------
## formula_df %>%
##   mutate(glm_result = map(formula,
##                           ~glm(.x, data = diabetes, family = 'binomial')),
##          aic = map_dbl(lm_result,
##                        ~.x$aic))


## ----eval = FALSE-------------------------------------------------------------
## formula_df %>%
##   mutate(glm_result = map(formula,
##                           ~glm(.x, data = diabetes, family = 'binomial')),
##          aic = map_dbl(lm_result,
##                        ~.x$aic)) %>%
##   arrange(aic)


## ----eval = FALSE-------------------------------------------------------------
## ?tidyr::who
## dim(who)
## names(who)
## who[1:5, 1:8]
## who


## -----------------------------------------------------------------------------
who1 = who %>% 
  pivot_longer(new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "values", 
               values_drop_na = TRUE)
who1


## -----------------------------------------------------------------------------
who2 = who1 %>% 
  mutate(key = str_replace(key, "newrel", "new_rel"))
who2


## -----------------------------------------------------------------------------
who3 = who2 %>% 
  separate(key, into = c("newold", "tb_type", "sexage"))


## -----------------------------------------------------------------------------
who4 = who3 %>% 
  separate(sexage, into = c("sex", "age_group"), sep = 1)


## -----------------------------------------------------------------------------
who_final = who4 %>% 
  select(-newold, -matches("iso"))


## -----------------------------------------------------------------------------
tidyr::who %>% 
  pivot_longer(new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "values", 
               values_drop_na = TRUE) %>%
  mutate(key = str_replace(key, "newrel", "new_rel")) %>%
  separate(key, into = c("newold", "tb_type", "sexage")) %>% 
  separate(sexage, into = c("sex", "age_group"), sep = 1) %>% 
  select(-newold, -matches("iso"))

