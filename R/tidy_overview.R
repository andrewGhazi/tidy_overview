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
tibble(id = c("subj1;ctrl", "subj2;case", "subj3;ctrl"))


## -----------------------------------------------------------------------------
tibble(id = c("subj1;ctrl", "subj2;case", "subj3;ctrl")) %>% 
  separate(id, 
           into = c('subject', 'type'),
           sep = ";")


## -----------------------------------------------------------------------------
tibble(subject = c("subj1", "subj2", "subj3"),
       type = c("ctrl", "case", "ctrl")) 


## -----------------------------------------------------------------------------
tibble(subject = c("subj1", "subj2", "subj3"),
       type = c("ctrl", "case", "ctrl"))  %>% 
  unite(col = "id",
        subject, type,
        sep = ";")






## ----eval = FALSE-------------------------------------------------------------
## wide %>%
##   pivot_longer(cols = x:z,
##                names_to = "key",
##                values_to = "val")


## ----eval = FALSE-------------------------------------------------------------
## long %>%
##   pivot_wider(names_from = key,
##               values_from = val)


## -----------------------------------------------------------------------------
dim(who)
who[1:4, 1:7]


## -----------------------------------------------------------------------------
names(who)


## -----------------------------------------------------------------------------
who1 = who %>% 
  pivot_longer(cols = new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "tb_cases", 
               values_drop_na = TRUE)
who1


## -----------------------------------------------------------------------------
who2 = who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2


## -----------------------------------------------------------------------------
who3 = who2 %>% 
  separate(key, into = c("case_type", "tb_type", "sex_age"), sep = "_")
who3


## -----------------------------------------------------------------------------
who4 = who3 %>% 
  separate(sex_age, into = c("sex", "age_group"), sep = 1)
who4


## -----------------------------------------------------------------------------
who_final = who4 %>% 
  select(-case_type, -matches("iso"))
who_final


## -----------------------------------------------------------------------------
tidyr::who %>% 
  pivot_longer(new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "tb_cases", 
               values_drop_na = TRUE) %>%
  mutate(key = str_replace(key, "newrel", "new_rel")) %>%
  separate(key, into = c("case_type", "tb_type", "sex_age")) %>% 
  separate(sex_age, into = c("sex", "age_group"), sep = 1) %>% 
  select(-case_type, -matches("iso"))


## -----------------------------------------------------------------------------
sum_of_squares = function(x, y) {
  x^2 + y^2
}

sum_of_squares(3, 4)


## -----------------------------------------------------------------------------
tibble(random_samples = list(rnorm(20), rnorm(20)))


## -----------------------------------------------------------------------------
tibble(random_samples = list(rnorm(20), rnorm(20)),
       really_anything = list(diabetes, 
                              glm((diabetic == "Yes") ~ age + bmi, 
                                  data = diabetes, 
                                  family = 'binomial')))




## -----------------------------------------------------------------------------
map(1:3, sqrt)


## ----eval = FALSE-------------------------------------------------------------
## map(1:3, sqrt) # sqrt(1:3) is better in reality


## ----eval = FALSE-------------------------------------------------------------
## read_and_tidy = function(file_name) {...}
## run_ml_algorithm = function(tidy_df) {...}
## 
## tidy_datasets = map(file_names, read_and_tidy)
## map(tidy_datasets, run_ml_algorithm)


## -----------------------------------------------------------------------------
map(1:3, ~sqrt(.x) + 5)


## -----------------------------------------------------------------------------
map_dbl(1:5, ~sqrt(.x) + 1)
map_lgl(1:5, ~.x == 3)
map_chr(letters[1:5], ~paste(rep(.x, 3), collapse=''))


## ----warn = FALSE, message = FALSE--------------------------------------------
colnames(diabetes)[1:7]

colnames(diabetes)[1:7] %>% 
  combn(m = 2, simplify = FALSE)


## ----warn = FALSE, message = FALSE--------------------------------------------
diabetes = diabetes %>% mutate(is_diabetic = diabetic == "Yes")

formula_df = colnames(diabetes)[1:7] %>% 
  combn(m = 2, simplify = FALSE) %>% 
  map_chr(~paste("is_diabetic ~ ", .x[1], " + ", .x[2], sep = "")) %>%
  tibble(formula = .)
formula_df


## -----------------------------------------------------------------------------
formula_df %>% 
  mutate(glm_result = map(formula,
                          ~glm(as.formula(.x), data = diabetes, family = 'binomial'))) 


## -----------------------------------------------------------------------------
formula_df %>% 
  mutate(glm_result = map(formula,
                          ~glm(.x, data = diabetes, family = 'binomial')),
         aic = map_dbl(glm_result,
                       ~.x$aic)) 


## -----------------------------------------------------------------------------
formula_df %>% 
  mutate(glm_result = map(formula,
                          ~glm(.x, data = diabetes, family = 'binomial')),
         aic = map_dbl(glm_result,
                       ~.x$aic)) %>% 
  arrange(aic)




## -----------------------------------------------------------------------------
band_members
band_instruments


## -----------------------------------------------------------------------------
band_members %>% 
  inner_join(band_instruments, by = "name")




## -----------------------------------------------------------------------------
band_members %>% full_join(band_instruments, by = 'name')

