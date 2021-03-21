## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(knitr.duplicate.label = "allow")
knitr::purl("./tidy_overview.Rmd", output = "R/tidy_overview.R")


## ----echo=FALSE, out.height="95%", out.width="95%"----------------------------
knitr::include_graphics('images/tidyverse-logo.png')


## ----out.width="100%", echo = FALSE-------------------------------------------
knitr::include_graphics("images/data-science.png")


## ----out.width="100%", echo = FALSE-------------------------------------------
knitr::include_graphics("images/data-science.png")

## ----echo = FALSE, out.width="100%"-------------------------------------------
knitr::include_graphics('images/kitchen.jpg')


## -----------------------------------------------------------------------------
print('R code looks like this.')
print('Console output has two pound symbols in front.')
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


## ----echo = FALSE, out.width="75%"--------------------------------------------
knitr::include_graphics('images/top.jpg')


## ----echo = FALSE, out.width="75%"--------------------------------------------
knitr::include_graphics('images/a83830605363b094e15f5dfb6bfa7862.jpg')


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

input %>% 
  boil() %>% 
  mash() %>% 
  stick(where = 'stew')


## ----eval = FALSE-------------------------------------------------------------
## input %>%
##   boil() %>%
##   mash() %>%
##   stick(where = 'stew')


## -----------------------------------------------------------------------------
diabetes


## ----message = FALSE----------------------------------------------------------
data_path = "data/diabetes.tsv"
diabetes = read_tsv(data_path)

## ----echo = FALSE, out.width="90%"--------------------------------------------
knitr::include_graphics('images/readr.png')


## ----echo = FALSE, eval = FALSE-----------------------------------------------
## # this is how you prepare the diabetes dataset
## library(MASS)
## diabetes = rbind(Pima.tr, Pima.te) %>%
##   as_tibble %>%
##   dplyr::rename(diabetic = type)
## write_tsv(diabetes, 'C:/Users/aghazi/tidy_overview/data/diabetes.tsv')
## write_tsv(diabetes, 'data/diabetes.tsv')
## # This is rbind(Pima.tr, Pima.te) using the two datasets from the R package MASS (not a tidyverse package)


## ----echo=FALSE, out.height="95%", out.width="95%"----------------------------
knitr::include_graphics('images/dplyr_logo.png')


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
diabetes %>% arrange(age)


## ----eval = FALSE-------------------------------------------------------------
## input_df %>%
##   mutate(col_name = col_values)


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(birth_year = 2021 - age)


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(is_mother = npreg > 0) %>% 
  group_by(is_mother)


## -----------------------------------------------------------------------------
diabetes %>% 
  mutate(is_mother = npreg > 0) %>% 
  group_by(is_mother) %>% 
  summarise(mean_age = mean(age),
            prop_diabetic = sum(diabetic == "Yes")/ n())


## ----echo = FALSE, out.width = "90%"------------------------------------------
knitr::include_graphics("images/ggplot2.png")


## ----fig.height=3.7-----------------------------------------------------------
diabetes %>% 
  ggplot(aes(x = bmi, y = glu)) + 
  geom_point()


## ----fig.height=3.7-----------------------------------------------------------
diabetes %>% 
  mutate(is_mother = npreg > 0) %>% 
  ggplot(aes(x = is_mother, y = age)) + 
  geom_boxplot()


## ----echo = FALSE, out.height='100%', fig.align='center'----------------------
knitr::include_graphics('images/geoms.png')


## ----echo = FALSE, out.width="90%"--------------------------------------------
knitr::include_graphics("images/rmd.png")


## ----cache=TRUE---------------------------------------------------------------
nyt_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
state_covid = read_csv(nyt_url)


## -----------------------------------------------------------------------------
state_covid


## ----eval = FALSE-------------------------------------------------------------
## state_covid %>%
##   filter(state == "Washington") %>%
##   mutate(new_deaths = c(NA, diff(deaths))) %>%
##   ggplot(aes(date, new_deaths)) +
##   geom_point() +
##   geom_smooth(method = "loess", span = .2) +
##   labs(title = "Daily new deaths in Washington",
##        y = "New Deaths",
##        caption = "Data from the New York Times: https://github.com/nytimes/covid-19-data") +
##   theme_bw()


## ----echo = FALSE, message = FALSE, warning = FALSE, out.width="90%"----------
state_covid %>% 
  filter(state == "Washington") %>% 
  mutate(new_deaths = c(NA, diff(deaths))) %>% 
  ggplot(aes(date, new_deaths)) +
  geom_point() + 
  geom_smooth(method = "loess", span = .2) + 
  labs(title = "Daily new deaths in Washington",
       y = "New Deaths",
       caption = "Data from the New York Times: https://github.com/nytimes/covid-19-data") + 
  theme_bw()


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

