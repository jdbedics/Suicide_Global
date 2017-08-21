# Based on Wickham workshop
library(tidyverse)

# clear up console
update.packages(ask=FALSE)

# get data
setwd("~/CLU Google Drive/My Documents/Statistics Instruction/Suicide_Global/data")

global <- read_csv("suicide-rates-by-country.csv")
length(unique(global$Entity))
#summary(global) # min is 1950
global <- global %>% mutate(year1950 = Year - 1950) %>%
  rename(country = Entity, code = `Country code`, rate = `suicide rate`)
global

#----------------------# nesting data -----------------------
by_country <- global %>%
  group_by(country) %>%
  nest() # we now have one row per country
by_country

# Explore countries
names(by_country)
head(global, n=20) # country 1 is afghanistan
head(by_country) # coutnry 1 is still afghanistan
by_country$data[[1]] # look at country 1
by_country$country #usa is #185
by_country$data[[185]] # look at USA

#------------------ fit models -------------------------------
#library(dplyr)
#library(purrr)
str(global)
# create linear models by country
country_model <- function(df) {
  lm(rate ~ year1950, data=df)
}

#create a list
models <- by_country %>%
  mutate(
    model = map(data, country_model)
  )
models # created a list of linear models


# Explore models
models %>% filter(country == "Japan")

# use the broom package
#library(broom)
models1 <- models %>%
  mutate(
    tidy = model %>% map(broom::tidy),
    glance = model %>% map(broom::glance),
    augment = model %>% map(broom::augment),
    rsq = glance %>% map_dbl("r.squared")
  )
models1

models2 %>% arrange(desc(rsq))


# visualization
models1 %>% ggplot(aes(rsq, reorder(country, rsq))) +
  geom_point()

unnest(models2, data) # back to original
unnest(models, glance, .drop=TRUE) %>% View()
unnest(models, tidy) # coefficients from the model

# practice graph
models1 %>%
  unnest(tidy) %>%
  select(country, term, estimate, rsq) %>%
  spread(term, estimate) %>% setNames(c("country","rsq","intercept","Year")) %>%
  ggplot(aes(intercept, Year)) +
  geom_point(aes(colour = country, size = rsq)) +
  geom_smooth(se=FALSE) +
  xlab("Life Expectancy in 1950 (Intercept)") +
  ylab("Yearly Improvement") +
  scale_size_area()
# small rsq show that a liner model is not the best to explain the data
# showing us that coutnries that were worst off, improved the most - there's no data in top right...
# worse countries are catching up to best counties - mostly asia - africa started low and still little change

# augmented data
unnest(models1, augment)

models %>%
  unnest(augment) %>%
  ggplot(aes(year1950, .resid)) +
  geom_line(aes(group=country), alpha=1/3) +
  geom_smooth(se=FALSE) +
  geom_hline(yintercept=0, colour= "white") +
  facet_wrap(~continent)
# end -----------------------------------


# earlier code (extra)
gapminder %>%
  group_by(continent, country) %>%
  nest() %>%
  mutate(
    mod = data %>% map(country_model)
  )

# seq_along - Example
means <- vector("double", ncol(mtcars))
for(i in seq_along(mtcars)) {
  means[[i]] <- mean(mtcars[[i]], na.rm=TRUE)
}

medians <- vector("double", ncol(mtcars))
for(i in seq_along(mtcars)) {
  medians[[i]] <- median(mtcars[[i]], na.rm=TRUE)
}

# library(purr) - functional code
means1 <- map_dbl(mtcars, mean)
medians1 <- map_dbl(mtcars, median)
means
means1
medians
medians1

mtcars %>% map(sum)
# * vector
mtcars %>% map_dbl(mean)


# Reed College Youtube --------------------------------------
library(rvest) # for read_html
library(purrr)
library(readr)
library(dplyr)
library(lubridate)
?readr
read_html("https://massshootingtracker.org/data") %>%
  html_nodes("a[href^='https://docs.goo']") %>%
  html_attr("href") %>%
  map_df(read_csv) %>%
  mutate(date = mdy(date)) ->
  shootings
names(shootings)
View(shootings)























