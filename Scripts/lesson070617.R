library(tidyverse)
x<-c(30,31,NA,NA,44)
x[0]
x[7]

z<-1:10
x<-1:5
y<-z^x

w <- rnorm(10)
w[w<0]
w[which(w<0)]

mod <- lm(lifeExp~gdpPercap, data=gapminder_plus) #select df.residual
mod$df.residual
mod["df.residual"]
mod$qr$qr[1]

gapminder_plus %>% 
  group_by(continent) %>% 
  summarise(mean_lifeExp=mean(lifeExp),
            min_le = min(lifeExp),
            max_le = max(lifeExp))

gapminder_plus %>% 
  ggplot()+
  geom_line(mapping = aes(x=year, y=lifeExp, color=continent, group=country))+
  geom_smooth(mapping = aes(x=year, y=lifeExp), method = "lm", color="black")+
  facet_wrap(~ continent)

by_country <- gapminder_plus %>% group_by(continent, country) %>% 
  nest()

by_country$data[[1]]

#map(list, function) same as apply

model_by_country <- by_country %>% 
  mutate(model=map(data, ~lm(lifeExp~year, data=.x))) %>% 
  mutate(summr=map(model, broom::glance)) %>%  #glance extracts useful data from models
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  ggplot()+
  geom_jitter(mapping = aes(x=continent, y=r.squared))

by_country %>% 
  mutate(model=map(data, ~lm(lifeExp~year, data=.x))) %>% 
  mutate(summr=map(model, broom::glance)) %>%  #glance extracts useful data from models
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  ggplot()+
  geom_jitter(mapping = aes(x=continent, y=r.squared))

by_country %>% 
  mutate(model=map(data, ~lm(lifeExp~year, data=.x))) %>% 
  mutate(summr=map(model, broom::glance)) %>%  #glance extracts useful data from models
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  filter(r.squared<0.6) %>% 
  select(country) %>% 
  left_join(gapminder_plus) %>% 
  
  ggplot()+
    geom_line(mapping = aes(x=year, y=lifeExp, color=continent, group=country))+
    facet_wrap(~ continent)

by_country %>% 
  mutate(model=map(data, ~lm(lifeExp~year, data=.x))) %>% #adding a model 
  mutate(summr=map(model, broom::glance)) %>%  #glance extracts useful summaries from models
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  filter(r.squared<0.5) %>% 
  select(country) %>% 
  left_join(gapminder_plus) %>% 
  ggplot()+
  geom_line(mapping = aes(x=year, y=lifeExp, color=country, group=country))
  
##life expectancy over gdp

by_country %>% 
  mutate(model=map(data, ~lm(lifeExp~log(gdpPercap), data=.x))) %>% #adding a model 
  mutate(summr=map(model, broom::glance)) %>%  #glance extracts useful summaries from models
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  filter(r.squared<0.3) %>% 
  select(country) %>% 
  left_join(gapminder_plus) %>% 
  ggplot()+
  geom_point(mapping = aes(x=log(gdpPercap), y=lifeExp, color=country, group=country, size=year))

gapminder_plus %>% 
ggplot()+
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) 

saveRDS(by_country, "by_country_tibble.rds")# possible to save any piece of data, plot, table etc
  