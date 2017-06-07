library(tidyverse) #loading packages
gapminder <- read_csv(file = "Data/gapminder-FiveYearData.csv") #specific function for tidyverse, Data folder automatically created for project
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) #dots

ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x = gdpPercap, y = lifeExp, color = continent)) #dots separated and colored by groups

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp, color = continent, size = pop)) #each ponit has variable size

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp) , alpha =0.1, size =2, color = "blue") # defined outside of aes, so it does not refer to the data but to all of the points - alpha transparency, size of points and color

ggplot(data = gapminder) +
  geom_line(mapping = aes(x = log(gdpPercap), y = lifeExp, color = continent, group = country))

ggplot(data = gapminder) +
  geom_line(mapping = aes(x = year, y = lifeExp, color = continent, group = country))

ggplot(data = gapminder) +
  geom_boxplot(mapping = aes(x = continent, y = lifeExp))
 
ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x = continent, y = lifeExp, color = continent)) +
  geom_boxplot(mapping = aes(x = continent, y = lifeExp, color = continent))     # order matters, one aes on top of other         

ggplot(data = gapminder, mapping = aes(x = continent, y = lifeExp, color = continent)) +
  geom_jitter() +
  geom_boxplot() # since the same mapping, it can be oved to the body of function 
  
ggplot(data = gapminder, mapping = aes(x = continent, y = lifeExp, color = continent)) +
  geom_boxplot() +
  geom_jitter()

ggplot(data = gapminder, mapping = aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_smooth(method = "lm") +
  geom_jitter(alpha = 0.1) #changes specific to the layer

ggplot(data = gapminder, mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_smooth(method = "lm") +
  geom_jitter(alpha = 0.1, color = continent) #it does not understand where to map it to

ggplot(data = gapminder, mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_smooth(method = "lm") +
  geom_jitter(mapping = aes(color = continent), alpha = 0.1)

ggplot(data = gapminder, mapping = aes(x = as.factor(year), y = lifeExp)) +
  geom_boxplot()

ggplot(data = gapminder, mapping = aes(x = as.factor(year), y = log(gdpPercap))) +
  geom_boxplot()

ggplot(data = gapminder) +
  geom_density2d(mapping = aes(x = lifeExp, y = log(gdpPercap), group = continent, color = continent)) #doesnt work

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point()+
  geom_smooth()+
  scale_x_log10()+ # automatic log forx data
  facet_wrap(~ continent)

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp, color = continent))+ #now smoothing is for all contries together
  geom_smooth(method = "lm")+
  scale_x_log10()+ # automatic log forx data
  facet_wrap(~ year)

ggplot(data = filter(gapminder, year == 2007))+
  geom_bar(mapping = aes(x = continent))

  
ggplot(data = filter(gapminder, year == 2007))+
  geom_bar(mapping = aes(x = continent), stat = "count")

filter(gapminder, year == 2007, continent == "Oceania")

ggplot(data = filter(gapminder, year == 2007, continent == "Oceania"))+
  geom_bar(mapping = aes(x = country, y = pop), stat = "identity") #identity means do not transform my data

ggplot(data = filter(gapminder, year == 2007, continent == "Asia"))+
  geom_col(mapping = aes(x = country, y = gdpPercap))+
  coord_flip() #x exchanged with y

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/10^6)) +
  geom_point()+
  scale_x_log10()+ # automatic log forx data
  facet_wrap(~ year)+
  labs(title = "Life Expectancy versus GDP per year", 
       subtitle = "In the last 50 years life expectancy has improved in most countries", 
       caption= "Source: Gapminder foundation, gapminder.com",
       x = "GDP per capita, in USD",
       color = "Continent",
       y = "Life Expectancy",
       size = "Population in 10^6")

ggsave("my_fancy_plot.png") #saves last plot in working directory 
