library(tidyverse)
download.file(url = "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0TAlJeCEzcGQ&output=xlsx", 
              destfile = "Data/indicator gapminder infant_mortality.xlsx")

download.file(url = "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0NpF2PTov2Cw&output=xlsx", 
              destfile = "Data/indicator undata total_fertility.xlsx")

library("readxl")
raw_fert <- read_excel(path = "Data/indicator undata total_fertility.xlsx", sheet = "Data")
raw_infantMor <- read_excel(path = "Data/indicator gapminder infant_mortality.xlsx", sheet = "Data")

#changing the wide into long format
fert <- raw_fert %>% 
  rename(country = `Total fertility rate`)%>% 
  gather(key=year, value=fert, -country) %>% 
  mutate(year=as.integer(year))

download.file(url = "https://raw.githubusercontent.com/dmi3kno/SWC-tidyverse/master/data/gapminder_plus.csv", 
              destfile = "Data/gapminder_plus.csv")
gapminder_plus <- read_csv(file = "Data/gapminder_plus.csv")

gapminder_plus %>% 
  filter(continent == "Africa", year == "2007") %>% 
  mutate(dead_babies=infantMort*pop/10^3) %>% 
  filter(dead_babies>2*10^6) %>% 
  select(country) %>% 
  left_join(gapminder_plus) %>% #View() %>% 
  mutate(dead_babies=infantMort*pop/10^3, gdp_bln=gdpPercap*pop/1e9, pop_mln=pop/10^6) %>%
  select(-continent) %>% 
  gather(key=variables, value=values, -c(country, year, dead_babies, pop)) %>%  #de-select what I don't want to be gathered, it is shorter
  ggplot()+ #it can also be (.) or (data=.) because pipe output drops the data into the dot
  geom_line(mapping = aes(x = year, y = values, color = country) )+
  geom_text(data=. %>% filter(year == 2007) %>% 
              group_by(variables) %>% 
              mutate(max_value=max(values)) %>%  
              filter(values==max_value),
              aes(x=year, y=values, label=country, color=country))+
  facet_wrap(~ variables, scales = "free_y")+
  labs(title = "Key Parameters for selected African countries", 
       subtitle = "with over 2mln babies",
       caption = "some data from gapminder",
       x ="Year",
       y=NULL)+
       theme_bw()+#how it looks like
       theme(legend.position = "none")
  