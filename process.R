# Description to data manipulation with dplyr
#import data
install.packages("dplyr")
library(dplyr)
gapminder <- read.csv("https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv")

#summary
summary(gapminder)

# load the package
install.packages("dplyr")
library(dplyr)
?filter


#only australian data

Australia <- filter(gapminder,country == "Australia")
"this"=="that"
"this"=="this"

#only life expectancy greater than 81
life80<-filter(gapminder,lifeExp>80)

# 2.arrange():reorder rows

#highest GDP per capita
arrange(gapminder,desc(lifeExp))
arrange(gapminder,gdpPercap)
head(arrange(gapminder,desc(gdpPercap)))

#count the no : of rows
count(gapminder,country == "Afghanistan", wt = NULL, sort = FALSE)
"this"=="that"
"this"=="this"
count(gapminder,country=="Angola", wt = NULL, sort = FALSE)
# distinct
distinct(gapminder,country == "china", keep_all = FALSE)
is.na(gapminder)
View(gapminder)

#select():pick variables.....
gap_small <- select(gapminder,year,country,gdpPercap)
# combine operations
gap_small_97 <- filter(gap_small,year== 1997)
arrange(gap_small_97,desc(gdpPercap))

gap_small_97 <- filter(select(gapminder,year,country,gdpPercap),year==1997)

gap_small_Australia <- filter(select(gapminder,year,country,gdpPercap),country=="Australia" )

#using pipe operator(%>%):........
gap_small_1952 <- gapminder %>%
  select(year,country,gdpPercap) %>%
  filter(year == 1952)

gap_small_2002 <- gapminder %>%
  select(year, country,lifeExp) %>%
  filter(year== 2002  & country == "Eritrea")

#4.mutate():create new variables
gap_gdp <- gapminder %>%
  mutate(gdp = pop*gdpPercap, gdpmill=gdp/10^6)

# 5. summarise (): collapse to a single summary
gapminder %>% 
  summarise(meanle = mean(gdpPercap))

library(moments)
gapminder %>% 
  summarise(skewness(lifeExp))

# group_by(): change the scope
gapminder %>% 
  group_by(continent) %>% 
  filter(year==2002)

gapminder %>% 
  group_by(desc(gdpPercap))

# mean life expectency for each continent in 2007
gapminder %>% 
  group_by(continent) %>% 
  filter(year==2007) %>% 
  summarise(meanle = mean(lifeExp))

#challenge: max life expectency for each country ever country recorded
Maxlife <- gapminder %>% 
  group_by(country) %>% 
  summarise(Maxlife = max(lifeExp)) %>% 
  arrange(desc(Maxlife))
Maxlife

#associating ggplot2 with dplyr
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

#ggplot2 for population of Europe for every year
gapminder %>% 
  filter(continent == "Europe") %>% 
  group_by(year) %>% 
  summarise(sum = sum(pop)) %>% 
  ggplot(aes(x = year, y= sum))+
  geom_line()
  
  

#using starwars data
library(dplyr)
?starwars
starwars %>% 
  group_by(species) %>% 
  summarise(n=n(),
            mass = mean(mass , na.rm = TRUE)) %>% 
  filter(n>1)

  


