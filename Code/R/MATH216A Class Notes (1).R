#Class Notes 9/11/19
2+2
2 + 2
2                 +2

#Let's learn how to store things in R
x = 2
# x <- 2 #This is the better way
x + x

#Learn how to use functions in R

#Let's look at the c() function
#c() binds things together in a vector
y <- c(1,3,2)
y

#Calculate the mean of y
mean(y)

#Let's sort y
sort(y)

#Let's bring up the help documentation for sort
?sort
sort(x = y)

sort(x = y,
     decreasing = TRUE)

#Install a new package
install.packages("tidyverse")

#Load the tidyverse library
library(tidyverse)

#Access the diamonds data set
View(diamonds)

#Filter our data using the filter() function
expensive.diamonds <- filter(.data = diamonds,
                             price > 15000)

#Let's only take premium cut diamonds
filter(.data = expensive.diamonds,
       cut == 'Premium')

#Let's do both of the above filters simultaneously
filter(.data = diamonds,
       price > 15000 & cut == "Premium")

#Introducing the pipe operator %>%
y %>% mean()

diamonds %>%
  filter(price > 15000) %>%
  filter(cut == "Premium") %>%
  filter(carat > 2) %>%
  filter(color == "G")

#Introducing the arrange() function
diamonds %>%
  filter(price > 15000) %>%
  filter(cut == "Premium") %>%
  filter(carat > 2) %>%
  filter(color == "G") %>%
  arrange(carat)


## Class Notes 9/13/19

#We can arrange in descending order in 2 ways
diamonds %>%
  filter(price > 15000) %>%
  filter(cut == "Premium") %>%
  filter(carat > 2) %>%
  filter(color == "G") %>%
  arrange(desc(carat))

#Now introducing the mutate() function/verb
#mutate() adds a new column to the data set (typically based on existing columns)
diamonds.with.volume <- diamonds %>%
  mutate(volume = x*y*z)

#Now introducing summarize() function/verb summarise()
#Summarizes our data into a single statistic/value
diamonds %>%
  summarize(mean.price = mean(price))

#Introducing group_by()
#groups our data based on a categorical variable
diamonds %>%
  group_by(color, cut) %>%
  summarize(mean.price = mean(price))

#Neat tricks with filter()
#Suppose I want to find diamonds of the 3 best colors:
diamonds %>%
  filter(color == "D" | color == "E" | color == "F")

diamonds %>%
  filter(color %in% c("D", "E", "F"))

#Load in some data from nycflights13
install.packages('nycflights13')
library(nycflights13)

#Name of data set is:
View(flights)
flights

#Practice question(s):

#1) What is the average departure delay for flights
#into Burlington Airport vs. Logan Airport (in Boston)
flights %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  filter(dep_delay > 0) %>%
  group_by(dest) %>%
  summarize(mean.delay = mean(dep_delay,
                              na.rm = TRUE))


#Challenge: Calculate the PROPORTION of flights that are delayed
#into the above airports.

#Hint: Make use of ifelse() function


## Class Notes 9/16/19


flights %>%
  mutate(is.delayed = ifelse(dep_delay > 0,
                             1,
                             0)) %>%  #add an INDICATOR variable
  select(dep_delay,
         is.delayed) #Lets us select columns

##
flights %>%
  mutate(is.delayed = ifelse(dep_delay > 0,
                             1,
                             0)) %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  group_by(dest) %>%
  summarize(prop.delayed = mean(is.delayed,
                                na.rm = TRUE))
## Another solution
flights %>%
  mutate(is.delayed = ifelse(dep_delay > 0,
                             1,
                             0)) %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  group_by(dest) %>%
  na.omit() %>% #Removes ANY row with an NA.
  summarize(number.of.delays = sum(is.delayed),
            total.flights = n(),  #Counts total rows (or "things")
            prop.delayed = number.of.delays/total.flights)

## Making data visualizations
library(ggplot2)

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point()

#Let's spice up our graph with color
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(color = "#3037b8")

#Let's change color based on a variable in the data set
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = clarity))

#Add themes to our graph
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = clarity)) +
  theme_classic()

#Change other optional arguments
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = clarity),
             shape = "diamond") +
  theme_classic()

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = clarity,
                 shape = cut)) +
  theme_classic()


#Let's now make a histogram
#(a plot of a single quantitaive variable)

diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(color = "red")

diamonds %>%
  arrange(price) %>%
  head(1)


## Class Notes 9/18/19
diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(color = "black",
                 fill = "blue",
                 bins = 200) +
  theme_classic()

#Let's "zoom in" on the missing diamonds data
diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(color = "black",
                 fill = "blue",
                 bins = 50) +
  theme_classic() +
  xlim(1300,1700)


### Here are two questions you should ask before making
# any graph.


# Question 1) How many variables am I trying to show?

# Question 2) What types of variables are they?
# Quantitative (Continuous): Takes numeric values that
# have mathematical significance.

# Categorical (Discrete): Values fall into discrete categories
# which are often finite.

# Suppose I want to show the distribution of prices
# of diamonds for each of the different colors.

# 2 variables
# Types: 1 quantiative (price) and 1 cateogrical (color)
diamonds %>%
  ggplot(mapping = aes(x = color,
                       y = price)) +
  geom_boxplot()


## Graph 1
flights %>%
  ggplot(mapping = aes(x = origin,
                       y = sched_arr_time)) +
  geom_violin(aes(fill = origin)) +
  scale_fill_manual(values = c("red",
                               "white",
                               "blue"))

flights %>%
  ggplot(mapping = aes(x = origin,
                       y = sched_arr_time,
                       fill = origin)) +
  geom_violin() +
  scale_fill_brewer(palette = "Spectral")


## Class Notes 9/20/19

# Graph 2
flights %>%
  filter(!is.na(dep_delay)) %>%
  mutate(was.flight.delayed = ifelse(dep_delay > 0,
                                     "delayed",
                                     "not delayed")) %>%
  ggplot(mapping = aes(x = carrier)) +
  geom_bar(aes(fill = was.flight.delayed))


# Graph 3
flights %>%
  filter(!is.na(dep_delay)) %>%
  mutate(was.flight.delayed = ifelse(dep_delay > 0,
                                     "delayed",
                                     "not delayed")) %>%
  ggplot(mapping = aes(x = carrier)) +
  geom_bar(aes(fill = was.flight.delayed),
           position = "fill") +
  ylab("Proportion of delayed flights")



# Graph 4
flights %>%
  group_by(origin) %>%
  summarize(avg.air.time = mean(air_time,
                                na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = reorder(origin,
                                   -avg.air.time),
                       y = avg.air.time)) +
  geom_bar(aes(fill = origin),
           stat = "identity") +
  scale_fill_manual(values = c("#A020F0",
                               "#FFA500",
                               "#006400"))
  
#Let's read in data from outside R
climate.data <- read_csv(file.choose())

climate.data <- read_csv("Climate.csv")

#Let's show the relationship of average temperatures
#over time

# x = date
# y = avg. temp
# lines for each country

climate.data %>%
  ggplot(aes(x = dt,
             y = AverageTemperature)) +
  geom_line(aes(color = Country)) +
  theme(legend.position = "none")



library(lubridate)

climate.data %>%
  mutate(year = year(dt)) %>%
  filter(year > 1900) %>%
  group_by(year) %>%
  summarize(avg.yearly.temp = mean(AverageTemperature,
                                   na.rm = TRUE)) %>%
  ggplot(aes(x = year,
             y = avg.yearly.temp)) +
  geom_line() +
  theme(legend.position = "none")


#Let's try geom_point()
climate.data %>%
  mutate(year = year(dt)) %>%
  filter(year > 1900) %>%
  group_by(year) %>%
  summarize(avg.yearly.temp = mean(AverageTemperature,
                                   na.rm = TRUE)) %>%
  ggplot(aes(x = year,
             y = avg.yearly.temp)) +
  geom_line(alpha = .2) +
  geom_point(aes(color = avg.yearly.temp)) +
  scale_color_gradient(low = "blue",
                       high = "red")

## Class Notes 9/23/19

#Read in our "big" data
install.packages('data.table')
library(data.table)
reddit.data <- fread(file.choose())
reddit.data <- fread("RedditUsersFull(4).csv")

#Let's learn to manipulate strings!
library(stringr)

reddit.subset <- reddit.data %>%
  head(5)

#Let's find all of the reddit usernames with an "a"
str_detect(reddit.subset$author,
           "a")

#Let's deal with case sensitivity
str_detect(reddit.subset$author,
           "a|A")

str_detect(reddit.subset$author,
           regex("a",
                 ignore_case = TRUE))

#Let's identify usernames with ANY digit
str_detect(reddit.subset$author,
           "[:digit:]")


#Activity: Calculate the proportion of users on reddit
#with at least one digit in their name.

solving.this.problem <- str_detect(reddit.subset$author,
                                   "[:digit:]")

reddit.subset %>%
  mutate(digits = solving.this.problem) %>%
  summarize(prop.digits = mean(digits))

## A similar solution
reddit.data %>%
  mutate(digits = ifelse(str_detect(reddit.data$author,
                                    "[:digit:]"),
                         1,
                         0)) %>%
  summarize(prop.digits = mean(digits))


#Let's count the number of digits in each username (on average)

str_count(reddit.subset$author,
          "[:digit:]")

reddit.data %>%
  mutate(num.digits = str_count(reddit.data$author,
                                "[:digit:]")) %>%
  summarize(mean.digits = mean(num.digits))

#Let's restrict our data to only usernames with digits
reddit.data %>%
  mutate(num.digits = str_count(reddit.data$author,
                                "[:digit:]")) %>%
  filter(num.digits > 0) %>%
  summarize(mean.digits = mean(num.digits))


## Class Notes 9/25/19


# Let's calculate the distribution of digits (0-9) in reddit usernames


#Start with 0
reddit.data %>%
  mutate(zero.count = str_count(author,
                                "0")) %>%
  summarize(total.zero.count = sum(zero.count))

#Introducing for loops

for(i in 1:10){
  some.random.value <- i
}

#Learn how to index in R (indexing a vector and data.frame)
toy.vector <- c(1, 3, 5, 11, 13)

toy.vector[2] #This extracts the 2nd element of our toy.vector
toy.vector[2:4]
toy.vector[c(1,3,4)]
toy.vector[-1] #Removes the first element of toy.vector
toy.vector[length(toy.vector)]

#Now let's look at a data set
diamonds[1, 1] #extracts the first row and first column of diamonds
diamonds[1:4, 5:9]
diamonds[[1, 1]] #Force output to be a number

# suppose I want a vector of the numbers 1 through 10
ten.loop <- NULL

for(i in 1:10){
  ten.loop[i] <- i
}



#Calculate distribution of digits
digit.vector <- NULL

for(i in 0:9){
  digit.vector[i+1] <- reddit.data %>%
    mutate(digit.count = str_count(author,
                                  toString(i))) %>%
    summarize(total.digit.count = sum(digit.count)) %>%
    .[[1]]
  
  print(i)
}

digit.vector


#Create a data set for graphing
digit.data <- data.frame(count = digit.vector,
                         digit = 0:9)


digit.data %>%
  ggplot(aes(x = factor(digit), #Make digit categorical
             y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  ylab("Count (in millions)") +
  scale_y_continuous(labels = c(0,1,2,3),
                     breaks = c(0,1000000,2000000,3000000)) +
  xlab("Digit")

## Class Notes 9/27/19

reddit <- fread("RedditUsersFull.csv")

### Useful characters

#  ^   - denotes start of a string
# $    - denotes end of a string

reddit100 <- reddit %>%
  head(100)

#Indentifying appending 2-digit numbers
str_detect(reddit100$author,
           "[:digit:][:digit:]$")

reddit100 %>%
  filter(str_detect(reddit100$author,
                    "([:alpha:]|[:punct:])[:digit:]{2}$"))

#Do 2-character usernames exist?
reddit %>%
  mutate(length = str_length(author)) %>%
  filter(length <= 2)

#Make our two-digit user dataset
two.digit.data <- reddit %>%
  filter(str_detect(author,
                    "([:alpha:]|[:punct:])[:digit:]{2}$"))

  
#Extract these two-digit numbers
test <- "Alex99"

str_sub(test,
        start = str_length(test) - 1,
        end = str_length(test))

two.digit.numbers <- str_sub(two.digit.data$author,
        start = str_length(two.digit.data$author) - 1,
        end = str_length(two.digit.data$author))
  
two.digit.data.final <- data.frame(digits = as.numeric(two.digit.numbers))


two.digit.data.final %>%
  ggplot(aes(x = digits)) +
  geom_bar() +
  scale_x_continuous(limits = c(60,99),
                     breaks = 60:99)



###
profiles <- fread("Profiles.csv")

#Make a distribution of heights of people on OKCupid
profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram()

summary(profiles$height)

profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram() +
  scale_x_continuous(limits = c(48, 84))

## Class Notes 9/30/19

# Learn how to join data in R

toy.data1 <- data.frame(name = c("Alex",
                                 "Bob",
                                 "Bill"),
                        color = c("Red",
                                  "Blue",
                                  "Green"))
toy.data2 <- data.frame(name = c("Alex",
                                 "Frank",
                                 "Bill"),
                        salary = c(5,
                                   3,
                                   2))


left_join(toy.data1,
          toy.data2)

inner_join(toy.data1,
           toy.data2)

full_join(toy.data1,
          toy.data2,
          by = "name") #join my data sets by the name column

toy.data2 <- data.frame(Names = c("Alex",
                                 "Frank",
                                 "Bill"),
                        salary = c(5,
                                   3,
                                   2))

left_join(toy.data1,
          toy.data2,
          by = c("name" = "Names"))


#Let's look at faceting
profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram() +
  scale_x_continuous(limits = c(48, 84)) +
  facet_wrap(~sex)

profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram() +
  scale_x_continuous(limits = c(48, 84)) +
  facet_wrap(drinks~sex)


## Class Notes 10/2/19

profiles %>%
  ggplot(aes(x = body_type)) +
  geom_bar(aes(fill = body_type),
           stat = "count")+
  facet_wrap(~sex) +
  coord_flip()
  
profiles %>%
  ggplot(aes(x = sex)) +
  geom_bar(aes(fill = body_type),
           position = "fill")

profiles2 <- profiles

#Reorder my drinks column
profiles2$drinks <- factor(profiles2$drinks,
                           levels = c("not at all",
                                      "rarely",
                                      "socially",
                                      "often",
                                      "very often",
                                      "desperately",
                                      ""))

profiles2 %>%
  filter(age < 80) %>%
  ggplot(aes(x = drinks,
             y = age)) +
  geom_boxplot(aes(fill = sex))



#Web scraping
#Someone has something on the internet, and I want it!
library(rvest)

url1 <- "https://en.wikipedia.org/wiki/Baker_Mayfield"

url1 %>%
  read_html() %>% #Read HTML from website
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>% #What exactly am I looking for?
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  View()

bm.data <- url1 %>%
  read_html() %>% #Read HTML from website
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>% #What exactly am I looking for?
  html_table(fill = TRUE) %>%
  .[[1]]

#Let's make a copy of our bm.data dataset
bm.data2 <- bm.data

#Change column names of our data
colnames(bm.data2) <- bm.data[1, ]

#Class Notes 10/4/19

#Let's remove rows 1, 3, and 7
bm.data2 <- bm.data2[-c(1, 3, 7), ]

#Just give me the first 14 columns
bm.data2 <- bm.data2[ , 1:14]

bm.data2 %>%
  ggplot(aes(x = Year,
             y = Yards)) +
  geom_bar(stat = "identity")

#Let's first get rid of all of the commas
bm.data2$Yards <- str_replace_all(bm.data2$Yards,
                                  ",",
                                  "")

#Let's convert our categorical Yards to numemric Yards
bm.data2$Yards <- as.numeric(bm.data2$Yards)

bm.data2 %>%
  ggplot(aes(x = Year,
             y = Yards)) +
  geom_bar(stat = "identity")

#What happens if we have a column with spaces (or weird characters)
bm.data2 %>%
  mutate(`We have a column like this` = 5)

#Let's scrape in some text
cnn.url <- "https://www.cnn.com/2019/10/03/politics/us-envoys-trump-ukraine-investigate/index.html"
library(rvest)

cnn.text <- cnn.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="body-text"]') %>%
  html_text() %>%
  str_replace_all("\\\\", "")


## Class Notes 10/7/19
library(tidytext)

trump.url <- "https://en.wikipedia.org/wiki/Donald_Trump"

trump.text <- trump.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()


#Convert our vector of text into a data.frame
trump.data <- data.frame(text = trump.text,
                         stringsAsFactors = FALSE)

#Tokenize my data (default token is a word)
trump.data %>%
  unnest_tokens("word", "text")

#Count up the number of each token (word)
trump.data %>%
  unnest_tokens("word", "text") %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(-count)

#Introducing the count() function
trump.data %>%
  unnest_tokens("word", "text") %>%
  count(word) %>%
  arrange(-n)


#Let's remove stopwords (words without meaning) from our analysis
stop_words

#Let's make use of anti-join
trump.data %>%
  unnest_tokens("word", "text") %>%
  count(word) %>%
  arrange(-n) %>%
  anti_join(stop_words) %>%
  head(10) %>%
  ggplot(aes(x = reorder(word, n),
             y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text = element_text(size = 18))


## Class Notes 10/9/19

#Put multiple graphs on the same page/graphic
library(gridExtra)
grid.arrange()


#Sentiment Analysis
library(tidytext)

#Highlight 3 sentiment dictionaries
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

trump.text <- trump.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()


#Convert our vector of text into a data.frame

trump.text <- str_replace_all(trump.text,
                              "\n",
                              "")

trump.data <- data.frame(text = trump.text,
                         stringsAsFactors = FALSE) %>%
  filter(text != "") %>%
  mutate(paragraph = row_number())

trump.data[1,1]

#Tokenize my data (default token is a word)
#Calculate sentiments of trump Wikipedia text
trump.data %>%
  unnest_tokens("word", "text") %>%
  count(word) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(something = n*value) %>%
  summarize(sum(something))


## Class Notes 10/11/19

#Let's visualize the sentiment by paragraph
trump.data %>%
  unnest_tokens("word", "text") %>%
  count(paragraph, word) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(something = n*value) %>%
  group_by(paragraph) %>%
  summarize(sum = sum(something)) %>%
  mutate(is.positive = ifelse(sum > 0,
                              TRUE,
                              FALSE)) %>%
  ggplot(aes(x = paragraph,
             y = sum)) +
  geom_bar(aes(fill = is.positive),
           stat = "identity") +
  # geom_hline(yintercept = 0,
  #            color = "red",
  #            size = 2) +
  scale_fill_manual(values = c("red",
                               "black"))

#Let's find these super negative paragraphs
trump.data %>%
  unnest_tokens("word", "text") %>%
  count(paragraph, word) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(something = n*value) %>%
  group_by(paragraph) %>%
  summarize(sum = sum(something)) %>%
  mutate(is.positive = ifelse(sum > 0,
                              TRUE,
                              FALSE)) %>%
  arrange(sum)

trump.data$text[165]


#Let's repeat this analysis for ALL presidents

presidents <- c("Bill Clinton",
                "Barack Obama",
                "Donald Trump")

president.data <- NULL

for(i in 1:length(presidents)){
  
  trump.url <- paste0("https://en.wikipedia.org/wiki/",
                      presidents[i])
  
  trump.text <- trump.url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  
  
  #Convert our vector of text into a data.frame
  
  trump.text <- str_replace_all(trump.text,
                                "\n",
                                "")
  
  trump.data <- data.frame(text = trump.text,
                           stringsAsFactors = FALSE) %>%
    filter(text != "") %>%
    mutate(paragraph = row_number())
  

  
  #Tokenize my data (default token is a word)

  

  
  #Let's visualize the sentiment by paragraph
  president.data[[i]] <- trump.data %>%
    unnest_tokens("word", "text") %>%
    count(paragraph, word) %>%
    inner_join(get_sentiments("afinn")) %>%
    mutate(something = n*value) %>%
    group_by(paragraph) %>%
    summarize(sum = sum(something)) %>%
    mutate(is.positive = ifelse(sum > 0,
                                TRUE,
                                FALSE)) %>%
    ggplot(aes(x = paragraph,
               y = sum)) +
    geom_bar(aes(fill = is.positive),
             stat = "identity") +
    # geom_hline(yintercept = 0,
    #            color = "red",
    #            size = 2) +
    scale_fill_manual(values = c("red",
                                 "black")) +
    ggtitle(presidents[i])
  
  print(i)
}


## Class Notes 10/14/19

g1 <- ggplot()


president.data[3]

#Show all 3 graphs simultaneously
library(gridExtra)
grid.arrange(president.data[[1]],
             president.data[[2]],
             president.data[[3]])


#Let's get all of the presidents
pres.url <- "https://www.plaintextlist.com/politics/list_of_us_presidents/"

presidents <- pres.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text() %>%
  unique() %>%
  str_replace_all(" ",
                  "\\_")



president.data[27]


#Let's try to plot all 44 graphs
grid.arrange(president.data[[1]],
             president.data[[2]],
             president.data[[3]],
             president.data[[4]],
             president.data[[5]],
             president.data[[6]],
             ncol = 2)

#Let's make web apps using shiny
library(shiny)

#Let's test our graphing code OUTSIDE of shiny

input <- NULL
input$slider1 <- 50

profiles %>%
  filter(age >= input$slider1) %>%
  ggplot(aes(x = age,
             y = height)) +
  geom_point()

##Class Notes 10/16/19

#Let's design an app that shows users the relationship
#between departure time and arrival time of flights (using
# the flights data set) AND highlights all flights belonging to
# the user-selected carrier.




#Let's use read_csv() to read in the data
life <- read_csv(file.choose())
income <- read_csv(file.choose())

life.data <- life
income.data <- income

#Let's convert from wide-format data to long-format data
life.gathered <- life.data %>%
  gather(key = "Year", #Hidden variable whose values appear in columns
         value = "Life.Expectancy", #Hidden variable whose values are in the cells
         -country)

income.gathered <- income.data %>%
  gather(key = "Year", #Hidden variable whose values appear in columns
         value = "Income", #Hidden variable whose values are in the cells
         -country)

joined.data <- life.gathered %>%
  inner_join(income.gathered,
             by = c("country", "Year"))


#Let's check which countries got excluded
income.data %>%
  anti_join(life.data,
            by = "country")



#Let's make a graph of the state of the world in the year 2000
joined.data %>%
  filter(Year == 2000) %>%
  ggplot(aes(x = Income,
             y = Life.Expectancy)) +
  geom_point()


library(rvest)

region.url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

regions <- region.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table() %>%
  .[[1]]

#Let's try joining our data

joined.data %>%
  inner_join(regions %>% select(-`Global South`),
             by = c("country" = "Country")) %>%
  View()

#What did we lose in this join?
income.data %>%
  anti_join(regions %>% select(-`Global South`),
             by = c("country" = "Country")) 

## Class Notes 10/23/19
joined.data <- joined.data %>%
  inner_join(regions %>% select(-`Global South`),
             by = c("country" = "Country"))


#Scrape in population data
pop.url <- "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"

pop.data <- pop.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table() %>%
  .[[1]]

population.data <- pop.data

#Change column names
colnames(population.data)[c(2,6)] <- c("country", "Population")

population.data <- population.data %>%
  select(country, Population)

#Let's make our Population numeric
population.data$Population <- str_replace_all(population.data$Population,
                                              ",",
                                              "") %>%
  as.numeric()

#Let's remove our superscripts
population.data$country <- str_replace_all(population.data$country,
                                           "\\[[:alpha:]\\]",
                                           "")

#Here is a more general approach using the wild card character
population.data$country <- str_replace_all(population.data$country,
                                           "\\[.+\\]",
                                           "")


#Let's join our data
joined.data <- joined.data %>%
  left_join(population.data)



#Let's make a graph of the state of the world in the year 2000
joined.data %>%
  filter(Year == 2000) %>%
  ggplot(aes(x = Income,
             y = Life.Expectancy)) +
  geom_point(aes(size = Population,
                 color = Region))+
  scale_x_log10() +
  scale_size_continuous(breaks = c(0, 10000, 100000, 1000000,
                                   10000000, 100000000, Inf))

#Let's save our joined.data to our computer
write_csv(joined.data,
          "gapminder.csv")


## Class Notes 10/25/19
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addMarkers(lat = 44.0153,
             lng = -73.1673,
             label = "Middlebury",
             popup = "Here is a description of Midd")



toy.data <- data.frame(latitude = c(10,-20,30),
                       longitude = c(5,25,45),
                       name = c("A", "B", "C"))

#Let's map the toy.data

toy.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~latitude,
             lng = ~longitude)




#
volcano.url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Like&query_0=&op_8=eq&v_8=&type_10=EXACT&query_10=None+Selected&le_2=&ge_3=&le_3=&ge_2=&op_5=eq&v_5=&op_6=eq&v_6=&op_7=eq&v_7=&t=102557&s=5&d=5"

volcano.data <- volcano.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()

volcano.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(label = ~`Volcano Name`)

#Let's try clustering our markers

volcano.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

#Let's make a choropleth!
library(geojsonio)


#Let's read in our geojson file
states <- geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json",
                       what = "sp")
View(states@data)

#Make copy of SPDF
states.data <- states

states.data %>%
  leaflet() %>%
  #addTiles() %>%
  addPolygons() %>%
  setView(-96, 37.8, 3) #Sets the default viewing window



## Class Notes 10/28/19
library(readxl)

majors <- read_excel(file.choose(),
                     skip = 1)

#Make a copy of my data
majors.data <- majors

#Remove NAs
majors.data <- na.omit(majors.data)

#Remove .s from field of study column
majors.data$`Field of study` <- str_replace_all(majors.data$`Field of study`,
                                                "\\.",
                                                "")

#Remove our first two rows
majors.data <- majors.data[-c(1,2),]

#Gather our data (converting from wide to long format)
majors.gathered <- majors.data %>%
  gather(key = "Year",
         value = "Count",
         -`Field of study`)

#Fix the year column
majors.gathered$Year <- str_replace_all(majors.gathered$Year,
                                        "\\-[:digit:]{2}",
                                        "") %>%
  as.numeric()


#Let's graph!
majors.gathered %>%
  ggplot(aes(x = Year,
             y = Count)) +
  geom_line(aes(color = `Field of study`)) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_text(aes(label = `Field of study`),
            data = majors.gathered %>%
              filter(Year == max(majors.gathered$Year)))

## Class Notes 11/4/19
library(ggimage)
#introduces geom_image()


fake.data <- data.frame(platypi = c("Baby",
                                    "Weird Looking Baby",
                                    "Adult"),
                        cuteness = c(10,
                                  3,
                                  5),
                        picture = c("C:\Users\alyford\OneDrive - Middlebury College\Teaching\Course Material\Fall 2019\MATH 216\Notes\Images\Baby.jpg",
                                    "C:\Users\alyford\OneDrive - Middlebury College\Teaching\Course Material\Fall 2019\MATH 216\Notes\Images\WeirdBaby.jpg",
                                    "C:\Users\alyford\OneDrive - Middlebury College\Teaching\Course Material\Fall 2019\MATH 216\Notes\Images\Adult.jpg"))

fake.data %>%
  ggplot(aes(x = platypi,
             y = cuteness)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image = picture,
                 y = cuteness - 1),
             size = .2)



#Let's read in our geojson file
states <- geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json",
                       what = "sp")

states.data <- states




states.data %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons() %>%
  setView(-96, 37.8, 3)


#Let's color our states based on Area
View(states@data)


bins1 <- c(0, 10000, 50000, 100000, Inf)
colors1 <- colorBin(palette = "YlOrRd",
                    bins = bins1,
                    domain = states.data@data$CENSUSAREA)

states.data %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~colors1(CENSUSAREA),
              weight = 2,
              color = "white",
              dashArray = "3",
              opacity = 1,
              fillOpacity = .7) %>%
  setView(-96, 37.8, 3)

#Let's color our states by population

pop.data <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table() %>%
  .[[1]]


pop.data.clean <- pop.data[ ,c(3,4)]
colnames(pop.data.clean)[2] <- "Population"

#Convert our populations to numeric
pop.data.clean$Population <- str_replace_all(pop.data.clean$Population,
                                             "[:punct:]",
                                             "") %>%
  as.numeric()

#Let's join our data sets
states.data@data <- left_join(states.data@data,
                              pop.data.clean,
                              by = c("NAME" = "Name"))


#Let's graph!

bins1 <- c(0, 1000000, 2000000, 5000000, 10000000, 30000000, Inf)
colors1 <- colorBin(palette = "YlOrRd",
                    bins = bins1,
                    domain = states.data@data$Population)

states.data %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~colors1(Population),
              weight = 2,
              color = "white",
              dashArray = "3",
              opacity = 1,
              fillOpacity = .7) %>%
  setView(-96, 37.8, 3) %>%
  addLegend(pal = colors1,
            values = states.data@data$Population)


## Class Notes 11/8/19
library(rtweet)

solar.tweets <- search_tweets("#solarpanels",
                               n = 100,
                              include_rts = FALSE)

#Let's look at individual users

dt.tweets <- get_timeline("realDonaldTrump")

multiple.tweets <- get_timelines(c("realDonaldTrump",
                                 "HillaryClinton",
                                 "Pontifex"),
                                 n = 100)

#Let's make a time series plot: ts_plot()
multiple.tweets %>%
  group_by(screen_name) %>%
  ts_plot("days")


# stream_tweets()

test.stream <- stream_tweets("#footballcat",
                             timeout = 30)


# Get location data



solar.tweets <- search_tweets("#solarpanels",
                              n = 100,
                              include_rts = FALSE)


soloar.tweets.with.loc <- lat_lng(solar.tweets)

soloar.tweets.with.loc %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

## Class Notes 11/11/19


ggplot()
## Benford's Law

#Learn how to write a function in R
#This function is going to calculate the mean of a set of numbers
#and then add 1 for no reason.

wrongmean <- function(numbers){
  x <- (sum(numbers)/length(numbers)) + 1
  return(x) #Most functions return something to the user
}

y <- c(1,3,5)
mean(x)
wrongmean(numbers = y)


## Let's write a function that calculates the median of a set
## of numbers. BUT you can't use the median() function.


median.two <- function(numbers){
  sort.numbers <- sort(numbers)
  middle.index <- ceiling(length(numbers)/2)
  ifelse(length(numbers) %% 2 == 0,
         return(mean(c(sort.numbers[middle.index],
                     sort.numbers[middle.index + 1]))),
         return(sort.numbers[middle.index]))
}

x <- c(1,2,3)
y <- c(1,2,3,4)

median.two(x)
median.two(y)


#Let's write a function to clean numberic data from the web
cleanNumbers <- function(vector1){
  str_replace_all(vector1,
                  ",",
                  "") %>%
    as.numeric() %>%
    return()
}


## Class Notes 11/13/19
cleanNumbers(c("1,951", "1,723"))


#Benford's Law distribution

benford.data <- data.frame(digit = 1:9,
                           prob = c(.301,
                                    .176,
                                    .125,
                                    .097,
                                    .079,
                                    .067,
                                    .058,
                                    .051,
                                    .046))

#Make a graph of Benford's Law
benford.data %>%
  ggplot(aes(x = factor(digit),
             y = prob)) +
  geom_bar(stat = "identity")

#Let's get some data
#I need to extract the first significant digit from each value
diamonds$price

first.digits <- str_sub(diamonds$price,
        start = 1,
        end = 1)

data.frame(digit = first.digits) %>%
  count(digit) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = digit,
             y = p)) +
  geom_bar(stat = "identity",
           fill = "blue",
           alpha = .5) +
  geom_bar(data = benford.data,
           fill = "red",
           mapping = aes(y = prob),
           stat = "identity",
           alpha = .5)


## Our goal is write a function where the user can input
## a vector of data, we clean the vector appropriately,
## and overlay the first-digit distribution of the data
## with Benford's Law.



## Class Notes 11/15/19

path1 <- '//*[@id="mw-content-text"]/div/table'
url1 <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

pop <- url1 %>%
  read_html() %>%
  html_nodes(xpath = path1) %>%
  html_table() %>%
  .[[1]] %>%
  select(Population)


cleanAndGraph <- function(vector1){
  
  benford.data <- data.frame(digit = 1:9,
                             prob = c(.301,
                                      .176,
                                      .125,
                                      .097,
                                      .079,
                                      .067,
                                      .058,
                                      .051,
                                      .046))
  
  first.digit.data <- str_replace_all(vector1,
                                      ",",
                                      "") %>%
    str_sub(1, 1)
  
  data.frame(digit = first.digit.data) %>%
    count(digit) %>%
    mutate(p = n/sum(n)) %>%
    ggplot(aes(x = digit,
               y = p)) +
    geom_bar(stat = "identity",
             fill = "blue",
             alpha = .5) +
    geom_bar(data = benford.data,
             mapping = aes(y = prob),
             fill = "red",
             stat = "identity",
             alpha = .5)
  
}



cleanAndGraph(pop$Population)


#Let's create an app that allows users to enter their favorite
#song + artist and we'll display the lyrics and give a brief
#sentiment analysis of their song.



## Class Notes 11/18/19

#Let's learn about linear regression!


#Two purposes for linear regression:
#1) I want to predict the value of a quantitative variable
#2) I want to understand and quantify the relationship between
# two quantitaive variables.
library(readxl)
vt.schools <- read_xls(file.choose())

View(vt.schools)


#Let's look at the PERCENTAGE of students on free and reduced lunch
vt.schools$StudentsFreeReducedLunch

vt.schools2 <- vt.schools %>%
  mutate(percentFRL = StudentsFreeReducedLunch/TotalEnrollment * 100)




#Let's do linear regression!

model1 <- lm(M8~percentFRL,
             data= vt.schools2)

summary(model1)


#Let's explore our data
vt.schools2 %>%
  ggplot(aes(x = percentFRL,
             y = M8)) +
  geom_point()



#Let's investigate the outlier point
vt.schools2 %>%
  filter(M8 == 0) %>%
  View()

vt.schools2 %>%
  filter(SupervisoryUnion == "Orange Southwest") %>%
  View()

vt.schools.no.outlier %>%
  ggplot(aes(x = percentFRL,
             y = M8)) +
  geom_point()



#Let's do regression without the outlier
vt.schools.no.outlier <- vt.schools2 %>%
  filter(M8 != 0)

model2 <- lm(M8 ~ percentFRL,
             data = vt.schools.no.outlier)
summary(model2)



vt.schools.no.outlier %>%
  ggplot(aes(x = percentFRL,
             y = M8)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)


## Class Notes 11/20/19

#Let's use our model2 to predict the M8 score for Orange Southwest
vt.schools2 %>%
  filter(M8 == 0) %>%
  select(M8, percentFRL)

#To predict this M8 score, we do:
2597.1759 - 1.0432 * 45.5

predict(model2,
        data.frame(percentFRL = 45.5))


#Let's look at some wine tasting data
wine.data <- read_csv(file.choose())


#Let's randomly select 5 varieties of wine
#We only want varieties with at least 50 different wines
#GOAL: The output of this is a vector of the 5 selected wine varieties.

wine50 <- wine.data %>%
  group_by(variety) %>%
  summarize(count = n()) %>%
  filter(count > 50)

sample(wine50$variety, size = 5)

wine.data %>%
  count(variety) %>%
  filter(n > 50) %>%
  sample_n(5) %>%
  .$variety

#How many varieties are there?
length(unique(wine.data$variety))

set.seed(17)

five.varieties <- wine.data %>%
  count(variety) %>%
  filter(n > 50) %>%
  sample_n(5) %>%
  .$variety

#Is there a difference in the price of the wines?
#Is there a relationship between the "variety" variable
#and the "price" variable
wine.data %>%
  filter(variety %in% five.varieties) %>%
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_wrap(~variety)

wine.data %>%
  filter(variety %in% five.varieties) %>%
  ggplot(aes(x = variety,
             y = price))+
  geom_boxplot(aes(fill = variety))

#Let's use variety to predict price
wine.model <- lm(price ~ variety,
                 data = wine.data %>%
                   filter(variety %in% five.varieties))
summary(wine.model)


#Suppose I want to predict the price of a Verdelho wine
predict(wine.model,
        data.frame(variety = "Verdelho"))


## Class Notes 11/22/19

favorite.number <- c(9, 17, 12, 4, 7, 5, 21, 24, 14)
tv.hours <- c(1, 3, 6, 4, 3, 3, 1.5, 8, 6)

class.data <- data.frame(favorite.number = favorite.number,
                         tv.hours = tv.hours)

#Let's create a linear regression model to predict a person's
#favorite number as a function of how much tv they watch.

class.model1 <- lm(favorite.number ~ tv.hours,
                   data = class.data)
summary(class.model1)



#House price data
house.train <- read_csv(file.choose())
  

#How good is our model?
house.model1 <- lm(SalePrice ~ LotArea,
                   data = house.train)
summary(house.model1)$r.squared

#R-squared is the percentage of variation in my House prices (response variable)
#that's explained by my model.
house.model1 <- lm(SalePrice ~ LotArea,
                   data = house.train)
summary(house.model1)


#Instead of R-squared, we can use AIC (Akaike Information Criterion)
AIC(house.model1)
#Lower AIC is better


#Here is the step() function
model1 <- lm(SalePrice ~ .,
             data = house.train)



#Let's choose a "random" subset of variables
model2 <- lm(SalePrice ~ LotArea + YearBuilt + PoolArea + GarageYrBlt + RoofStyle,
             data = house.train)
summary(model2)

step(model2)

x <- runif(1460, 0, 1)

house.train <- house.train %>%
  mutate(random.numbers = x)

model2 <- lm(SalePrice ~ LotArea + YearBuilt + random.numbers,
             data = house.train)
summary(model2)

step(model2)


## Class Notes 11/25/19

names <- c("Ben", "Mike", "Jessica", "Niki", "Alec", "Jeff", "Angel",
           "Adin", "Kevin", "Carl", "Isabel", "Pedro", "Natalie",
           "Joselyn", "Holly", "Elisa")

sample(names)



#Let's split our training data set into two:
#a training set and a validation set
house.model1 <- lm(SalePrice ~ Fence,
                   data = house.train)
summary(house.model1)


#Let's cut our data set randomly in half
rowsIWant <- sample(1:nrow(house.train),
                    size = .5*nrow(house.train))
  
training.set <- house.train[rowsIWant, ]
validation.set <- house.train[-rowsIWant, ]




