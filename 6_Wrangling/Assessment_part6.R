#Assessment Part 1: Data Import

#q1
#Which of the following is NOT part of the data wrangling process?
#Checking correlations between your variables

#q2
#Which files could be opened in a basic text editor?
#.txt .csv .tsv

#q3
#You want to analyze a file containing race finish times for a recent marathon. You open the file in a basic text editor and see lines that look like the following:

#initials,state,age,time
#vib,MA,61,6:01
#adc,TX,45,5:45
#kme,CT,50,4:19

#What type of file is this?
#A comma-delimited file with a header

#q4
#Assume the following is the full path to the directory that a student wants to use as their working directory in R: "/Users/student/Documents/projects/"
#Which of the following lines of code CANNOT set the working directory to the desired "projects" directory?
setwd(/Users/student/Documents/projects/)

#q5
#We want to copy the "murders.csv" file from the dslabs package into an existing folder "data", which is located in our HarvardX-Wrangling projects folder. We first enter the code below into our RStudio console.
getwd()
#[1] "C:/Users/UNIVERSITY/Documents/Analyses/HarvardX-Wrangling"
filename <- "murders.csv"
path <- system.file("extdata", package = "dslabs")
#Which of the following commands would NOT successfully copy “murders.csv” into the folder “data”?
file.copy(file.path(path, "murders.csv"), getwd())

#q6
#You are not sure whether the murders.csv file has a header row. How could you check this?
#Open the file in a basic text editor.
#In the RStudio “Files” pane, click on your file, then select “View File”.
#Use the command read_lines (remembering to specify the number of rows with the n_max argument).

#q7
#What is one difference between read_excel() and read_xlsx()?
#read_excel() reads both .xls and .xlsx files by detecting the file format from its extension, while read_xlsx() only reads .xlsx files.

#q8
#You have a file called “times.txt” that contains race finish times for a marathon. The first four lines of the file look like this:
#initials,state,age,time
#vib,MA,61,6:01
#adc,TX,45,5:45
#kme,CT,50,4:19
#Which line of code will NOT produce a tibble with column names “initials”, “state”, “age”, and “time”?
race_times <- read.csv("times.txt")

#q9
#You also have access to marathon finish times in the form of an Excel document named “times.xlsx”. In the Excel document, different sheets contain race information for different years. The first sheet is named “2015”, the second is named “2016”, and the third is named “2017”.
#Which line of code will NOT import the data contained in the “2016” tab of this Excel sheet?
times_2016 <- read_xlsx("times.xlsx", sheet = “2”)

#q10
#You have a comma-separated values file that contains the initials, home states, ages, and race finish times for marathon runners. The runners’ initials contain three characters for the runners’ first, middle, and last names (for example, “KME”).
#You read in the file using the following code.
race_times <- read.csv(“times.csv”)
#What is the data type of the initials in the object race_times?
#characters

#q11
#Which of the following is NOT a real difference between the readr import functions and the base R import functions?
#The base R import functions can read .csv files, but cannot read files with other delimiters, such as .tsv files, or fixed-width files.

#q12
#You read in a file containing runner information and marathon finish times using the following code.
race_times <- read.csv("times.csv", stringsAsFactors = F)

#What is the class of the object race_times?
#data frame

#q13
#Select the answer choice that summarizes all of the actions that the following lines of code can perform. Please note that the url below is an example and does not lead to data.
url <- "https://raw.githubusercontent.com/MyUserName/MyProject/master/MyData.csv "
dat <- read_csv(url)
download.file(url, "MyData.csv")
#Create a tibble in R called dat that contains the information contained in the csv file stored on Github. Download the csv file to the working directory and name the downloaded file “MyData.csv”.


#Assessment Part 2: Data Import
library(tidyverse)
#q14
#Inspect the file at the following URL:
#https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data External link
#Which readr function should be used to import this file?
#read_csv()

#q15
#Check the documentation for the readr function you chose in the previous question to learn about its arguments. Determine which arguments you need to the file from the previous question:
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
#No, there is no header. The col_names=FALSE argument is necessary.


#Assessment: Combining Tables
#q1
#You have created data frames tab1 and tab2 of state population and election data, similar to our module lines:
tab1
tab2
dim(tab1)
# 5 x 2
dim(tab2)
# 6 x 2
dat <- left_join(tab1, tab2, by = “state”)
#5 rows by 3 columns

#q2
#We are still using the tab1 and tab2 tables shown in question 1. What join command would create a new table “dat” with three rows and two columns?
dat <- inner_join(tab1, tab2, by = “state”) 

#q3
#Which of the following are real differences between the join and bind functions?
#Binding functions combine by position, while join functions match by variables.
#Joining functions can join datasets of different dimensions, but the bind functions must match on the appropriate dimension (either same row or column numbers).
#Bind functions can combine both vectors and dataframes, while join functions work for only for dataframes.

#q4
#We have two simple tables, shown below, with columns x and y:
df1<-data.frame(x=c("a","b"),y=c("a","a"))
df1
df2<-data.frame(x=c("a","a"),y=c("a","b"))
df2
final <- setdiff(df1, df2)

#intro q5-7
#install.packages("Lahman")
library(Lahman)
#The Batting data frame contains the offensive statistics for all baseball players over several seasons.  Filter this data frame to define top as the top 10 home run (HR) hitters in 2016:
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
#Also Inspect the People data frame, which has demographic information for all players:
People %>% as_tibble()

#q5
top_names <- top %>% left_join(People) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

#q6
#Which bind or join function fills the blank to generate the correct table?
#Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, then use the correct bind join function to add a salary column to the top_names data frame from the previous question. Name the new data frame top_salary. Use this code framework:
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

#q7
#Inspect the AwardsPlayers table. Filter awards to include only the year 2016.
#
won <- AwardsPlayers %>% filter(yearID == 2016)
#How many players from the top 10 home run hitters won at least one award in 2016?
length(intersect(won$playerID, top$playerID))
#How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?
length(setdiff(won$playerID, top$playerID))
#q16
#Inspect the imported data from the previous question.
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_csv(url, col_names=F)
#How many rows are in the dataset?
#How many columns are in the dataset?

#Assessment Part 2: Reshaping Data
library(tidyverse)
library(dslabs)

#Q1
#If you have numerical data in multiple columns, but you want to combine them into one column, which function should you use?
#pivot_longer()

#q2
#If you have text data in one column, but you want to split it into multiple columns, which function should you use?
#separate() should be correct, but pivot_longer() appears as correct.

#q3
#A collaborator sends you a file containing data for three years of average race finish times.

#age_group,2015,2016,2017
#20,3:46,3:22,3:50
#30,3:50,3:43,4:43
#40,4:39,3:49,4:51
#50,4:48,4:59,5:01

#Are these data considered “tidy” in R? Why or why not?
#No. These data are not considered “tidy” because the variable “year” is stored in the header.

#q4
#which of the datasets is tidy?

#q5
#Your file called “times.csv” has age groups and average race finish times for three years of marathons.
d <- read_csv("times.csv")
#Which of the following commands will help you “tidy” the data?
tidy_data <- d %>%
  pivot_longer('2015':'2017', names_to = "year", values_to = "time")

#q6
#You have a dataset on U.S. contagious diseases, but it is in the following wide format:
head(dat_wide)
#state year population HepatitisA Mumps Polio Rubella
#Alabama 1990    4040587      86	   19    76    1
#Alabama 1991    4066003      39	   14    65    0
#Alabama 1992    4097169      35	   12    24    0
#Alabama 1993    4133242      40	   22    67    0
#Alabama 1994    4173361      72	   12    39    0
#Alabama 1995    4216645      75     2    38    0
#You want to transform this into a tidy dataset, with each row representing an observation of the incidence of each specific disease (as shown below):
head(dat_tidy)
#state   year  population  disease  count
#Alabama 1990	4040587 HepatitisA	86
#Alabama 1991	4066003 HepatitisA	39
#Alabama 1992	4097169 HepatitisA	35
#Alabama 1993	4133242 HepatitisA	40
#Alabama 1994	4173361 HepatitisA	72
#Alabama 1995	4216645 HepatitisA	75
#Which of the following commands would achieve this transformation to tidy the data?
dat_tidy <- dat_wide %>%
  pivot_longer(HepatitisA:Rubella, names_to = "disease", values_to = "count")

#q7
#You have successfully formatted marathon finish times into a tidy object called tidy_data. The first few lines are shown below.
#age_group year   time
#20        2015   03:46
#30        2015   03:50
#40        2015   04:39
#50        2015   04:48
#20        2016   03:22

#Select the code that converts these data back to the wide format, where each year has a separate column.
tidy_data %>% pivot_wider(names_from = year, values_from = time)

#q8
#You have the following dataset:
head(dat)
#state   abb region    	var   people
#Alabama  AL  South population 4779736
#Alabama  AL  South  	total 	  135
#Alaska   AK   West population  710231
#Alaska   AK   West  	total  	   19
#Arizona  AZ   West population 6392017
#Arizona  AZ   West  	total 	  232

#You would like to transform it into a dataset where population and total are each their own column (shown below):
dat_tidy <- dat %>% pivot_wider(names_from = var, values_from = people)

#q9
#A collaborator sends you a file containing data for two years of average race finish times, "times.csv":

#age_group,2015_time,2015_participants,2016_time,2016_participants
#20,3:46,54,3:22,62
#30,3:50,60,3:43,58
#40,4:39,29,3:49,33
#50,4:48,10,4:59,14

#You read in the data file:
d <- read_csv("times.csv")

#Which of the answers below best makes the data tidy?
tidy_data <- d %>%
  pivot_longer(-age_group, names_to = "key", values_to = "value") %>% 
  separate (col  = key, into  = (c("year", "variable_name"), sep = "_") %>% 
             pivot_wider(names_from = variable_name, values_from = value)

#q10
#You are in the process of tidying some data on heights, hand length, and wingspan for basketball players in the draft. Currently, you have the following:
head(stats)
#Select all of the correct commands below that would turn this data into a tidy format with columns "height", "hand
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  pivot_wider(names_from = variable_name, values_from = value)

tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  pivot_wider(names_from = variable_name, values_from = value)

tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
  pivot_wider(names_from = variable_name, values_from = value)


#Q11 #dataset
co2

#Q12
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- pivot_longer(co2_wide, -year, names_to = "month", values_to = "co2")
co2_tidy 

#Q13
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

#Q14
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)
dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)
head(dat_tidy)

#Q15
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

#Q16
#Which function can reshape tmp2 to a table with six rows and five columns named major, admitted_men, admitted_women, applicants_men, and applicants_women?
#pivot_wider()

#WEB SCRAPPING ASSESSMENT
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])

#q1
t1<-html_table(nodes[[1]])
t1
t2<-html_table(nodes[[2]])
t2
t3<-html_table(nodes[[3]])
t3
t4<-html_table(nodes[[4]])
t4

#q2
length(nodes)

html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])

#q3
tab1<-html_table(nodes[[10]])
tab1 
tab1 <- tab1 %>% select(X2, X3, X4) 
tab1 <-  data.frame(tab1[-1,])
colnames(tab1)<-c("Team", "Payroll", "Average")
tab1                   

tab2<-html_table(nodes[[19]])
tab2
tab2 <- tab2 %>% select(X1, X2, X3) 
tab2 <-  data.frame(tab2[-1,])
colnames(tab2)<-c("Team", "Payroll", "Average")
tab2

combine<-tab1 %>% full_join(tab2, by= "Team")
str(combine)


#Info to q4-5
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

tab <- html_nodes(read_html(url), "table")
length(tab)

#q5
html_table(tab, fill=T)


#Assessment: String processing part 2
#q5
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

#q6
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

#q7
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

#q8
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo*" 
pattern <- "mo?"
str_detect(animals, pattern)

#9
schools<-c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia","U California","California State University")
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

#q10
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#q11
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#q12
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

#q13
yes <- c("5 feet 7inches", '5 7')
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#q14
s<- "19"
pattern <- "\\d"
pattern <- "[3-42]"
str_detect(s,pattern)

#q15
s<- "19"
pattern <- "193+"
str_detect(s,pattern)


#Assessment: String processing part 3
#q1
str_split(schedule$staff, ", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")

#q2
schedule <-data.frame(day=c("Monday", "Tuesday"), staff=c("Mandy, Chris and Laura","Steve, Ruth and Frank"))
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
tidy

#q3
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))  
#q4
s<- "19.5"
pattern <- "^1\\d*$"
str_detect(s,pattern)

#Assessment part 2
#q5
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

#Update polls by changing the column names to c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes") and only keeping rows that have a percent sign (%) in the remain column.
polls <- polls[-1,]
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
pattern <- "\\d+(?:\\.\\d+)?%"
#How many rows remain in the polls data frame?
length(grep(pattern, polls$remain))


#q6
#The remain and leave columns are both given in the format "48.1%": percentages out of 100% with a percent symbol.
#Which of these commands converts the remain vector to a proportion between 0 and 1?
as.numeric(str_remove(polls$remain, "%"))
parse_number(polls$remain)
as.numeric(str_replace(polls$remain, "%", ""))/100
parse_number(polls$remain)/100


#q7
#The undecided column has some "N/A" values. These "N/A"s are only present when the remain and leave columns total 100%, so they should actually be zeros.

#Use a function from stringr to convert "N/A" in the undecided column to 0. The format of your command should be function_name(polls$undecided, "arg1", "arg2").

#What function replaces function_name?
str_replace(polls$undecided, "N/A", "0")

#q8
#The dates column contains the range of dates over which the poll was conducted. The format is "8-10 Jan" where the poll had a start date of 2016-01-08 and end date of 2016-01-10. Some polls go across month boundaries (16 May-12 June).
#The end date of the poll will always be one or two digits, followed by a space, followed by the month as one or more letters (either capital or lowercase). In these data, all month abbreviations or names have 3, 4 or 5 letters.
#Write a regular expression to extract the end day and month from dates. Insert it into the skeleton code below:
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date
#"\\d+\\s[a-zA-Z]+"  "[0-9]+\\s[a-zA-Z]+" "\\d{1,2}\\s[a-zA-Z]+" "\\d+\\s[a-zA-Z]{3,5}"


#Assessment:Dates, Times, and Text Mining
library(dslabs)
library(lubridate)
options(digits = 3) 

#q3
data(brexit_polls)
head(brexit_polls)
brexit_polls %>% mutate(month=month(startdate)) %>% filter(month==4) %>% count()

brexit_polls %>% mutate(fin=round_date(enddate, unit="weeks")) %>% filter(fin=="2016-06-12") %>% count()

#q4
brexit_polls %>% mutate(fin_day=weekdays(enddate)) %>% group_by(fin_day) %>% count() %>% arrange(desc(n))

#q5
data(movielens)
head(movielens)
movielens %>% mutate(date=as_datetime(timestamp), year=year(date), hour=hour(date)) %>% group_by(year) %>% count() %>% arrange(desc(n))
movielens %>% mutate(date=as_datetime(timestamp), year=year(date), hour=hour(date)) %>% group_by(hour) %>% count() %>% arrange(desc(n))

#q6
library(tidyverse)
#install.packages('gutenbergr') not available in my R version
install.packages('devtools')
library(devtools)
install_github("cran/gutenbergr")
library(gutenbergr)

library(tidytext)
options(digits = 3)

head(gutenberg_metadata)
pattern<-"Pride and Prejudice"
index<-which(str_detect(gutenberg_metadata$title, pattern)==TRUE)
count(gutenberg_metadata[index,])

#q7
gutenberg_works(title=="Pride and Prejudice")

#q8
pp<-gutenberg_download(gutenberg_id=1342)

words <- pp %>% 
  unnest_tokens(word, text, token = "words") %>%
  select(word) 
count(words)

#q9
words_sw<-c(stop_words$word)
words %>% filter(!word %in% words_sw) %>% count()

#q10
words %>% filter(!word %in% words_sw & !str_detect(word, "\\d+")) %>% count()

#q11
pp_Word<-words %>% filter(!word %in% words_sw & !str_detect(word, "\\d+"))
pp_Word %>% group_by(word) %>% filter(n()>100) %>% summarize(n()) %>% count()

pp_Word %>% group_by(word) %>% count() %>% arrange(desc(n))

#q12

afinn <- get_sentiments("afinn")
afinn_sentiments <-  inner_join(afinn,words)
dim(afinn_sentiments)

mean(afinn_sentiments$value > 0)

afinn_sentiments %>% filter (value==4) %>% count()



afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)

mean(afinn_sentiments$value > 0)

