###-----------------------------------------------------
### Homework 3 Assignment: Vectors, types and coercion
###-----------------------------------------------------

###-----------------------------------------------------
### First, input the multiple vectors to be classified.
###-----------------------------------------------------

boolean_vector <- c(TRUE,FALSE,FALSE)
numeric_vector <- c(1,2,3)
numeric_vector_2 <- c(1.3,2.4,3.5)
character_vector <- c("a","b", "c")

###-----------------------------------------------------
### Next, type the classify commands.
###-----------------------------------------------------

class(boolean_vector)
class(numeric_vector)
class(numeric_vector_2)
class(character_vector)

###------------------------------------------------------
### When this code is ran, it lists that the objects
### 'boolean_vector', 'numeric_vector', 'numeric_vector_2', 
### and 'character_vector' are logical, numeric, numeric, 
### and character vector classes respectively. Object 
### 'boolean_vector' is classified as logical, as the only 
### entries listed are objects using boolean logic. Objects 
### 'numeric_vector' and 'numeric_vector' are numeric as 
### they are only numbers without quotation marks around 
### them. The addition of quotation marks would make this 
### a character classed vector. And the last object, 
### 'character_vector', is a character class vector as it 
### uses neither boolean nor numeric values.
###------------------------------------------------------

numeric_character_vector <- c(1,2,"a")
boolean_numeric_vector <- c(TRUE,FALSE,2)
class(numeric_character_vector)
class(boolean_numeric_vector)

###------------------------------------------------------
### When the classification of each vector is performed
### you can see that object 'numeric_character_vector' is 
### a character vector while object 'boolean_numeric_vector'
### is a numeric vector. 'numeric_character_vector'  is 
### listed as a character vector due to the fact that "a" 
### can only be stored as a character and not logic or numeric
### values. 1 and 2, while numeric, can also be stored as
### characters, in the case of "1" and "2".That is why this
### is a character vector. In terms of the object
### 'boolean_numeric_vector', it is listed as a numeric class 
### as TRUE and FALSE are in essence just 1 and 0, allowing 
### them to be stored as numerical values. 2 on the other 
### hand, cannot be stored as Boolean logic, as it only uses 
### 0's and 1's, so it cannot be listed as a logical class.
###------------------------------------------------------


###------------------------------------------------------
### The first step in this code is to initialize the 
### package to be utilized. In order to initialize
### tidyverse, the command below is performed.
###------------------------------------------------------

library("tidyverse")

###------------------------------------------------------
### Next, we are trying to store a ".csv" file as an 
### object. In order to do this, we need to be able to
### read the ".csv" file and then store it as an object.
### This is done in the command below.
###------------------------------------------------------

med_enz <- read_csv("data/med_enz.csv")


###------------------------------------------------------
### Then we attempt to determine the class of the
### newly created object via the command below, and find 
### there are actually four distinct classes as there are
### four distinct vectors. These are a "spec_tbl_df", a 
### "tbl_df", a "tbl" and a "data.frame.
###------------------------------------------------------

class(med_enz)


###------------------------------------------------------
### Then we attempt to determine the structure of the 
### data frame and find it consists of 324 objects 
### categorized into 4 variables, 3 of these variables
### being number vectors and one being a character vector.
###------------------------------------------------------

str(med_enz)

###------------------------------------------------------
### Now we can enter the command to determine how many
### rows are created in the data frame, which is 324.
###------------------------------------------------------

nrow(med_enz)

###------------------------------------------------------
### Lastly, we can send the command "glimpse" for our 
### data frame and it will show us the number of rows, 
### columns, and then list off the initial values in 
### each column.
###------------------------------------------------------

glimpse(med_enz)


###------------------------------------------------------
### Lastly, in order to generate a plot stored as an
### object "p", the commands below are entered.
###------------------------------------------------------

p <- ggplot(data = med_enz, aes(x = activity.nM.hr)) + geom_histogram()
print(p)
ggsave(filename = 'plots/hmk_3_plot.png', plot = p, height = 3, width = 4, units = "in", dpi = 300)


