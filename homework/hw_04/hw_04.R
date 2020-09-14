###-----------------------------------------------------
### Homework 4 Assignment: Vectors, types and coercion
###-----------------------------------------------------


###-----------------------------------------------------
### The first thing in order to get the code running is 
### activating the package to be utilized. Do so by 
### entering the command below.
###-----------------------------------------------------

library("tidyverse")

###-----------------------------------------------------
### Now convert the desired .csv file into an object 
### for R to utilize using the command below.
###-----------------------------------------------------
Cnw <- read_csv("data/chris_names_wide.csv")

###-----------------------------------------------------
### In order to convert your wide data set just created
### into a long data set, use the following command.The
### portions of the gather command are the, 1. Object
### being utilized, 2. the first column you are creating
### in your new long data set 3. the second column you 
### are creating in your long data set and lastly, 4. 
### the item your long data set is pivoting around
### of the column created in step 2.
###-----------------------------------------------------

Cnw_long <- gather(Cnw, Sex, Baby_Count, male:female)

###-----------------------------------------------------
### This has established a new long data set with the 
### columns "year", "Sex", and "Baby_Count" To graph them
### use the commands below.
###-----------------------------------------------------


ggplot(Cnw_long, aes(x= year, y=Baby_Count))+geom_line(aes(colour= Sex))

###-----------------------------------------------------
### The command "ggplot" is used to generate graphics.
### The first portion of using "ggplot" is to tell it 
### what data set to use. In this case, "Cnw_long" was
### utilized. The next step is to tell it some of the 
### aesthetics of the table it will generate. The first
### step is tell it what the x and y-axis are based on.
### In this case, the x-axis is the year, and the y-axis
### is the Baby_Count. In order to distinquish the two 
### populations based on lines, adding in the command
### "geom_line" tells the "ggplot" to generate a line
### graph. Next, you can return to the "aes" command
### to tell it to change the color of the variables. In 
### This case, telling it to base the "colour" on the 
### variable "Sex". The colors were set to default, but 
### generate blue and red/pink lines for the genders.
###-----------------------------------------------------

ggplot(Cnw_long, aes(x= year, y=Baby_Count))+geom_smooth()

###-----------------------------------------------------
### Another way of representing the data is by using a 
### "smooth conditional means" line. The command for this
### is above and follows the same syntax as the command
### before, with the exception of the "geom" command. 
### Instead of generating a line plot with the "geom_line"
### command, a "smooth conditional means" line was
### generated via the "geom_smooth" function. Note that
### a lot of "ggplot"'s functions start with "geom_" and 
### then the desired function.
###-----------------------------------------------------

ggplot(Cnw_long, aes(x=Sex, y=Baby_Count))+geom_boxplot()

###-----------------------------------------------------
### Lastly, a boxplot was generated with similar syntax
### to what was used above. The exception is that
### instead of using a line graph, a boxplot is used. 
### Note, again, that the command for the generation
### of a boxplot also starts with "geom_". Also, note
### that telling it what graph to utilize is not done
### until the END of the function command. 
###-----------------------------------------------------


