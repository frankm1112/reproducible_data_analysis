library(tidyverse)
library(ggpubr)
library(png)
UTK_Covid_Data <- read_csv("homework/hw_07/data/UTK_Covid_data.csv")
space_cat_img <- readPNG("homework/hw_07/spacecat.png")

ggplot(UTK_Covid_Data, aes(x = Students, y = Date))+
  ggtitle(label= "how bad we slippin",
          subtitle= "sheer summation o' slippin students")+
  background_image(space_cat_img)+
  theme(
    plot.background = element_rect(fill="green"),
    plot.title = element_text(color ="red", size = 24, face= "bold.italic", hjust=1),
    plot.subtitle = element_text(color = "purple", size= 18, face= "bold"))+
  scale_x_continuous(name = "slip severity of slippin slippers", breaks=c(30, 200, 700, 1100, 1645, 2100), 
    labels = c("30"= "standin", "200" = "slippin", "700" = "trippin", "1100" 
               = "frickin flippin", "1645"= "bruh were in a pandemic", "2100" = "oof"))+
  scale_y_discrete(name = "who a slippin slipper", labels =c("12-Aug-20" = 
        "George Soros","13-Aug-20" = "Danny Bonaduce","14-Aug-20" = 
        "Halle Berry", "15-Aug-20" = "Ben Affleck","16-Aug-20" = "Madonna",
        "17-Aug-20" = "Robert De Niro","18-Aug-20" = "Patrick Swayze",
        "19-Aug-20" = "Peter Gallagher","20-Aug-20" = "Demi Lovato", "21-Aug-20"
        = "Hayden Pannettiere", "22-Aug-20" = "Dua Lipa", "23-Aug-20" = 
        "Louis XVI","24-Aug-20" = "Dave Chappelle","25-Aug-20" = "Sean Connery",
        "26-Aug-20" = "Keke Palmer", "27-Aug-20" = "Sarah Chalke","28-Aug-20" = "Jack Black",
        "29-Aug-20" = "Michael Jackson","30-Aug-20" = "Cameron Diaz","31-Aug-20" = "Chris Tucker",
        "1-Sep-20" = "Dr. Phil", "2-Sep-20" = "Keanu Reeves","3-Sep-20" = "Charlie Sheen",
        "4-Sep-20" = "Beyonce","5-Sep-20" = "Michael Keaton",
        "6-Sep-20" = "Idris Elba", "7-Sep-20" = "Elizabeth I", "8-Sep-20" = "Pink", 
        "9-Sep-20" = "Adam Sandler", "10-Sep-20" = "Guy Ritchie",
        "11-Sep-20" = "Kygo", "12-Sep-20" = "Paul Walker", "13-Sep-20" = "Tyler Perry",
        "14-Sep-20" = "Andrew Lincoln", "15-Sep-20" = "Tommy Lee Jones",
        "16-Sep-20" = "B.B. King", "17-Sep-20" = "Narendra Modi", "18-Sep-20" = "Lance Armstrong",
        "19-Sep-20" = "Jimmy Fallon", "20-Sep-20" = "George R. R. Martin",
        "21-Sep-20" = "Stephen King", "22-Sep-20" = "Andrea Bocelli","23-Sep-20" = "Bruce Springsteen",
        "24-Sep-20" = "F. Scott Fitzgerald", "25-Sep-20" = "Will Smith",
        "26-Sep-20" = "Serena Williams", "27-Sep-20" = "Lil Wayne", "28-Sep-20" = "Naomi Watts",
        "29-Sep-20" = "Kevin Durant", "30-Sep-20" = "T-Pain",
        "1-Oct-20" = "Zach Galifianakis"))+
  theme(
    axis.text.y = element_text(face = "plain", color = "#FF69B4", size = 8, angle = 180), 
    axis.text.x = element_text(face = "bold", color = "#FF0000", size = 8),
    axis.line = element_line(colour = "#00FFFF", size = 4, linetype = "dotdash", lineend = "round"),
    axis.title.x = element_text(color = "purple", size = 22, face = "bold"),
    axis.title.y = element_text(color = "#B19CD9", size = 30, face = "bold.italic"))+
  geom_dotplot(aes(x = Students))