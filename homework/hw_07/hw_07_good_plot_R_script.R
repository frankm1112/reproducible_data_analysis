library(tidyverse)
UTK_Covid_Data <- read_csv("data/UTK_Covid_data.csv")

ggplot(UTK_Covid_Data, aes(x = Date, y = Students))+
  ggtitle("UTK Student Coronavirus Cases") +
  theme(axis.text.x=element_text(face="plain", color = "#0000CD", size=8, angle=90), 
        plot.title = element_text(color ="black", size = 14, face= "bold", hjust=0.5))+
  scale_x_discrete(name = "Date",
                   limits=c("12-Aug-20","13-Aug-20","14-Aug-20","15-Aug-20","16-Aug-20",
                            "17-Aug-20","18-Aug-20","19-Aug-20","20-Aug-20","21-Aug-20",
                            "22-Aug-20","23-Aug-20","24-Aug-20","25-Aug-20","26-Aug-20",
                            "27-Aug-20","28-Aug-20","29-Aug-20","30-Aug-20","31-Aug-20",
                            "1-Sep-20","2-Sep-20","3-Sep-20","4-Sep-20","5-Sep-20",
                            "6-Sep-20","7-Sep-20","8-Sep-20","9-Sep-20","10-Sep-20",
                            "11-Sep-20","12-Sep-20","13-Sep-20","14-Sep-20","15-Sep-20",
                            "16-Sep-20","17-Sep-20","18-Sep-20","19-Sep-20","20-Sep-20",
                            "21-Sep-20","22-Sep-20","23-Sep-20","24-Sep-20","25-Sep-20",
                            "26-Sep-20","27-Sep-20","28-Sep-20","29-Sep-20","30-Sep-20",
                            "1-Oct-20")) +
  scale_y_continuous(name="Active Student Cases")+
  theme(axis.line = element_line(colour="black", size = 1, linetype= "solid")) +
  geom_point()