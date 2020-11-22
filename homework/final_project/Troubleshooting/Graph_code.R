colony_counts <- read.csv("data/Practice_Dataset.csv")
scatter_plot <- function(z) {
  long_colony_counts <- pivot_longer(z,
                                     cols = starts_with("T"),
                                     names_to = "Time_Points", 
                                     values_to = "Cell_Density")
  
  ggplot(long_colony_counts, aes(x = Time_Points, y = Cell_Density))+
    geom_point()
}

long_colony_counts <- pivot_longer(colony_counts,
                                   cols = starts_with("T"),
                                   names_to = "Time_Points", 
                                   values_to = "Cell_Density")

print(long_colony_counts)

rename(long_colony_counts, Pizza = Time_Points)
