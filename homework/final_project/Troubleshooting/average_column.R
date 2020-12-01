



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


back_to_wide <- pivot_wider(long_colony_counts,
                            names_from = contains('Sample'),
                            values_from = contains('Cell'))

CBA_replicates <- back_to_wide %>%
  select(contains('CB-A'))
CBA_average <- as.data.frame(rowMeans(CBA_replicates)) %>%
  rename(
    Average = 'rowMeans(CBA_replicates)'
  )
CBA_all <- cbind(CBA_replicates, CBA_average)

CBD_replicates <- back_to_wide %>%
  select(contains('CB-D'))
CBD_average <- as.data.frame(rowMeans(CBD_replicates)) %>%
  rename(
    Average = 'rowMeans(CBD_replicates)'
  )
CBD_all <- cbind(CBD_replicates, CBD_average)

Coculture_replicates <- back_to_wide %>%
  select(contains('Coculture'))
Coculture_average <- as.data.frame(rowMeans(Coculture_replicates)) %>%
  rename(
    Average = 'rowMeans(Coculture_replicates)'
  )
Coculture_all <- cbind(Coculture_replicates, Coculture_average)




