library(tidyverse)

### Problem 1 ###

numeric_vector <- c(1, 2, 2, 2, 3, 4, 5, 5, 5)

variance <- function(x) {
  (1 / (length(x) - 1)) * sum( (x - mean(x) ) ^2)
  }

skewness <- function(y) {
  ((1 / (length(y) - 2)) * sum((y - mean(y) ) ^3)) / (variance(y) ^(3/2))
  }

variance(numeric_vector)
skewness(numeric_vector)


### Problem 2 ###

na_vector_1 <- c(1, 2, 3, NA, 5, 6, NA)

na_vector_2 <- c(1, NA, 3, NA, 5, 6, 7)

na_vector_3 <- c(1, NA, 3, NA, 5, 6, NA)

both_na <- function(x,y) { 
  length(intersect( which(is.na(x)), which(is.na(y))))
  }

both_na(na_vector_1, na_vector_2)

both_na(na_vector_1, na_vector_3)

### Problem 3 ###

colony_counts <- read.csv("data/Practice_Dataset.csv")

scatter_plot <- function(z) {
  long_colony_counts <- pivot_longer(z,
               cols = starts_with("T"),
               names_to = "Time_Points", 
               values_to = "Cell_Density")
  
  ggplot(long_colony_counts, aes(x = Time_Points, y = Cell_Density))+
    geom_point()
}

scatter_plot(colony_counts)



### Problem 4 ###

vector_sorting <- function(z) {if (class(z) == "numeric") {
   z*2
  } else if (class(z) == "character"){
  sort(z)
  } else {
  return("Error: Vectors provided are neither numeric nor character vectors.")
  }
}

animals <- c('dog', 'cat', 'zebra', 'fish', 'antelope', 'buffalo', 'chicken')

vector_sorting(na_vector_1)
vector_sorting(na_vector_2)
vector_sorting(na_vector_3)
vector_sorting(animals)
vector_sorting(colony_counts)


### Problem 5 ###

mean_calculation <- function(x) {if (class(x) == "numeric"){
  return (mean(x))
  } else {
    warning("Error: Mean calculation was unsuccessful. Please ensure that the
    dataset of interest is strictly numeric.") 
  }
  
}

mean_calculation(numeric_vector)
mean_calculation(animals)


### Problem 6 ###

one_million_numbers <- sample(1000000)
one_more_million_numbers <- sample(1000000)

system.time(one_million_sums <- (one_million_numbers + one_more_million_numbers))

  
system.time(for (i in seq_along(one_million_numbers)) {
  for_loop_sum_2 <- (one_million_numbers[i] + one_more_million_numbers[i])
})


system.time(for (i in seq_along(one_million_numbers)) {
  (one_million_numbers[i] + one_more_million_numbers[i])
})

system.time(for (i in seq_along(one_million_numbers)) {
  (one_million_numbers + one_more_million_numbers)
})
system.time(one_million_sums <- (one_million_numbers + one_more_million_numbers))


