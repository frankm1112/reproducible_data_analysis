

colony_counts <- read.csv("data/Practice_Dataset_Non_numeric.csv")

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

rename(long_colony_counts, Pizza = contains('Time'))
class(colony_counts)
class(colony_counts[[1]])
class(colony_counts)

class(long_colony_counts$Cell_Density)

for (i in colnames(colony_counts)) {
  print(class(colony_counts[[i]]))
}

for (i in colnames(colony_counts)) {
  print (class(colony_counts[[i]]))
  if (! class(colony_counts[[i]] == "numeric")){
    warning("Error: Non-header row/column cell contains a non-numeric value. 
            Please only enter numeric values.")
  }

  else{
  }
}

ids <<- NULL

class(long_colony_counts$[1])



output <- class(i)
for (i in col){
  class(col(i)) 
}

for 
if (! class(df(col(i))) == "numeric") {
}
  else if (! class(user) == "character"){
    warning("Error: User provided is not given as a character value.")
  }
  else { attr(df, "user") <- user
  structure(df, class = c("pet_description", "data.frame"))
  }
}

for (columnName,)