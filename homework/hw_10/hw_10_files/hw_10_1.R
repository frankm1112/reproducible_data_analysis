library(tidyverse)

animal_information <- as.data.frame(read.csv("data/Pet_Info.csv"))

class_creation <- function(df, user) {
  if (! class(df) == "data.frame") {
    warning("Error: Data provided is not in a data.frame format.")
  }
  else if (! class(user) == "character"){
    warning("Error: User provided is not given as a character value.")
  }
  else { attr(df, "user") <- user
  structure(df, class = c("pet_description", "data.frame"))
  }
}

new_class <- class_creation(animal_information, user = "Frank May")

class(new_class)


summary.pet_description <- function(df) {
  print(paste0("Data Filed by: ", attr(df, "user"), sep = " "))
  df_summ <- summary.data.frame(df)
  df_summ
}

summary(new_class)


