library(tidyverse)
library(modelr)

diamonds_data <- diamonds

carat_price <- lm(price ~ carat, diamonds_data)


###-------------------------------------------
### Carat is y value
### Price is x value
### Intercept is b value
### Returned price value is m value.
###-------------------------------------------

carat_price_coefficients <- carat_price$coefficients

carat_cost_equation <- function(x,y) {
  (x - (y [1]))/(y [2])
}

carat_value <- 1

carat_cost_equation(carat_value,coefficients)

carat_and_color_price <- lm(price ~ carat + color, diamonds_data)

carat_and_color_price_coefficients <- carat_and_color_price$coefficients

carat_cost_equation(carat_value, carat_and_color_price_coefficients)

carat_and_chr_color_price <- lm(price ~ carat + as.character(color), diamonds_data)

carat_and_chr_color_price_coefficients <- carat_and_chr_color_price$coefficients

carat_cost_equation(carat_value, carat_and_chr_color_price_coefficients)

diamonds_data_color_character <- diamonds_data as.character(diamonds_data$color)
carat_and_color_price_character <- carat_and_color_price()

plot_1 <- ggplot(diamonds_data, aes(carat, price))+
  geom_point(size = 1, colour = "D3D3D3")+
  geom_abline(
    aes(intercept = carat_price$coefficients[1], 
        slope = carat_price$coefficients[2],
        colour = "0000FF")) +
  geom_abline(
    aes(intercept = carat_price$coefficients[1], 
        slope = (carat_price$coefficients[2] + carat_price$coefficients[3]),
        colour = "FF0000")) +
  geom_abline(
    aes(intercept = carat_and_chr_color_price$coefficients[1], 
    slope = (carat_price$coefficients[2] + carat_price$coefficients[3]),
    colour = "00FF00"))
  )
print(plot_1)
    

grid <- carat_price %>%
  data_grid(carat)