library(tidyverse)
library(data.table)
library(pacman)
library(httpgd)
hgd()
options(device = httpgd::hgd)
pacman::p_load(tidyverse, data.table, readxl)
print(starwars |> select(name:skin_color, species, -height))
print(mpg |> select(-hwy))
mpg |> select(mileage = hwy)
starwars |> 
    select(name, birth_year) |>
    mutate(dog_years = birth_year * 7) |>
    filter(dog_years > 250) |>
    print()
starwars |> 
    group_by(species, gender) |>
    summarise(ave_height = mean(height, na.rm = TRUE)) |>
    print()
mpg |>
    group_by(manufacturer, year) |>
    tally()

mpg |> 
    ggplot() +
    aes(x = displ, y = cty, color = class) +
    geom_point() +
    labs(title = "Engine Displacement vs City MPG",
         x = "Engine Displacement (L)",
         y = "City MPG",
         color = "Vehicle Class") +
    theme_minimal() |>
    print()