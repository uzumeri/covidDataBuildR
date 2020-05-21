library(tidyverse)
library(choroplethrMaps)

fplot <- function(ds,v) {
  p <-ggplot(ds, aes(date,v)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ state)
  plot(p)
}
fplotns <- function(ds,v) {
  p <-ggplot(ds, aes(date,v)) +
    geom_point(color="Red") +
    facet_wrap(~ state)
  plot(p)
}

X2020_01_11_social_distancing$statefips <- str_sub(X2020_01_11_social_distancing$origin_census_block_group, 1, 2)
X2020_01_11_social_distancing$countyfips <- str_sub(X2020_01_11_social_distancing$origin_census_block_group, 3, 5)
X2020_01_11_social_distancing$tractfips <- str_sub(X2020_01_11_social_distancing$origin_census_block_group, 6, 11)
X2020_01_11_social_distancing$bgfips <- str_sub(X2020_01_11_social_distancing$origin_census_block_group, 12, 12)


