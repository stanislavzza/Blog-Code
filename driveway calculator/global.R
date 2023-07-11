library(shiny)
library(tidyverse)
library(ggforce)

car_choices <- tribble(~Model, ~Wheelbase, ~Clearance,
                       "2010 Civic", 105.1, 6.7,
                       "2023 Prius", 108, 6,
                       "2023 Prius Prime", 106, 5.3,
                       "2023 Bolt EV", 102.4, 5.5,
                       "2023 Bolt EUV", 105.3, 5.5,
                       "Leaf", 106.3, 5.9,
                       "2023 Corrola Hybrid", 103, 5.3,
                       "2023 Corrola Cross Hybrid", 103.9, 8.1,
                       "2015 Chevy Spark", 93.5, 6,
                       "BMW i3", 101.2, 5.5,
                       "2005 Camry",107.1, 5.5,
                       "2022 Rav4 Hybrid", 105.9, 7.8)