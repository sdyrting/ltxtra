## code to prepare aus_2021_2023 dataset

library(devtools)
library(tidyverse)
library(readxl)

infile <- "./data-raw/3302055001DO001_20212023.xlsx"

states <- tibble(
  state = c('NSW','VIC','QLD','SA','WA','TAS','NT','ACT','AUS'),
  age_range = 'A8:A108',
  sheet = paste0('Table ',seq(1,9)),
  male_range = 'B8:E108',
  female_range = 'F8:I108'
)

aus_2021_2023 <- tibble()

for (i in 1:nrow(states)) {
  age <- read_excel(infile,sheet=states$sheet[i],
                    range=states$age_range[i],
                    col_names = c('Age')) %>%
    mutate(OpenInterval=(Age == 100))
  
  lt_m <- read_excel(infile,sheet=states$sheet[i],
                     range=states$male_range[i],
                     col_names = c('lx','qx','Lx','ex')) %>%
    mutate(Sex='Male',State=states$state[i]) %>%
    bind_cols(age)
  lt_f <- read_excel(infile,sheet=states$sheet[i],
                     range=states$female_range[i],
                     col_names = c('lx','qx','Lx','ex')) %>%
    mutate(Sex='Female',State=states$state[i]) %>%
    bind_cols(age)
  
  aus_2021_2023 <- aus_2021_2023 %>%
    bind_rows(lt_f) %>%
    bind_rows(lt_m)

}

aus_2021_2023 <- aus_2021_2023 %>%
  group_by(State,Sex) %>%
  mutate(
    lx = 100000*cumprod(lag(1-qx,default=1)),
    dx = lx - lead(lx,default=0),
         .nx = lead(Age,default=Inf)-Age,
         ax = ifelse(OpenInterval,Lx/dx,(Lx-.nx*lead(lx,default=0))/dx),
         mx = dx/Lx,
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx/lx
         ) %>%
  select(State,Sex,Age,mx,qx,ax,lx,dx,Lx,Tx,ex,OpenInterval)


usethis::use_data(aus_2021_2023, overwrite = TRUE)
