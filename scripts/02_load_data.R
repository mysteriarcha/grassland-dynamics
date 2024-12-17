source("scripts/01_load_libraries.R")

#### Uploading Devin data

# 1. COMPOSITONAL ANALYSIS ------------------------------------------------
# Load data
Fevin <- read_xlsx("./data/composition/Fevin_2.xlsx")

# Define the ordinal scale of van der Maarel from the Braun-Blanquet scale 
Fevin <- 
  Fevin %>% 
  mutate(
    ord_cover = case_when(cover == "r" ~ 1,
                          cover == "+" ~ 2,
                          cover == "1" ~ 3,
                          cover == "2m" ~ 4,
                          cover == "2a" ~ 5,
                          cover == "2b" ~ 6,
                          cover == "3" ~ 7,
                          cover == "4" ~ 8,
                          cover == "5" ~ 9
    )
  )

# Make a function to have a metric scale of abundances. The value of 
# a is recommended by van der Maarel (see van der Maarel, 2007)
ord_to_metric_covers <- 
  function(x, 
           a = 1.415, 
           log = F){
  if(log) return((x - 2)/a)
  exp((x-2)/a)
}

## Transform the data
{
  # Set continuous cover values
  Fevin$cover <- round(ord_to_metric_covers(Fevin$ord_cover), 2)
  
  # Set dates in proper format:
  Fevin$date  <- paste(Fevin$year, Fevin$month, "01", sep = "/") 
  Fevin$date  <- as.Date(Fevin$date,format = "%Y/%B/%d")
  
  # Arrange and subset the data
  Fevin <-  
    Fevin %>% 
      arrange(date) %>% 
      mutate(
        species = as.factor(species),
        big_plot = str_extract(plot, "F."),
        small_plot = str_extract(plot, "P.")
      ) %>% 
      dplyr::select(-c(plot, remarks, ord_cover, month, year))  
  
  ## Get the number of time observations and its range
  n  <- length(unique(Fevin$date))
  .t <- setNames(1:n, unique(Fevin$date))
  
}

## Check species names:
all_spp <- sort(unique(Fevin$species))

common_spp <- 
  Fevin %>% 
  group_by(date, species) %>% 
  summarise(mean_cover = mean(cover, na.rm = T)) %>%
  group_by(species) %>%
  summarise(total_cover = sum(mean_cover)) %>%
  filter(total_cover > 4) %>% 
  .$species

rare_spp <- sort(setdiff(unique(Fevin$species), common_spp))

get_spp_obs <- function(df = Fevin, spp) df[df$species %in% spp, ]

## We see some problems: 
#  1) "Bromus japonicus" and "Bromus tectorum" appear only after 2022, Earlier 
#  there are only values of "Bromus sp.". 
#  We will aggregate them all  under this  name for uniformization and 
#  because of the functional similarity of these two species.
# get_spp_obs(spp = c("Bromus tectorum", "Bromus japonicus"))

## 2) Anthriscus cerefolium dissappears abruptly in 2021. We will leave it
#  like this but it can be an identification error
# get_spp_obs(spp = "Anthriscus cerefolium")

## 3) There is a "Boraginaceae" plant with quite remarkable abundances, 
#  possibly Echium vulgare. We will add its values to E. vulgare
# get_spp_obs(spp = "Boraginaceae")

## 4) There is an obseravtion each of "Echium vul" and "Echium vula".
#  Add their values to E. vulgare
# get_spp_obs(spp = c("Echium vul", "Echium vula"))

## 5) There is an "Euphorbia/Hypericum" detected on July 2024. 
#  Ignore it for now, but check in the pictures
# get_spp_obs(spp = "Euphorbia/Hypericum")

## 6) There are "indet", "Indet" and "Indet." species
# indet_obs <- which(Fevin$species %in% c('indet', 'Indet', 'Indet.'))
# Fevin[indet_obs, ]
#  However all of them are in the "rare_spp" vector so we will just drop them
#  by now. We could try to check the pictures

## 7) There is Muscari sp. As there are no more species of the genus, we
#  can work with it, but keep that in mind (Muscari comosum is grows abundantly
#  in the area)
# get_spp_obs(spp = "Muscari sp.")

## 8) There is Sonchus sp. (Only 1 in May 2024)
# get_spp_obs(spp = "Sonchus sp.")

## 9) There is Teucriu mcha. This one is in common_spp! 
# get_spp_obs(spp = "Teucriu mcha")
#  A single observation of May 2024 but with high abundance. 
#  Obviously it's  Teucrium chamaedrys

## 10) There is "Veronica sp." in (both) September(s) 2020 and 2022. Possible 
#  to check with the pictures?
# get_spp_obs(spp = "Veronica sp.")

