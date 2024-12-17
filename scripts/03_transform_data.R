source("scripts/02_load_data.R")

## One problem we face with the original dataframe is that we cannot
#  aggregate easily the abundances accross scales. Tha's because in the df
#  only the observations higher than 0 (presence) are recorded, not the
#  absences. This makes the scaling different for species that are very common
#  than from those which are rare.
## We have to standardize the dataframe to include the same number of 
#  observations (6*7*time) for all the species.


## 1) Set the range of values for which we have observations:
Fevin$time <- .t[as.character(Fevin$date)]

## 2) Get a table only with reasonably common species
Fevin_common <- Fevin[Fevin$species %in% common_spp, ]

## 3) Make a matrix where all values of the sampling design will be represented,
#  not only those recorded in the process
Fscale_mat_abundances <- matrix(0, nrow = n*7*6, ncol = length(common_spp))
colnames(Fscale_mat_abundances) <- common_spp

## 4) Make a function to fill in this matrix with the abundance values
#  and return a data frame
replenish <- function(mat = Fscale_mat_abundances,
                      df  = Fevin_common){
  
  spp <- unique(df$species)
  
  time_space_combinations <-
    expand.grid(
      time       = .t, 
      big_plot   = unique(df$big_plot),
      small_plot = unique(df$small_plot)
    ) %>% 
    arrange(time, big_plot)
  
  ind_locs <-
    apply(
      time_space_combinations,
      1, 
      paste0,
      collapse = "_"
    )
  
  for(i in spp){
    df_ <- df[df$species == i, ]
    df_locs <-
      df_[c("time", "big_plot", "small_plot")] %>% 
      apply(
        1,
        paste0, 
        collapse = "_"
      )
    mat_locs <- ind_locs %in% df_locs
    mat[mat_locs, i] <- df_$cover
  }
  
  for(i in 1:length(mat)) if(is.na(mat[i])) mat[i] <- 0
  
  mat            <- as.data.frame(mat)
  mat$time       <- time_space_combinations$time
  mat$big_plot   <- time_space_combinations$big_plot
  mat$small_plot <- time_space_combinations$small_plot
  
  return(mat)

  }

## 6) Evaluate the function and make relative abundance versions
P_Devin_wide      <- replenish()

## Make the uniformizations found at the end of load_data.R object:
P_Devin_wide %<>% 
  mutate(`Bromus sp.` = `Bromus sp.` + `Bromus japonicus` + `Bromus tectorum`,
         `Echium vulgare` = `Echium vulgare` + Boraginaceae,
         `Teucrium chamaedrys` = `Teucrium chamaedrys` + `Teucriu mcha`
         ) %>% 
  select(-c(`Boraginaceae`, `Bromus japonicus`, `Bromus tectorum`, 
            `Teucriu mcha`)
         )

# Redefine the number of common spp
n_common_spp <- NCOL(P_Devin_wide) - 3

## Now make the data in relative abundances:
{
  P_Devin_wide_prop <-
    apply(P_Devin_wide[1:(ncol(P_Devin_wide)-3)], 1, function(x) x/sum(x)) %>% 
    t() %>% 
    as.data.frame()
  
  P_Devin_wide_prop$time       <- P_Devin_wide$time
  P_Devin_wide_prop$big_plot   <- P_Devin_wide$big_plot
  P_Devin_wide_prop$small_plot <- P_Devin_wide$small_plot
  }

## 7) Obtain the wide-format aggregates at F and T scales:
F_Devin_wide <- 
  aggregate(P_Devin_wide[1:(ncol(P_Devin_wide)-3)], 
            by  = list(P_Devin_wide$time, P_Devin_wide$big_plot),
            FUN = mean) %>%
  .[-c(1,2)]

F_Devin_wide_prop <-
  aggregate(P_Devin_wide_prop[1:(ncol(P_Devin_wide)-3)], 
            by  = list(P_Devin_wide_prop$time, P_Devin_wide_prop$big_plot),
            FUN = mean) %>%
  .[-c(1,2)]

F_Devin_wide$time     <- F_Devin_wide_prop$time <- rep(1:36, 7)
F_Devin_wide$big_plot <- F_Devin_wide_prop$big_plot <- rep(1:7, each = n)

T_Devin_wide <-
  aggregate(P_Devin_wide[1:(ncol(P_Devin_wide)-3)], 
            by = list(P_Devin_wide$time),
            FUN = mean) %>% 
  .[-1]

T_Devin_wide_prop <-
  aggregate(P_Devin_wide_prop[1:(ncol(P_Devin_wide)-3)], 
            by = list(P_Devin_wide$time),
            FUN = mean) %>% 
  .[-1]

T_Devin_wide$time <- T_Devin_wide_prop$time <- .t

## 8) Switch the P_Scale matrix to long form
P_Devin_long <-
  P_Devin_wide %>% 
  pivot_longer(1:(ncol(P_Devin_wide)-3), names_to = "species") %>% 
  arrange(species) %>% 
  mutate(date = names(.t)[time])
P_Devin_long_prop <-
  P_Devin_wide_prop %>% 
  pivot_longer(1:(ncol(P_Devin_wide)-3), names_to = "species") %>% 
  arrange(species) %>% 
  mutate(date = names(.t)[time])

# P_Devin_long_prop %>% 
#   group_by(time, big_plot, small_plot) %>% 
#   summarise(richness = sum(value > 0)) %>% 
#   ggplot(aes(x = time, y = richness, linetype = big_plot, color = small_plot))+
#   geom_line() +
#   theme_classic()


## 9) Get the rest of scales in long-format
F_Devin_long <-
    P_Devin_long %>% 
    group_by(species, time, date,big_plot) %>% 
    summarise(value = mean(value))
F_Devin_long_prop <-
  P_Devin_long_prop %>% 
  group_by(species, time, date,big_plot) %>% 
  summarise(value = mean(value))

T_Devin_long <-
    F_Devin_long %>% 
    group_by(species, time, date) %>% 
    summarise(value = mean(value))
T_Devin_long_prop <-
  F_Devin_long_prop %>% 
  group_by(species, time, date) %>% 
  summarise(value = mean(value))

## For the multispatial CCM (to get the causation network between the time series)
#  we need a list with the time series grouped in pairs. Furthermore, giving them
#  the structure adopted by the multispatialCCM and pttstability packages will make
#  work easier:

## Include NAs as breaking points between spatial replicates to make better 
#  compatibility with the multispatialCCM and pttstability packages:
F_ts <- vector("list", 7L)
for(i in 1:7){
  ind <- (1:7 == i)
  ind <- which(rep(ind, each = n))
  F_ts[[i]] <- if(i == 7) F_Devin_wide[ind, ] else rbind(F_Devin_wide[ind,], NA)
}
F_ts <- Reduce(rbind, F_ts)

## Make pairwise list of time series:
F_pw_ts <- vector("list", NROW(spp_grid))
for(i in seq_along(F_pw_ts)){
  F_pw_ts[[i]] <- F_ts[, spp_grid[i,]]
  if(i == length(F_pw_ts)) names(F_pw_ts) <- nms_interactions
}


{
  saveRDS(common_spp,   "./data/RDS/common_spp.RDS")
  saveRDS(T_Devin_wide, "./data/RDS/T_Devin_wide.RDS")
  saveRDS(T_Devin_long, "./data/RDS/T_Devin_long.RDS")
  saveRDS(F_Devin_wide, "./data/RDS/F_Devin_wide.RDS")
  saveRDS(F_Devin_long, "./data/RDS/F_Devin_long.RDS")
  saveRDS(P_Devin_wide, "./data/RDS/P_Devin_wide.RDS")
  saveRDS(P_Devin_long, "./data/RDS/P_Devin_long.RDS")
  }
