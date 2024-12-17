
## Calculate Legendre's beta-diversity:

## We will take the absolute values for abundance. For relative values
#  the metric returns the same values for B and C, which doesn't make sense
#  for part of our analyses

TBI_matrices <- vector("list", n)
for(i in .t){
  TBI_matrices[[i]] <- 
    as.matrix(F_Devin_wide[F_Devin_wide$time == i,][1:length(common_spp)]) 
}

## Let's calculate sequentially (i.e. linearly, from time i to time i+1) the
#  beta-diversity
TBI_out <- data.frame(meanD = 0, B_den = 0, C_den = 0, p_value = 0)
for(i in .t[-n]){
  tmp <- adespatial::TBI(TBI_matrices[[i]], TBI_matrices[[i+1]], method = "ruzicka")
  # TBI_out$meanD[i] <- tmp$BCD.summary$`mean(D)`
  # TBI_out$B_den[i] <- tmp$BCD.summary$`mean(B/den)`
  # TBI_out$C_den[i] <- tmp$BCD.summary$`mean(C/den)`
  # TBI_out$p_value[i] <- tmp$t.test_B.C$p.param
  TBI_out[i, ] <- c(tmp$BCD.summary$`mean(D)`, tmp$BCD.summary$`mean(B/den)`,
                    tmp$BCD.summary$`mean(C/den)`, tmp$t.test_B.C$p.param)
  TBI_out$date <- names(.t)[-n]
}

TBI_out

## However, we could also have a matrix across the entire set of temporal
#  distances (i.e. not only comparing i to i+1 but 1 to n and all other
#  comparisons in between)

## We can do this by a double for loop, or better by indexing the possible
#  combinations of distances

t_combs <- as.data.frame(t(combn(.t, 2)))
t_combs$date1 <- as.Date(names(.t)[t_combs[,1]])
t_combs$date2 <- as.Date(names(.t)[t_combs[,2]])
t_combs$difftime <- t_combs$date2 - t_combs$date1

get_TBI <- function(mat_db = TBI_matrices, 
                    i, j, meth = "%difference"){
      tmp <- TBI(mat_db[[i]], mat_db[[j]], method = meth);
      B   <- tmp$BCD.summary$`mean(B/den)`;
      C   <- tmp$BCD.summary$`mean(C/den)`
      return(c(B, C))
}
get_TBI <- Vectorize(get_TBI, vectorize.args = c("i", "j"))

TBI_global <- get_TBI(i = t_combs[[1]], j = t_combs[[2]])
TBI_global <- t(TBI_global)
TBI_global <- data.frame("B" = TBI_global[,1], 
                         "C" = TBI_global[,2],
                         "D_1" = rowSums(TBI_global),
                         "D_2" = TBI_global[,2] - TBI_global[,1],
                         "difftime" = t_combs$difftime
                         )

centr_BC <- c(centroidr::centroid(TBI_global$B), centroidr::centroid(TBI_global$C))
centr_intercept <- centr_BC[2] - centr_BC[1]
TBI_global %>% 
  mutate(D_2 = C - B) %>% 
  ggplot(aes(x = difftime, y = D_2)) +
  geom_point() +
  theme_classic()

p_BC_plot <-
  TBI_global %>% 
  ggplot(aes(x = B, y = C, alpha = difftime)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  geom_density2d(alpha = .25)+
  geom_point(x=centr_BC[1], y = centr_BC[2], size = 6, color = "red",
             shape = 4) +
  geom_abline(slope = 1, intercept = centr_intercept,
              color = "royalblue") +
  theme_classic()

TBI_global$fitted <-
  TBI_global %>% 
  mutate(GvsL = ifelse(C > B, 1L, 0L)) %>% 
  glm(GvsL ~ difftime, family = "binomial", data = .) %>% 
  predict(type="response")
TBI_global %>% 
  mutate(GvsL = ifelse(C > B, 1L, 0L)) %>% 
  ggplot(aes(x = difftime, y = GvsL)) +
  geom_point(alpha = .1) +
  geom_line(aes(y = fitted), color = "royalblue") +
  theme_classic()
