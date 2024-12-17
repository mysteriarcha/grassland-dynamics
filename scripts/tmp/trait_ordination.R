
source("load_libraries.R")
source("transform_data.R")
source("name_interactions.R")

## Trait ordination

pladias <- read_csv("~/Documents/Gonzalo R/PhD/phd_masaryk/data/traits/pladias_imputed_NRMSE_0.282781575341991.csv")
pladias <- dplyr::rename(pladias, species = ...1)

Devin_pladias           <- pladias[pladias$species %in% spp, ]
rownames(Devin_pladias) <- Devin_pladias$species
Devin_pladias$species   <- NULL

Devin_pladias <- 
  Devin_pladias[, c(
    # I: Life Forms
    "ChamPh", "GeoPh", "HemiPh", "TherPh", 
    # II: Phenology
    "LfGrSpring", "LfGrSummer", "LfGrWinter", "LfGrPersist", "FloweringLength",
    # III: Reproductive System
    "Dioecy", "AutoGam", "ZooGam", "AnemoGam",
    # IV: Size
    "Height",
    # V: LQTs
    "LA", "SLA", "LDMC",
    # VI: SeedMass
    "SeedMass",
    # VII: BudBank
    "BdBank_size", "BdBank_depth",
    # VIII: Clonality
    "Clon_persist", "Clon_multip", "LateralSpr", "ClonInd")
  ]
Devin_pladias$Height <- exp(Devin_pladias$Height)
Devin_pladias$SeedMass <- exp(Devin_pladias$SeedMass)
Devin_pladias %<>% 
  rename(
    "Chamephyte" = "ChamPh",
    "Geophyte"   = "GeoPh",
    "Hemicryptophyte" = "HemiPh",
    "Therophyte" = "TherPh",
    "Spring LG" = "LfGrSpring",
    "Summer LG" = "LfGrSummer",
    "Winter LG" = "LfGrWinter",
    "Persistent LG" = "LfGrPersist",
    "Flowering Length" = "FloweringLength",
    "Autogamy" = "AutoGam",
    "Zoogamy" = "ZooGam",
    "Anemogamy" = "AnemoGam",
    "Seed Mass" = "SeedMass",
    "Bud Bank Size" = "BdBank_size",
    "Bud Bank Depth" = "BdBank_depth",
    "Clonality Persistence" = "Clon_persist",
    "Clonality Multiplicity" = "Clon_multip",
    "Clonality Index" = "ClonInd"
  )

PCA_Devin_pladias <-
  prcomp(Devin_pladias, center = T, scale. = T)
PCA_Devin_hillsmith <-
  ade4::dudi.hillsmith(Devin_pladias, scannf = F, nf = NCOL(Devin_pladias))
summary(PCA_Devin_pladias)
summary(PCA_Devin_hillsmith)
PCA_scores <- scores(PCA_Devin_pladias) %>% as.data.frame()
PCA_scores$spp <- spp
PCA_loadings <- scores(PCA_Devin_pladias, display = "species") %>% as.data.frame()
PCA_percvar  <- PCA_Devin_pladias$sdev**2/sum(PCA_Devin_pladias$sdev**2)
spp
ggplot(PCA_scores, aes(x = PC1, y = PC2)) +
  # geom_point() +
  geom_segment(
    data = PCA_loadings, 
    aes(x=0,y=0,
        xend = PC1, yend = PC2),
        arrow = arrow(angle = 30, length = unit(0.25, "inches"),
                      ends = "last")) +
  theme_classic() 


