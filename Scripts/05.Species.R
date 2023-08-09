#### Biomass time Series of main species ####
#Goal here is to observe fluctuations in biomass of the species that 
#contributed the most to changes in community level.

#### Load data ####
Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")
# Simper <- readRDS("R_Objects/simper.rds") #Important species
traits <- readODS::read_ods("Data/traits_210606.ods")
Data <-readRDS("R_Objects/Data.rds") #Biomass data
# Dt_boot <- readRDS("R_Objects/Dt_boot.rds")

library(magrittr)
##### Make data into a data.frame ####
Dt <- plyr::rbind.fill(Map(function (x, y) {
  # Create the data.frame
  d <- data.frame(x[["pred"]], x[["mass"]])
  # Melt species biomass into a single column
  d <- reshape2::melt(d, id.var = c("year", "sites", #"boot",
                                    "depth", #"c_depth",
                                    'obsvr', "transect_id",
                                    "lat", "lon"))
  # Change colnames 
  colnames(d) <- c("Year", "Sites", #"Boot",
                   "Depth", #"Env",  
                   'Observer', "Transect", 
                   "Lat", "Lon", 
                   "Species", "Biomass")
  
  s <- data.frame(x[["pred"]], x[["size"]])
  
  # Add size to the data.frame
  d$Size <- reshape2::melt(s, id.var = c("year", "sites", #"boot",
                                         "depth", #"c_depth",
                                         'obsvr', "transect_id", 
                                         "lat", "lon")) %>%
    dplyr::pull(value)
  
  #Add abundance to the data.frame
  a <- data.frame(x[["pred"]], x[["abun"]])
  a <- reshape2::melt(a, id.var = c("year", "sites", # "boot", 
                                    "depth", #"c_depth",
                                    'obsvr', "transect_id", 
                                    "lat", "lon"))  
  d$Abun <- a %>% dplyr::pull(value)
  
  d$Island <-  rep(y, nrow(d))
  d
}, Data, names(Data)))



#How many censuses are available at each site?
data.frame(Dt %>%
  dplyr::group_by(Island, Sites, Observer) %>%
  dplyr::summarise(dplyr::n_distinct(Transect)))

# Change characters for factors
Dt[] <- lapply(Dt, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else x
})

# Dt$Year <- as.numeric(as.character(Dt$Year))
# 
# Dt$Group <- ifelse(traits$Planctivorous == 1, "plank_2nd",
#                    paste(ifelse(traits$MeanSize < 15,
#                                 "Small", "Large"),
#                          ifelse(traits$FoodTroph < 3, "1st",
#                                 ifelse(traits$FoodTroph >= 4,"3rd",
#                                        "2nd")), sep = "_"))[match(Dt$Species,
#                                                        traits$code)]
# 
# #Check which species belong to each group
# data.frame(Dt %>% dplyr::group_by(Group, Species) %>% dplyr::summarise()) %>% 
#     dplyr::group_by(Group) %>% dplyr::summarise(data.table::uniqueN(Species))

# Add group category
Dt$Group <- data.frame(groups = paste(ifelse(traits$MeanSize < 15, 
                                             "Small", "Large"), 
                                      ifelse(traits$FoodTroph < 3, "1st", 
                                             ifelse(traits$FoodTroph >= 4,"3rd",
                                                    "2nd")), sep = "_"),
                       code = traits$code)[match(Dt$Species,
                                                 traits$code), 'groups']

#Check which species belong to each group
Dt %>% dplyr::group_by(Group) %>% 
             dplyr::summarise(Species = data.table::uniqueN(Species))

#Calculate mean Abundance, Biomass and Size
Dtbio <- Dt %>% dplyr::group_by(Island, Sites, Observer, 
                                Transect, Group, Species) %>%
  dplyr::summarise(Abundance = mean(Abun),
                   Biomass = mean(Biomass), 
                   Size = mean(Size, na.rm = TRUE)) %>%
  dplyr::group_by(Island, Sites, Group, Species) %>%
  dplyr::summarise(Abundance = mean(Abundance),
                   Biomass = mean(Biomass), 
                   Size = mean(Size, na.rm = TRUE)) %>%
  dplyr::group_by(Island, Group, Species) %>%
  dplyr::summarise(Abundance = mean(Abundance),
                   Biomass = mean(Biomass), 
                   Size = mean(Size, na.rm = TRUE))

#Total Island Biomass
Dtbio %>% dplyr::group_by(Island) %>% dplyr::summarise(Biomass = sum(Biomass)/40, 
                                                       Abundance = sum(Abundance)/40,
                                                       Size = mean(Size))
#Biomass per group
Dtbio %>% dplyr::group_by(Island, Group) %>% 
  dplyr::summarise(Biomass = sum(Biomass)/40, 
                   Abundance = sum(Abundance)/40,
                   Size = mean(Size))

# #Subset for 4 species with greatest biomass within each group
# Dtbio <- plyr::rbind.fill(by(Dtbio, list(Dtbio$Island, Dtbio$Group), function(x) {
#   x <- x[order(x$biomass, decreasing = TRUE),]
#   x[1:4,]
# }))
# 
# #Take rows with NAs out
# Dtbio <- Dtbio[complete.cases(Dtbio),]
# 
# #Retained biomass
# Dtbio %>% dplyr::group_by(Island) %>% dplyr::summarise(sum(biomass))
# #Retained biomass per group
# Dtbio %>% dplyr::group_by(Island, Group) %>% dplyr::summarise(sum(biomass))
# 
# #Order variables
# Dt <- dplyr::arrange(Dt, Island)
# Dtbio <- dplyr::arrange(Dtbio, Island)
# 
# #Subset species on the dataset
# Dt <- plyr::rbind.fill(Map(function(x, y) {
#   x[x$Species %in% droplevels(y$Species),]
# }, x = split(Dt, Dt$Island), y = split(Dtbio, Dtbio$Island)))


# Add non-sampled species-level traits to the data
Dt <- Dt %>%
  dplyr::left_join(dplyr::select(traits, Species, code,
                                 shape, AspectRatio, FoodTroph),
                   by = c("Species" = "code")) %>%
  dplyr::relocate(Island, Year, Sites, Lat, Lon, Observer, 
                 Transect, Depth, Species, Species.y, Group)


#Add species mean abundance, mass and size within each island as traits
Dt <- dplyr::left_join(Dt, Dtbio, suffix = c("", "_mean"),
                       by = c("Island" = "Island",
                              "Group" = "Group",
                              "Species" = "Species"))
rm(list = ls()[!ls() %in% c("Dt", "Islands", "traits")])

######  Observe data ####
#Observe data before running the model
require(magrittr); library(ggplot2)


# ##### Biomass as a function of Predictors ####
# 
# #Years
# Dt %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   # plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#   #                                           "Large_1st", "Small_2nd", "Small_1st")) %>%
#   #Call a new plot
#   ggplot(aes(x = as.factor(Year- 2000), y = Biomass + 1)) +
#   geom_violin(trim = TRUE, scale = "width", draw_quantiles = c(.25, .5, .75)) +
#   #Add points
#   # geom_jitter(aes(color = Species), width = 0.25, alpha = 0.1) +
#   #Produce smoothed tendencies for each site
#   # geom_smooth(size = 0.8, se = FALSE) + 
#   #Split each island
#   facet_wrap(vars(Island),
#                       ncol = 4,
#                       scales = 'free_x') + 
#   #Scale y axis to log and clean background
#   scale_y_log10() + 
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank(),
#         legend.position = 'none')
# 
# 
#   
# #Sites
# Dt %>%
#  #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   # plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#   #                                           "Large_1st", "Small_2nd", "Small_1st")) %>%
#   #Call a new plot
#   ggplot(aes(x = Sites, y = Biomass + 1)) +
#   geom_violin(trim = TRUE, scale = "width", draw_quantiles = c(.25, .5, .75)) +
#   #Add points
#   # geom_jitter(aes(color = Species), width = 0.25, alpha = 0.1) +
#   #Produce smoothed tendencies for each site
#   # geom_smooth(size = 0.8, se = FALSE) + 
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free_x') + 
#   #Scale y axis to log and clean background
#   scale_y_log10(limits = c(.99,NA)) + 
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank(),
#         legend.position = 'none')
# 
# 
#   
# # Depth 
# Dt %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(
#     Group, "Large_3rd", "Large_2nd", "Large_1st", "Small_2nd", "Small_1st")) %>%
#   #Call a new plot
#   ggplot(aes(x = Depth, y = Biomass + 1)) +
#   #Add points
#   geom_point(aes(color = Species), alpha = .3) +
#   #Produce smoothed tendencies for each site
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free_x') + 
#   #Scale y axis to log and clean background
#   scale_y_log10(limits = c(.99,NA)) + 
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank(),
#         legend.position = 'none')
#   
#   # patchwork::plot_layout(ncol = 1, guides = 'collect')
# 
# 
# 
# 
# 
# 
# #Trend individual sites
# 
# Dt %>%
#   plyr::mutate(Year = as.numeric(as.character(Year))) %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   # plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#   #                                           "Large_1st", "Small_2nd", "Small_1st")) %>%
#   #Call a new plot
#   ggplot(aes(x = Year - 2000, y = Biomass + 1, 
#              group = interaction(Species, Sites),
#              color = Species)) +
#   #Add points
#   # geom_jitter(width = 0.25, alpha = 0.1) +
#   #Produce smoothed tendencies for each site
#   geom_line(stat = "smooth", alpha = .5, se = FALSE) + 
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free_x') + 
#   #Scale y axis to log and clean background
#   scale_y_log10(limits = c(1,NA)) + 
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank()) 
# 
# Dt %>%
#   plyr::mutate(Year = as.numeric(as.character(Year))) %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   #Call a new plot
#   ggplot(aes(x = Biomass + 1)) +
#   geom_density() +
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free') + 
#   #Scale y axis to log and clean background
#   scale_x_log10(limits = c(1,NA)) +
#   # scale_x_continuous(breaks = 06:19)+
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank())
# 
# Dt %>%
#   plyr::mutate(Year = as.numeric(as.character(Year))) %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   # dplyr::filter(Biomass > 0) %>%
#   #Call a new plot
#   ggplot(aes(x = Biomass + 1, y = Species)) +
#   ggridges::geom_density_ridges() +
# 
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free') + 
#   #Scale y axis to log and clean background
#   scale_x_log10(limits = c(1,NA)) +
#   # coord_cartesian(xlim = c(1, 10000)) +
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank())




#### LM categorical year ####
library(lme4)
Dt$Year <- as.factor(Dt$Year)
Mod1 <- lmer(log1p(Biomass) ~ Island * Species * Year +
             Species * Depth +
             (1 + Island | Sites ) +
             (1 | Observer),
             data = Dt, REML = FALSE,
             control = lmerControl(
             calc.derivs = FALSE,
             optimizer ='bobyqa'))
 
dir.create("Output")
saveRDS(Mod1, "Output/Mod1.rds")
Mod1 <- readRDS("Output/Mod1.rds")

# Check parameters 
summary(Mod1)
anova(Mod1)
performance::r2_nakagawa(Mod1)

##### Predict Categorical GLMM Outputs #####
library(magrittr)
pred <- data.frame(Dt %>%
                     dplyr::group_by(Island, Year, Group, Species) %>%
                     dplyr::summarise(Depth = 5, Observer = "anderson_batista"))

#Add a custom depth
pred$Depth <- ifelse(pred$Island == "rocas", 2,
                     ifelse(pred$Island == "stpauls_rocks", 10,
                            ifelse(pred$Island == "trindade", 10,
                                   ifelse(pred$Island == "noronha", 10,0))))

pred <- dplyr::arrange(pred, Island, Species, Year)

# #Add an observer
# pred$Observer <- c("gugaw_ferreira", "carlos_ferreira",
#                    "renato_morais", "hudson_pinheiro")[
#                      match(pred$Island, Islands)]
pred$Sites <- c("enseada", "cagarras", "barretinha", "calheta")[
                     match(pred$Island, Islands)]


pred

# #Predict species biomass plus C.I.. on each year
# library(doParallel)
# 
# nodes <- detectCores()
# cl <- makeCluster(nodes - 2)
# registerDoParallel(cl)
# 
# #Predict species biomass plus C.I.. on each year
# prd <- merTools::predictInterval(merMod = Mod1, 
#                                  newdata = pred,
#                                  level = 0.95, n.sims = 1000, which = "fixed",
#                                  stat = "median", type="linear.prediction",
#                                  fix.intercept.variance = TRUE,
#                                  include.resid.var = FALSE,
#                                  .parallel = TRUE)
# 
# stopCluster(cl)
# 
# saveRDS(prd, "Servidor/Output/merprd.rds")

prd <- readRDS("Servidor/Output/merprd2.rds")

pred <- data.frame(pred, exp(prd))



#Add Trophic level
pred$Troph <- traits$FoodTroph[match(pred$Species,
                                     traits$code)]

#Add Size
pred$Size <- traits$MeanSize[match(pred$Species,
                                   traits$code)]

#Add species names
pred$Code <- pred$Species
pred$Species <- traits$Species[match(pred$Species, traits$code)]

#Arrange first by trophic level, then by site
pred <- pred %>%
  dplyr::arrange((Troph %/% 1), Size) %>%
  dplyr::mutate(id = factor(Species, levels = rev(unique(Species)))) 


#### Plot Trends #####
library(magrittr); library(ggplot2)
#stpauls_rocks


Plt <- lapply(Islands, function(i, pred) {
  pred %>%
    dplyr::mutate(Year = as.factor(as.numeric(as.character(Year)) - 2000)) %>%
    #Change Islands names and edit groups order in smoothed tendencies
    dplyr::filter(Island == i) %>%
    #Call a new plot
    ggplot(aes(x = Year, y = fit,
               color = id, fill = id, group = id)) + 
    scale_color_hue(h = c(0,300)) +
    scale_fill_hue(h = c(0,300)) +
    
    #Add SE background from gam with less knots
    geom_ribbon(aes(ymin = lwr,
                    ymax = upr,
                    colour = NULL),
                alpha = 0.1) +
    #Add line from gam with less knots
    geom_line(aes(y = fit), alpha = 1, linewidth = 0.6) +
    geom_point(aes(y = fit), alpha = .7) +
    #Scale y axis
    scale_y_log10() +
    #Split Islands
    facet_wrap(vars(id),
               scales = 'free', ncol = 4, dir = "h") +
    #Make a White background
    theme(panel.grid.major.x = element_line(size=.05,
                                            color = rgb(0.5,0.5,0.5, 0.3)),
          panel.grid.major.y = element_line(size=.05,
                                            color = rgb(0.5,0.5,0.5, 0.1)),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "gray", 
                                      fill=NA, size=.5),
          legend.position = "none") +
    #Labels
    labs(#title = "Mean biomass through time",
      x = "Time Series", 
      y = "Biomass (g/m² * 40)")
}, pred)
  
  
pred %>% dplyr::group_by(Island) %>% 
  dplyr::summarise(Species = (round(data.table::uniqueN(id) / 4 + 0.3))*1.5)

Plt[[1]]
Plt[[2]]
Plt[[3]]
Plt[[4]]




#### Similarity on species trends ###
# Compare species on fluctuation similarity using a 2D NMDS

Sim <- by(pred[, c("Island", "Code", "Year", "fit")],
   list(pred$Island), function(x) {
     d <- x %>% reshape2::dcast(Island + Code ~ Year, value.var = "fit")
     d <- `rownames<-`(d[, !colnames(d) %in% c("Island", "Code")], d$Code)
     # vegan::metaMDS(d, distance = "mahalanobis", autotransform = TRUE,
     #                try = 30, trymax = 1000, k = 2)
     # k <- plyr::ldply(split(x, droplevels(x$Code)), function(i) {
     #   coef(lm(scale(log(fit)) ~ scale(as.numeric(Year)), data = i))
     # }, .id = "Code")
     # # colnames(k) <- c("Code", "Int", "Trend")
     # k <- data.frame(k, Island = rep(unique(x$Island), nrow(k)))
     # dplyr::left_join(x, k, by = c("Code" = "Code", Island = "Island"))
     k <- kmeans(vegan::wisconsin(d), 10)$cluster
     k <- data.frame(k = k, Spp = names(k),
                     Island = rep(unique(x$Island), length(k)))

     dplyr::left_join(x, k, by = c("Code" = "Spp", Island = "Island"))
})

Sim <- plyr::rbind.fill(Sim)

# Sim$trend <- unlist(lapply(by(Sim$Trend, Sim$Island,Hmisc::cut2, g = 10), as.numeric))
Sim %>%
  dplyr::mutate(Year = as.factor(as.numeric(as.character(Year)) - 2000)) %>%
  #Call a new plot
  ggplot(aes(x = Year, y = fit,
             color = Code, fill = Code, group = Code)) + 
  scale_color_hue(h = c(0,300)) +
  scale_fill_hue(h = c(0,300)) +
  
  #Add SE background from gam with less knots
  # geom_ribbon(aes(ymin = lwr,
  #                 ymax = upr,
  #                 colour = NULL),
  #             alpha = 0.1) +
  #Add line from gam with less knots
  geom_line(aes(y = fit), alpha = 1, size = 0.6) +
  geom_point(aes(y = fit), alpha = .7) +
  ggrepel::geom_text_repel(aes(label = Code),
                           data = Sim %>%
                             dplyr::group_by(Island, Code) %>%
                             dplyr::slice_sample() %>%
                             dplyr::mutate(Year = as.factor(
                               as.numeric(as.character(Year)) - 2000))) +
  #Scale y axis
  scale_y_log10() +
  #Split Islands
  facet_grid(vars(Island), vars(k),
             scales = 'free') +
  #Make a White background
  theme(panel.grid.major.x = element_line(size=.05,
                                          color = rgb(0.5,0.5,0.5, 0.3)),
        panel.grid.major.y = element_line(size=.05,
                                          color = rgb(0.5,0.5,0.5, 0.1)),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray", 
                                    fill=NA, size=.5),
        legend.position = "none") +
  #Labels
  labs(#title = "Mean biomass through time",
    x = "Time Series", 
    y = "Biomass (g/m² * 40)")


selec <- data.frame(Island = "noronha",
           Slide = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
           Spp = c("sph_bar", "cep_ful", "ani_sur", 
                   "tha_nor", "chr_mul", "cep_fur",
                   "hol_ads", "myr_jac", "het_cru",
                   "spa_fro", "spa_axi", "spa_amp"))

selec <- data.frame(Island = "stpauls_rocks",
                    Slide = c(1, 1,1, 2, 2, 2, 3, 3),
                    Spp = c("car_lug", "car_cry", "mel_nig",
                            "can_mac", "alu_scr", "oph_tri",
                            "can_suf", "aul_str"))
dplyr::left_join(pred %>% 
  dplyr::filter(Island %in% selec$Island & Code %in% selec$Spp), selec,
  by = c(Island = "Island", "Code" = "Spp")) %>%
  dplyr::mutate(Year = as.factor(as.numeric(as.character(Year)) - 2000)) %>%
  #Call a new plot
  ggplot(aes(x = Year, y = fit,
             color = Code, fill = Code, group = Code)) + 
  scale_color_hue(h = c(0,300)) +
  scale_fill_hue(h = c(0,300)) +
  
  #Add SE background from gam with less knots
  # geom_ribbon(aes(ymin = lwr,
  #                 ymax = upr,
  #                 colour = NULL),
  #             alpha = 0.1) +
  #Add line from gam with less knots
  geom_line(aes(y = fit), alpha = 1, size = 0.6) +
  geom_point(aes(y = fit), alpha = .7) +
  ggrepel::geom_text_repel(aes(label = Code),
                           data = dplyr::left_join(
                             pred %>% 
                               dplyr::filter(Island %in% selec$Island &
                                               Code %in% selec$Spp), selec,
                             by = c(Island = "Island", "Code" = "Spp")) %>%
                             dplyr::group_by(Island, Code) %>%
                             dplyr::slice_sample() %>%
                             dplyr::mutate(Year = as.factor(as.numeric(
                               as.character(Year)) - 2000))) +
  #Scale y axis
  scale_y_log10() +
  #Split Islands
  facet_grid(vars(Slide), vars(Island),
             scales = 'free') +
  #Make a White background
  theme(panel.grid.major.x = element_line(size=.05,
                                          color = rgb(0.5,0.5,0.5, 0.3)),
        panel.grid.major.y = element_line(size=.05,
                                          color = rgb(0.5,0.5,0.5, 0.1)),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray", 
                                    fill=NA, size=.5),
        legend.position = "none") +
  #Labels
  labs(#title = "Mean biomass through time",
    x = "Time Series", 
    y = "Biomass (g/m² * 40)")


data.frame(pred %>% dplyr::group_by(Island, Species) %>%
             dplyr::summarise(Size = mean(Size)),
           plyr::rbind.fill(lapply(lapply(MDS, vegan::scores), as.data.frame))) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = log(Size)),alpha = .8) +
  scale_color_viridis_c(option = "C") +
  ggrepel::geom_text_repel(aes(label = Species)) +
  facet_wrap(vars(Island)) +
  theme_bw() +
  theme(panel.grid = element_blank())










#### Net changes in species biomass #####
#Goal here is to test differences in biomass between the first and last 3 years
Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")

##### Filter for first and last n years ####
#Keep only the first and last 2 years
Dt_diff <- plyr::ldply(Islands, function(i, Data) {
  n = 3
  
  x <- Data[Data$Island == i, ]
  
  #Create a list of years to subset
  sbst <- sort(unique(x$Year))
  
  #Select the first and last 3 years
  sbst <- sbst[c(1:n, (length(sbst) - n + 1):length(sbst))]
  
  #Subset
  x <- x[x$Year %in% sbst, ]
  
  #Add a new vector to indicate 'begin' and 'end'
  x$Time <- x$Year
  x$Time[x$Year %in% sbst[1:n]] <- 'Begin'
  x$Time[x$Year %in% sbst[(n + 1):(2*n)]] <- 'End'
  
  x
}, Dt)

head(Dt_diff)


#Observe biomass distribution within groups
require(magrittr); library(ggplot2)

#Observe data priori distribution
Dt_diff %>% 
  ggplot(aes(x = Biomass + 1)) +
  geom_density() + 
  # coord_cartesian(xlim = c(1, 100)) + 
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  scale_x_log10() +
  theme_minimal()

Dt_diff %>% 
  ggplot(aes(x = Biomass + 1, y = Species)) +
  ggridges::geom_density_ridges() + 
  coord_cartesian(xlim = c(1, NA)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  scale_x_log10() +
  theme_minimal()
#Highly right skewed... Fit a gamma model (?)

# 
# #Add a trait
# traits <- readxl::read_excel("Data/traits_20200512.xlsx")
# plt_grps3$Size <- traits$MeanSize[match(plt_grps3$Groups, traits$code)]
# plt_grps3$Troph <- traits$FoodTroph[match(plt_grps3$Groups, traits$code)]
# 
# 
# plt_grps3$GTime <- paste0(plt_grps3$Groups, "_", plt_grps3$Time)
# 
# 




# Estimate computing time
#
# tm <- plyr::ldply(c(100, 200, 500, 1000, 2000), function(y, d) {
#   system.time(
#     robustlmm::rlmerRcpp(
#       #Model
#       log(Biomass + 1) ~  Species * Time + Species * Depth + (1 | Sites)
#       ,
#       #Data
#       data = d %>% dplyr::slice_sample(n = y)))
# }, d = Dt_diff[Dt_diff$Island == "noronha", ])
# 
# tm$n <- c(100, 200, 500, 1000, 2000)
# 
# predict(lm(user.self ~ poly(n, 2), data = tm), 
#         newdata = data.frame(n = c(stpauls_rocks = 7400, noronha = 11440, 
#                                    rocas = 18360, trindade =32680)))
# 
# # stpauls_rocks       noronha         rocas      trindade 
# #      91.32241     200.29754     483.87690    1459.38371 (seconds)
# 
# Gonna take ~24 minutes to compute if processed in parallel
# Require 12 GB of ram memory - Create swap partition of size at least 16 GB

####  Run the after vs before model ####
# Apply a glm model (log(biomass) ~ preds) to filter env. var. effects
Dt_diff$SppTime <- paste0(Dt_diff$Species, "_", Dt_diff$Time) 

glmdiff <- parallel::mclapply(Islands, function(x, d){
  
 # library(lme4); library(magrittr)
  

  system.time(
    
    g1 <- lm(
      #Model
      log(Biomass + 1) ~  SppTime + Species*Depth + Species*Sites
      ,
      # family = Gamma(link = "identity"), 
      # start = if(x == "stpauls_rocks") {rep(1, 100)} else {
      #           if(x == "noronha") {rep(1, 170)} else {
      #             if (x == "rocas") {rep(1, 286)} else{
      #               if (x == "trindade") {rep(1, 342)}}}},
        
      #Data
      data = d[d$Island == x, ]))
  
  g1
}, Dt_diff, mc.cores = 4); names(glmdiff) <- Islands


##### Check residuals frequency distribution ####
#Residual distribution
data.frame(Dt_diff, 
           plyr::ldply(glmdiff, function(x) data.frame(Resid = resid(x)))) %>%
  ggplot(aes(x = Resid)) +
  geom_density() + 
  # coord_cartesian(xlim = c(-1, 1)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  # scale_x_log10() +
  theme_minimal()

data.frame(Dt_diff, 
           plyr::ldply(glmdiff, function(x) data.frame(Resid = resid(x))),
           plyr::ldply(glmdiff, function(x) data.frame(Pred = predict(x)))) %>%
  ggplot(aes(x = Resid, y = Pred)) +
  geom_point(alpha = .3, color = "grey") + 
  # coord_cartesian(xlim = c(-1, 1)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  # scale_x_log10() +
  theme_minimal()

#Residual per species
data.frame(Dt_diff, 
           plyr::ldply(glmdiff, function(x) data.frame(Resid = resid(x)))) %>%
  ggplot(aes(x = Resid, y = Species)) +
  ggridges::geom_density_ridges() + 
  coord_cartesian(xlim = c(-2, 2)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  # scale_x_log10() +
  theme_minimal()

#Residual vs fitted, painted by fish trophic level
data.frame(Dt_diff, 
           plyr::ldply(glmdiff, function(x) data.frame(Resid = resid(x))),
           plyr::ldply(glmdiff, function(x) data.frame(Pred = predict(x)))) %>%
  ggplot(aes(y = Resid, x = Pred, color = log(mAbun))) +
  geom_point(alpha = .5) + 
  scale_color_viridis_c() +
  # coord_cartesian(xlim = c(-1, 1)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  # scale_x_continuous(trans = "log") +
  theme_minimal()
# ALOT of zeroes

lapply(glmdiff, lme4::ranef)
lapply(glmdiff, summary)
lapply(glmdiff, anova)




# ##### Predict values #### 
# library(magrittr)
# pred_diff <- data.frame(Dt_diff %>%
#                      dplyr::group_by(Island, Species, Sites, Time) %>%
#                      dplyr::summarise(Depth = 5))
# 
# #Add a custom depth
# pred_diff$Depth <- ifelse(pred_diff$Island == "rocas", 2,
#                      ifelse(pred_diff$Island == "stpauls_rocks", 10,
#                             ifelse(pred_diff$Island == "trindade", 10,
#                                    ifelse(pred_diff$Island == "noronha", 10,0))))
# 
# pred_diff <- dplyr::arrange(pred_diff, Island, Species, Time)
# 
# # #Add an observer
# # pred$Observer <- c("gugaw_ferreira", "carlos_ferreira",
# #                    "renato_morais", "hudson_pinheiro")[
# #                      match(pred$Island, Islands)]
# # pred_diff$Sites <- c("enseada", "cagarras", "barretinha", "calheta")[
# #   match(pred_diff$Island, Islands)]
# 
# 
# head(pred_diff)
# 
# 
# #Predict species biomass plus C.I.. on each year
# pred_diff <- parallel::mclapply(Islands, function(i, glms, d) {
#   
#   #Predict output 
#   # prd <- merTools::predictInterval(merMod = glms[[i]], 
#   #                                  newdata = d[d$Island == i, ],
#   #                                  level = 0.95, n.sims = 1000, which = "fixed",
#   #                                  stat = "mean", type="linear.prediction",
#   #                                  include.resid.var = FALSE)
#   
#   prd <- predict(glms[[i]], newdata = d[d$Island == i, ], 
#                  interval = "confidence")
#   se <- predict(glms[[i]], newdata = d[d$Island == i, ], 
#                 interval = "confidence", se = TRUE)$se
#   #Combine with predicted data
#   data.frame(d[d$Island == i, ], prd, se)
#   
# }, glmdiff, pred_diff, mc.cores = 4)
# 
# pred_diff <- plyr::rbind.fill(pred_diff)
# 
# head(pred_diff)
# 
# #Transform predicted values back to biomass unit
# # pred_diff[,c("fit", "upr", "lwr")] <- 10^(pred_diff[,c("fit", "upr", "lwr")])
# 
# pred_diff <- pred_diff %>%
# reshape2::melt(id.vars = c("Island","Species","Sites", "Time"),
#                measure.vars = c("prd", "se"),
#                variable.name = "Estimates", value.name = "Values") %>%
#   reshape2::dcast(Island + Species + Sites ~ Time + Estimates, 
#                   value.var = "Values")
# 
# 
# head(pred_diff)
# 
# 
# 
# #Estimate differences unsing t values
# #Calculate t values by hand
# Diff <- plyr::ldply(Islands, function(i, m, prd) {
#   
#   #Calculate pooled standard devation
#   s <- sqrt(((prd[prd$Island == i,]$Begin_se^2) + 
#                (prd[prd$Island == i,]$End_se^2))/2)
#   
#   #Get differences between predicted values
#   dif <- (prd[prd$Island == i,]$Begin_prd - prd[prd$Island == i,]$End_prd)  
#   
#   d <- data.frame(prd[prd$Island == i, c("Island", "Species", "Sites")],
#              diff = dif,
#              se = s) 
#   d <- aggregate(d[,!colnames(d) %in% c("Island","Species","Sites")],
#                  list(d$Island, d$Species), mean)
#   
#   d$t_s <- d$dif / d$se
#   
#   data.frame(d,
#              p = ifelse(d$t_s > 0, round(pt(-d$t_s, 2*df.residual(m)), 4), 
#                         round(pt(d$t_s, 2*df.residual(m)), 4)),
#              signif = ifelse(d$t_s > 0, round(pt(-d$t_s, 2*df.residual(m)), 4), 
#                              round(pt(d$t_s, 2*df.residual(m)), 4)) < 0.05)
#   
# }, glmdiff, pred_diff)


#Test for heteroskedasticity in parameters estimate
lapply(glmdiff, lmtest::bptest)
#True for all models

# #Estimate parameters considering lack of variance homogeneity
MdlsSpp_cft <- lapply(glmdiff, function(m) {
  lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))})
MdlsSpp_cft



# Compare if the parameter before is different from the parameter after.
# Apply a t-test with known mean and SE. The t statistic is calculated as the 
# ratio between biomass and their SEs. 
# By doing so, we are comparing changes in biomass by observing differences 
# in parameter estimates.
# p values uses residual degrees of freedom 


#For each Island
DiffSpp <- parallel::mcMap(function(x, m, grp) {
  
  #Estimate Std Errors from covariance matrix
  s <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
  
  #Summarise the model
  s <- as.data.frame(s[,])
  
  #Change the name of the first parameter to in
  s[!rownames(s) %in% c("(Intercept)"), "Estimate"] <- 
    s[!rownames(s) %in% c("(Intercept)"), "Estimate"] + 
    #Add the value of the fixed intercept to all parameters
    s["(Intercept)", "Estimate"] 
  
  #Relabel parameters
  nms <- rownames(s)[grepl("SppTime", rownames(s)) |
                       grepl("(Intercept)", rownames(s))]
  nms <- stringr::str_remove(nms, "SppTime")
  nms[1] <- c("abu_sax_Begin")
  
  ss <- `rownames<-`(s[rownames(s)[grepl("SppTime", rownames(s)) |
                                     grepl("(Intercept)", rownames(s))],], nms)
  
  plyr::ldply(grp, function(y, s, m) {
    #Capture estimate values
    k <- s[grepl(y, nms),]
    
    #Calculate difference
    dif <- k[grepl("End", rownames(k)), "Estimate"] - 
      k[grepl("Begin", rownames(k)), "Estimate"]
    
    #Get SE 
    se <- sqrt(((k[grepl("End", rownames(k)), "Std. Error"])^2 +
                 (k[grepl("Begin", rownames(k)), "Std. Error"])^2)/2)
    
    #Get t value
    t_value <- dif / se
    
    #Calcualte p
    p <- ifelse(t_value > 0, round(pt(-t_value, df.residual(m)), 4), 
                round(pt(t_value, df.residual(m)), 4))
    
    return(data.frame(Island = x, Species = y, Fit = dif, SE = se, t_value, p))
    
  }, ss, m)
  
}, x = names(glmdiff), m = glmdiff, 
grp = Dt_diff %>% 
  dplyr::group_by(Island, Species) %>% 
  dplyr::summarise() %>% 
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  split(f = .$Island) %>% 
  lapply(FUN = function(x) x %>% dplyr::pull(Species)), 
mc.cores = getOption('mc.cores', 4L))

#Bind estimates into a single data.frame
DiffSpp <- plyr::rbind.fill(DiffSpp)

#### Plot after vs before for species ####
#Add groups
traits <- readxl::read_excel("Data/traits_20200512.xlsx")


DiffSpp$Group <- data.frame(groups = paste(ifelse(traits$MeanSize < 15, 
                                                "Small", "Large"), 
                                       ifelse(traits$FoodTroph < 3, "1st", 
                                              ifelse(traits$FoodTroph >= 4,"3rd",
                                                     "2nd")), sep = "_"),
                        code = traits$code)[match(DiffSpp$Species,
                                                  traits$code), 'groups']
DiffSpp <- plyr::mutate(DiffSpp, Changed = ifelse(Fit < 0 & p < .05, "Decreased",
                                ifelse(Fit > 0 & p < .05,
                                       "Increased", "Unchanged")))
DiffSpp$Spp <- traits$Species[match(DiffSpp$Species, traits$code)]
#Order species within Islands
new_order <- DiffSpp %>% 
  plyr::mutate(Group = forcats::fct_relevel(Group, "Small_1st", "Small_2nd",
                                          "Large_1st", "Large_2nd",
                                          "Large_3rd")) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha", 
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  dplyr::group_by(Island) %>% 
  dplyr::do(dplyr::tibble(
    al = levels(reorder(interaction(.$Island, .$Spp, drop=TRUE), 
                        (scales::rescale(.$Fit, c(0,1)) + 
                           as.numeric(as.factor(.$Group))*2))))) %>% 
  dplyr::pull(al)

# #Transform x axis on the plot
# invlog_trans <- function(){
#   scales::trans_new(name = 'invlog', 
#                     transform = function(x) {
#                       ifelse(x < 0, -((10 ^ abs(x)) - 1), ifelse(x == 0, 0, (10 ^ abs(x)) - 1))
#                       
#                     }, 
#                     inverse = function(x) {
#                       ifelse(x < 0, -(log10(abs(x) + 1)), ifelse(x == 0, 0, log10(abs(x)+1)))
#                     })
# }
# Plot
DiffSpp %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha",
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  dplyr::mutate(al = factor(interaction(Island, Spp), levels = new_order)) %>%
  ggplot(aes(x = exp(Fit), y = al)) + 
  #Adjust axes
  scale_x_log10(breaks = c(0, 0.1, 0.3, 1, 3, 10, 30)) +
  
  scale_y_discrete(breaks = new_order, 
                            labels = gsub("^.*\\.", "", new_order)) +
  # expand_limits(x = c(0.5,1.5)) +
  geom_vline(xintercept = exp(0)) +
  
  #Split data into grids of graphs
  facet_wrap(facets = vars(Island), 
                      ncol = 4, scales = 'free') +
  
  #Add points and lines
  geom_pointrange(aes(xmin = exp(Fit - SE),
                      xmax = exp(Fit + SE), 
                      color = Changed), 
                           show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red", "grey")) +
  
  #Make a White background
  theme(panel.grid.major.x = element_line(size=.1, 
                                                    color = rgb(0.9,0.9,0.9)),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 panel.border = element_rect(colour = "gray", 
                                                      fill=NA, size=.5)) +
  
  #Labels
  labs(y = "Species", 
                x = "Relative changes in species standing biomass")

# ggpubr::ggarrange(Graph3, Graph4, nrow = 2, heights = c(2,6))
# Graph3 + Graph4 + patchwork::plot_layout(heights = c(1, 6), guides = 'collect')
