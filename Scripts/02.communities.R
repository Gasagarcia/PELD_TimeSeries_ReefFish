#### Exploratory analysis Composition changes through time ####
# Data <-readRDS("R_Objects/Data.rds")

# # How is composition related to depth? Use an NMDS on transects to see
# # Take a loooooong time to run
# MDS <- lapply(Data, function(x) {
#   vegan::scores( vegan::metaMDS(x[['mass']], parallel = getOption("mc.cores", 8)) )
# })
# 
# 
# #Bind results togeter
# MDS <- plyr::rbind.fill( Map( function(x, y, z) 
#   data.frame(x, y[['depth']], y[['sites']], y[['year']],
#              rep(z, length( y[['depth']]))), MDS, Data, names(Data)))
# #Add names
# colnames(MDS) <- c('NMDS1', 'NMDS2', 'Depth', 'Sites', 'Year', 'Island')
# 
# #Plot
# ggplot2::ggplot(MDS, ggplot2::aes(x = NMDS1, y = NMDS2)) +
#   ggplot2::geom_point(ggplot2::aes(color = -Depth)) +
#   # stat_ellipse(aes(group = Sites, color = Sites)) +
#   ggplot2::coord_cartesian(xlim = c(-1,1), ylim = c(-1,1)) +
#   ggplot2::scale_color_gradientn(colors = c('red', 'red', 'orange','yellow','green','blue')) +
#   ggplot2::theme_minimal() +
#   ggplot2::facet_wrap(facets = ggplot2::vars(Island))
# 
#### Year effect on the data (CAP) ####
# Data <-readRDS("R_Objects/Data.rds")
# 
# # #Density of uvcs by depth
# # par(mfrow = c(2,2))
# # Map(function(x, y) hist(x$depth, main = y), Data, names(Data))
# 
# 
# 
# # Observe trends in composition using the mean biomass
# # 
# Data_sts <- Map(function(i, x) {
#   force(x)
#   # Add env. conditions
#   if (i == 'rocas') {
#     #Wave exposure for Rocas following Longo et al. 2015
#     x[["pred"]][['c_depth']] <- ifelse(x[["pred"]][['sites']] %in% c("barretinha", "falsa_barreta",
#                                                  "podes_crer", "salao",
#                                                  "farol_dois"), 'Open', 'Closed')
#   }  else {
#     #Other islands are split by depth
#     x[["pred"]][['c_depth']] <- ifelse(x[["pred"]][['depth']] <= 5, 'Shallow',
#                              ifelse(x[["pred"]][['depth']] > 5 & x[["pred"]][['depth']] <= 10,
#                                     'Mid', 'Deep'))
#   }
# 
# 
#   l <- list(paste(x[["pred"]][['sites']], x[["pred"]][['year']], x[["pred"]][['c_depth']], sep = "_"))
# 
#   x[['abun']] <- aggregate(x[['abun']], l, mean)[ , -1]
#   x[['mass']] <- aggregate(x[['mass']], l, mean)[ , -1]
#   x[['size']] <- aggregate(x[['size']], l, mean)[ , -1]
#   x[["pred"]] <- aggregate(x[["pred"]], l,
#                            FUN = function(y) {
#                              if (is.numeric(y)) {
#                                mean(y, na.rm = TRUE)
#                                } else { 
#                                paste(unique(y), collapse = '_&_')
#                                  }})[ , -1]
#   
#   x
# }, names(Data), Data)
# 
# 
# saveRDS(Data_sts, 'R_Objects/Data_sts.rds')
# 
# #### PERMANOVA #####
# # Apply a PERMANOVA per Island to test predictors significance
# 
# Perm <- lapply(Data_sts, function(i) {
#   vegan::adonis(i[["mass"]] ~ i[["year"]] + i[["sites"]]:i[["c_depth"]] + i[["year"]]:i[['sites']] + ,
#                 permutations = 1000)
# })
# 
# 
# Perm

##### Load Data #####
#Load data
Data <-readRDS("R_Objects/Data.rds")
Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")

#Add categorical Depth
Dt <- lapply(Islands, function(i, Data) {
  x <- Data[[i]]
  if (i == 'rocas') {
    #Wave exposure for Rocas following Longo et al. 2015
    x[["pred"]][['c_depth']] <- ifelse(x[["pred"]][['sites']] %in%
                               c("barretinha", "falsa_barreta",
                                 "podes_crer", "salao",
                                 "farol_dois"), 'Open', 'Closed')
  }  else {
    #Other islands are split by depth
    x[["pred"]][['c_depth']] <- ifelse(x[["pred"]][['depth']] <= 5, 'Shallow',
                             ifelse(x[["pred"]][['depth']] > 5 & 
                                      x[["pred"]][['depth']] <= 10,
                                    'Mid', 'Deep'))
  }
  
  #Add markers to preserve colnames
  colnames(x[["abun"]]) <- paste0("abun_", colnames(x[["abun"]]))
  colnames(x[["mass"]]) <- paste0("mass_", colnames(x[["mass"]]))
  colnames(x[["size"]]) <- paste0("size", colnames(x[["size"]]))
  
  #Merge all data into a single data.frame
  d <- data.frame(x[["mass"]], x[["abun"]], x[["size"]],  x[["pred"]])
  
  # Now order by year
  d2 <- dplyr::arrange(d, year)
  
  #And split into separate files again
  mass <- d2[,grepl("mass", colnames(d2), fixed = TRUE)]
  abun <- d2[,grepl("abun", colnames(d2), fixed = TRUE)]
  size <- d2[,grepl("size", colnames(d2), fixed = TRUE)]
  
  #Remove codes
  colnames(mass) <- stringr::str_remove(colnames(mass), "mass_")
  colnames(abun) <- stringr::str_remove(colnames(mass), "abun_")
  colnames(size) <- stringr::str_remove(colnames(mass), "size_")
  
  
  pred <- d2[,! (grepl("mass", colnames(d2), fixed = TRUE) |
                   grepl("abun", colnames(d2), fixed = TRUE) |
                   grepl("size", colnames(d2), fixed = TRUE))]
  
  list("mass" = mass, "abun" = abun, 
       "size" = size, "pred" = pred)
}, Data); names(Dt) <- Islands


#### Species Correlation plots ####
par(mfrow = c(2,4))
Map(function(x, i) {
  #Get species full name
  # colnames(x$mass) <- traits$Species[match(colnames(x$mass), traits$code)]
  #Perform spearman correlations
  mat <- cor(method = "spearman", 
             x = vegan::decostand(x$mass, method = "hellinger"))
  #Take diags out
  mat <- ifelse(mat == 1, 0, mat)
  #Make a correlation plot
  corrplot::corrplot(corr = mat,# title = i,
                     type = 'upper', diag = FALSE, method = "color",
                     order = "FPC", sig.level = 0.05, addgrid.col	= NA,
                     col = colorRampPalette(c("darkred", "red", "white", 
                                              "blue", "darkblue"))(100),
                     mar = c(0,0,0,0), tl.pos = 'td', tl.col = "black",
                     cl.lim = range(mat))
}, Dt, names(Dt))

### RDA ####
#Compute redundancy analysis
system.time(
  RDA <- parallel::mclapply(Dt, function(i) {
    i$pred$year <- as.character(i$pred$year)
    #Model
    mod <- vegan::capscale(vegan::wisconsin(i[["mass"]]) ~ year + 
                             Condition(depth * sites),
                           sqrt.dist = TRUE, 
                           distance = "bray", 
                           data = i$pred)
  }, mc.cores = getOption('mc.cores', 4L))
)

RDA
par(mfrow = c(2,4))
Map(plot, RDA, main = names(RDA), display = "cn")

#### Variance retained by each axis ####
#Observe retained variance in each axis
par(mfrow = c(2,2))
Map(function(x, i) {
  constrained_eig <- x$CCA$eig/x$tot.chi*100
  unconstrained_eig <- x$CA$eig/x$tot.chi*100
  expl_var <- c(constrained_eig[1:5], unconstrained_eig[1:5])
  barplot (expl_var, col = c(rep('red', 5), rep("black", 5)),
           las = 2, ylab = '% variation', main = i)
}, RDA, names(RDA))

#### Years significance ####
# Test whether years significantly explains data
# Takes anout 1min
RDA_anova <- lapply(RDA, anova, by = "term", model = "reduced",
                    permutations = permute::how(
                      nperm = 1000, 
                      within = permute::Within(type="series")),
                    parallel = getOption('mc.cores', 8L))



#### Boostrap samples with replacement ####
#Census small area may mask correlation on species occurrences.
#Use bootstrap to get combinations of censuses that represent 
#not a single censuses, but a mix of 5 censuses
par(mfrow = c(2,2))
lapply(Dt, function(x) hist(x$pred$depth))
#Create fixed depth categories
Dt <- Map(function(x, i) {
  if (i == "rocas") {
    boot <- ifelse(x$pred$c_depth == "Closed", 1,2)
    # hist(boot, main = i)
  }
  if (i == "noronha") {
    boot <- ifelse(x$pred$depth <= 7, 1,
                   ifelse(x$pred$depth > 7 & x$pred$depth <= 11, 2,
                          ifelse(x$pred$depth > 11 & x$pred$depth <= 17, 3, 4)))
    # hist(boot, main = i)
  }
  if (i == "trindade") {
    boot <- ifelse(x$pred$depth <= 7, 1,
                   ifelse(x$pred$depth > 7 & x$pred$depth <= 10, 2,
                          ifelse(x$pred$depth > 10 & x$pred$depth <= 15, 3,
                                 ifelse(x$pred$depth > 15 & x$pred$depth <= 20, 4, 5))))
    # hist(boot, main = i)

  }
  if (i == "stpauls_rocks") {
    boot <- ifelse(x$pred$depth <= 7, 1,
                   ifelse(x$pred$depth > 7 & x$pred$depth <= 15, 2,
                                 ifelse(x$pred$depth > 15 & x$pred$depth <= 25, 3, 4)))
    # hist(boot, main = i)
  }

  x$pred$boot <- boot
  x
}, Dt, names(Dt))





#### Bootstrap benchmarking ####

Boot <- function(x, nrep = NA, nboot = 5) {
  if (is.na(nrep)) {
    nrep <- quote(nrow(y))
  }
  #Combine mass, size, abun and predictors in a single data.frame
  colnames(x[["abun"]]) <- paste0("abun_", colnames(x[["abun"]]))
  colnames(x[["mass"]]) <- paste0("mass_", colnames(x[["mass"]]))
  colnames(x[["size"]]) <- paste0("size", colnames(x[["size"]]))
  d <- data.frame(x[["mass"]], x[["abun"]], x[["size"]],  x[["pred"]])
  
  d2 <- plyr::rbind.fill(
    #Now split full data by depth, sites and year
    by(d, list(d$boot, d$sites, d$year), function(y){
      
      #Within each category, sample 5 transects 10 times and average them
      plyr::ldply(1:eval(nrep), function(i, y) {
        r <- y[sample(1:nrow(y), nboot, replace = TRUE), ]
        
        as.data.frame(lapply(r, function(z) {
          if (is.numeric(z)) {
            mean(z, na.rm = TRUE)
          } else {
            paste(sort(unique(z)), collapse = '_&_')
          }}))
        
        
      }, y)
    })
  )
  
  d2 <- dplyr::arrange(d2, year)
  
  #Filter using colnames
  mass <- d2[,grepl("mass", colnames(d2), fixed = TRUE)]
  abun <- d2[,grepl("abun", colnames(d2), fixed = TRUE)]
  size <- d2[,grepl("size", colnames(d2), fixed = TRUE)]
  
  #Remove codes
  colnames(mass) <- stringr::str_remove(colnames(mass), "mass_")
  colnames(abun) <- stringr::str_remove(colnames(mass), "abun_")
  colnames(size) <- stringr::str_remove(colnames(mass), "size_")
  
  
  pred <- d2[,! (grepl("mass", colnames(d2), fixed = TRUE) |
                   grepl("abun", colnames(d2), fixed = TRUE) |
                   grepl("size", colnames(d2), fixed = TRUE))]
  
  list("mass" = mass, "abun" = abun,
       "size" = size, "pred" = pred)
}

Boot_RDA <- function(i) {
  # i$pred$boot <- as.factor(i$pred$boot)
  i$pred$year <- as.factor(i$pred$year)
  #Model
  mod <- vegan::capscale(vegan::wisconsin(i[["mass"]]) ~ 
                           year + Condition(depth * sites),
                         sqrt.dist = TRUE, 
                         distance = "bray", 
                         data = i$pred)
  mod
}

### Test different combinations of censuses to average and resampling size
# Use Noronha as baseline for testing
pars <- expand.grid(n = 1:10,
                    nrep = c(3, 5, 10, 20, NA), # NA gives the real sampling
                    nboot = c(3, 5, 10))

library(doMC); library(magrittr)
registerDoMC(8) #Consumes ~3 GB of RAM memory


# Generate bootstrapped data
system.time(
  bk <- plyr::mlply(pars[,-1], function(nrep, nboot) {
    Dt$noronha %>% 
      Boot(nrep = nrep, nboot = nboot)
  }, .parallel = TRUE)) # Takes about 3 min


# Extract correlation matrices from bootstrapped data 

bk_cor <- bk %>%
  plyr::llply(function(x) vegan::wisconsin(x$mass), .parallel = TRUE) %>%
  plyr::llply(cor, method = "spearman", .parallel = TRUE) %>%
  plyr::ldply(function(x) {
    cor(as.numeric(x), as.numeric(cor(vegan::wisconsin(Dt$noronha$mass))))},
    .parallel = TRUE)

#Plot correlation as a function of nrep and nboot parameters
library(ggplot2)
bk_cor %>%
  ggplot(aes(x = as.factor(nrep), y = V1, color = as.factor(nboot))) +
  geom_boxplot() + theme_bw() + labs(x = "Number of resampling trials",
                                     y = "Correlation with original species associations") +
  scale_color_viridis_d("Number of averaged samples")


# Perform an RDA for each data
bk_rda <- bk %>% 
  plyr::llply(Boot_RDA)







lapply(Dt[1:2], Boot_RDA) %>% lapply((function(x) x$tot.chi))

Dt_boot <- parallel::mclapply(Dt, Boot, mc.cores = getOption('mc.cores', 4L))










#Repeat bootstrap procedure 10 times on each combination of site, year and depth
#Takes 20 seconds to run
set.seed(1)

system.time(
  Dt_boot <- parallel::mclapply(Dt, function(x) {
    
    #Combine mass, size, abun and predictors in a single data.frame
    colnames(x[["abun"]]) <- paste0("abun_", colnames(x[["abun"]]))
    colnames(x[["mass"]]) <- paste0("mass_", colnames(x[["mass"]]))
    colnames(x[["size"]]) <- paste0("size", colnames(x[["size"]]))
    d <- data.frame(x[["mass"]], x[["abun"]], x[["size"]],  x[["pred"]])
    
    d2 <- plyr::rbind.fill(
      #Now split full data by depth, sites and year
      by(d, list(d$boot, d$sites, d$year), function(y){
        
        #Within each category, sample 5 transects 10 times and average them
        plyr::ldply(1:15, function(i, y) {
          r <- y[sample(1:nrow(y), 5, replace = TRUE), ]
          
          as.data.frame(lapply(r, function(z) {
            if (is.numeric(z)) {
              mean(z, na.rm = TRUE)
            } else {
              paste(sort(unique(z)), collapse = '_&_')
            }}))
          
          
        }, y)
      })
    )
    
    d2 <- dplyr::arrange(d2, year)
    
    #Filter using colnames
    mass <- d2[,grepl("mass", colnames(d2), fixed = TRUE)]
    abun <- d2[,grepl("abun", colnames(d2), fixed = TRUE)]
    size <- d2[,grepl("size", colnames(d2), fixed = TRUE)]
    
    #Remove codes
    colnames(mass) <- stringr::str_remove(colnames(mass), "mass_")
    colnames(abun) <- stringr::str_remove(colnames(mass), "abun_")
    colnames(size) <- stringr::str_remove(colnames(mass), "size_")
    
    
    pred <- d2[,! (grepl("mass", colnames(d2), fixed = TRUE) |
                     grepl("abun", colnames(d2), fixed = TRUE) |
                     grepl("size", colnames(d2), fixed = TRUE))]
    
    list("mass" = mass, "abun" = abun,
         "size" = size, "pred" = pred)
  }, mc.cores = getOption('mc.cores', 4L))
)

saveRDS(Dt_boot, "R_Objects/Dt_boot.rds")
Dt_boot <- readRDS("R_Objects/Dt_boot.rds")

#### Bootstrapped Corplot ####
par(mfrow = c(2,2))
Map(function(x, i) {
  #Get species full name
  
  # colnames(x$mass) <- traits$Species[match(colnames(x$mass), traits$code)]
  #Perform spearman correlations
  mat <- cor(method = "spearman", 
             x = vegan::wisconsin(x$mass))
  #Take diags out
  mat <- ifelse(mat == 1, 0, mat)
  #Make a correlation plot
  corrplot::corrplot(corr = mat,# title = i,
                     type = 'upper', diag = FALSE, method = "color",
                     order = "FPC", sig.level = 0.05, addgrid.col	= NA,
                     col = colorRampPalette(c("darkred", "red", "white", 
                                              "blue", "darkblue"))(100),
                     mar = c(0,0,0,0), tl.pos = 'td', tl.col = "black",
                     cl.lim = range(mat))
}, Dt_boot, names(Dt_boot))

#### Boot RDA ####
#Compute redundancy analysis
system.time( # About 2 min
  RDA_boot <- parallel::mclapply(Dt_boot, function(i) {
    # i$pred$boot <- as.factor(i$pred$boot)
    i$pred$year <- as.factor(i$pred$year)
    #Model
    mod <- vegan::capscale(vegan::wisconsin(i[["mass"]]) ~ year + 
                             Condition(depth * sites),
                           sqrt.dist = TRUE, 
                           distance = "bray", 
                           data = i$pred)
    mod
  }, mc.cores = getOption('mc.cores', 4L))
)

RDA_boot
par(mfrow = c(2,2))
lapply(RDA_boot, plot, display = "cn")

#Observe retained variance in each axis
par(mfrow = c(2,2))
Map(function(x, i) {
  constrained_eig <- x$CCA$eig/x$tot.chi*100
  unconstrained_eig <- x$CA$eig/x$tot.chi*100
  expl_var <- c(constrained_eig[1:5], unconstrained_eig[1:5])
  barplot (expl_var, col = c(rep('red', 5), rep("black", 5)),
           las = 2, ylab = '% variation', main = i)
}, RDA_boot, names(RDA_boot))

# Test whether years significantly explains data
# Takes about 3 min with 4 cores
system.time(
  RDA_boot_anova <- lapply(RDA_boot, anova, by = "term", model = "reduced",
                           permutations = permute::how(
                             nperm = 1000, 
                             within = permute::Within(type="series")),
                           parallel = getOption('mc.cores', 4L))
)


system.time(
  
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  ###____________IMPORTANT______________###
  #$#$#$ Took 3h 8min 55s to compute $#$#$#
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
  RDA_axis <- lapply(RDA_boot[rev(Islands)], anova, by = "axis", model = "reduced",
                    permutations = permute::how(minperm = 10, nperm = 1000,
                      within = permute::Within(type = "series",mirror = FALSE)),
                    parallel = getOption('mc.cores', 8L))
)

RDA_axis
#First 5 components are significant in all islands
saveRDS(RDA_axis, "R_Objects/RDA_axis.rds")
RDA_axis <- readRDS("R_Objects/RDA_axis.rds")
# First 5 axis in all islands plus axes 6 in trindade and rocas


### How similar are species correlation matrices derived from each dataset?
Map(function(x, y, i) {
  matx <- cor(method = "pearson", 
              x = vegan::decostand(x$mass, method = "hellinger"))
  maty <- cor(method = "pearson", 
              x = vegan::decostand(y$mass, method = "hellinger"))
  m <- lm(as.numeric(matx) ~ as.numeric(maty))
  print(summary(m))
  
  isl <- if (i %in% 'noronha') {
    "Fernando de Noronha"
  } else  if (i %in% 'rocas') {
    "Rocas Atoll"
  } else  if (i %in% 'stpauls_rocks') {
    "St. Paul's Rocks"
  } else "Trindade Island"
               
  return( data.frame(Dt = as.numeric(matx), Dt_Boot = as.numeric(maty)) %>%
            ggplot(aes(x = Dt, y = Dt_Boot)) +
            geom_abline(slope = 1, intercept = 0, color = "grey") +
            geom_point() + theme_bw() +
            geom_smooth(method = "lm", se = FALSE, linewidth = .5) + 
            annotate("text", 
                     label = paste0("Correlation = ", 
                                    round(m$coefficients[2], 3)), 
                     x = 0, y = .9, size = 5, 
                     family = "Times New Roman") +
            coord_cartesian(xlim = c(-.5, 1), ylim = c(-.5, 1)) +
            labs(title = isl, 
                 x = "Raw correlation", 
                 y = "Bootstrapped correlation"))
}, x = Dt, y = Dt_boot, i = names(Dt_boot)) %>% ggpubr::ggarrange(plotlist = .)

# data.frame(Dt = as.numeric(matx), Dt_Boot = as.numeric(maty)) %>%
#   ggplot(aes(y = (Dt_Boot - Dt), x = Dt)) +
#   geom_abline(slope = 0, intercept = 0, color = "grey") +
#   geom_point() + theme_bw() +
#   geom_smooth(method = "lm", se = FALSE, linewidth = .5) + 
#   scale_y_continuous(trans = 'pseudo_log') +
#   # annotate("text", 
#   #          label = paste0("Correlation = ", 
#   #                         round(m$coefficients[2], 3)), 
#   #          x = 0, y = .9, size = 5, 
#   #          family = "Times New Roman") +
#   # coord_cartesian(xlim = c(-.25, .25), ylim = c(-.25, .25)) +
#   labs(title = isl, 
#        y = "Bootstrapped correlation - Raw", 
#        x = "Raw correlation")
















#### Assess constrained deviation ####
## Test whether constrained deviation is linearly related to time ##
plt_dt <-  plyr::ldply(RDA_boot, function(x) {
  eig <- length(x$CCA$eig)
  d <- data.frame(Code = rownames(vegan::scores(x, choices = 1, display = "cn")),
             `colnames<-`(vegan::scores(x, choices = c(1:6),
                                        display = "cn"),
                          paste0("dbRDA", 1:6)))
  d$Year <- as.numeric(stringr::str_remove(d$Code, "year"))
  d
}, .id = "Island")

library(magrittr); library(ggplot2)
plt_dt %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2, color = Year)) +
  geom_point() + ggrepel::geom_text_repel(aes(label = Year)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  scale_color_viridis_c() +
  facet_wrap(vars(Island), nrow = 2, scales = 'free') +
  theme_bw() + theme(panel.grid = element_blank())

 
#Plot components as a function of years
#Filter for significant trends
## First Melt data
#Now select only trends with p values for Year < 0.05
signif <- function(d, names, variable, form = as.formula(Scores ~ Year)) {
  plyr::ldply(
    split(d, plyr::llply(names, function(i, d) {d[,i]}, d)), 
    function(x) {
      p <- anova(lm(form, data = x))[variable,"Pr(>F)"]
      
      ifelse(p < 0.05, {print(summary(lm(form, data = x))); return(x)}, return(NULL))
    })}

signif(
  plt_dt %>%  
    reshape2::melt(id.vars = c("Island", "Year"), 
                   measure.vars = paste0("dbRDA", 1:5),
                   variable.name = "Axes", value.name = "Scores") %>%
    plyr::mutate(Island = plyr::revalue(
      Island, c('noronha' = "Fernando de Noronha", 
                'rocas' = "Rocas Atoll", 
                'stpauls_rocks' = "St. Paul's Rocks",
                'trindade' = "Trindade Island"))),
  names = c("Island", "Axes"), variable = "Year",
  form = as.formula(Scores ~ Year))



#Now plot RDA axis as a function of years
plt_dt %>%
  dplyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  reshape2::melt(id.vars = c("Island", "Year"), 
                 measure.vars = paste0("dbRDA", 1:4),
                 variable.name = "Axes", value.name = "Scores") %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(Year = (Year - 2000)) %>%
  # dplyr::filter(scores > 0.2 | scores < -.2) %>%
  ggplot(aes(x = Year, y = Scores, color = Year)) +
  # geom_line(stat = "smooth", alpha = 0.2, method = "loess", se = FALSE) +
  geom_point(alpha = .7) + scale_color_viridis_c() +
  geom_smooth(aes(x = (Year), y = Scores),
              method = "lm", col = "darkgrey", fill = "grey", alpha = .2,
              data = plt_dt %>%
                reshape2::melt(id.vars = c("Island", "Year"),
                               measure.vars = paste0("dbRDA", 1),
                               variable.name = "Axes", 
                               value.name = "Scores") %>%
                plyr::mutate(Island = plyr::revalue(
                  Island, c('noronha' = "Fernando de Noronha",
                            'rocas' = "Rocas Atoll",
                            'stpauls_rocks' = "St. Paul's Rocks",
                            'trindade' = "Trindade Island"))) %>%
                signif(variable = "Year", names = c("Island", "Axes"),
                       form = as.formula(Scores ~ Year)) %>%
                dplyr::mutate(Year = (Year - 2000))) +
  facet_grid(rows = vars(Axes), cols = vars(Island), scales = "free") +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  scale_x_continuous(breaks = c(2*1:10)) +
  theme_bw() + theme(panel.grid = element_blank())
  

#### Interpret species placement within components ####
library(magrittr); library(ggplot2)
#Save species scores and other traits into a single data.frame
plt_spp_dt <-  plyr::ldply(RDA_boot, function(x) {
  eig <- length(x$CCA$eig)
  data.frame(Code = rownames(vegan::scores(x, choices = 1, 
                                             display = "species")),
             `colnames<-`(vegan::scores(x, choices = 1:6, 
                             display = "species"),
                          paste0("dbRDA", 1:6)))
    
             # vegan::scores(x, choices = eig + 1:3, display = "species"))
}, .id = "Island")

head(plt_spp_dt)

#Load traits
traits <- readODS::read_ods("Data/traits_210613.ods")
colnames(traits)

plt_spp_dt <- plt_spp_dt %>%
  dplyr::left_join(traits %>%
                     dplyr::select(c("Species", "code", "FoodTroph", "AspectRatio", 
                                     "shape", "FoodItems", 
                                     "Bottom_Attachment", "maxSize", "Size")),
                   by = c("Code" = "code"))
library(dplyr)
#Get mean biomass and abundance
plt_spp_dt <- plt_spp_dt %>%
  left_join(
  K <- plyr::ldply(Dt_boot, function(x) {
    k <- data.frame(x$mass, select(x$pred, sites, c_depth, year)) %>% 
      # Average all censuses within a same depth
      group_by(c_depth, sites, year) %>%
      summarise_all(mean) %>% ungroup %>%
      select(-c_depth) %>% 
      # Average within sites
      group_by(sites, year) %>%
      summarise_all(mean) %>% ungroup() %>%
      select(-sites) %>% 
      # Average within within years
      group_by(year) %>%
      summarise_all(mean) %>%
      ungroup() %>% select(-year)
    # Get temporal mean and SD in normal and wisconsin transformed values
    data.frame(Mass = apply(k, 2, mean), 
                 Sd = apply(k, 2, sd),
                 trMass = colMeans(vegan::wisconsin(k)),
                 trSd = apply(vegan::wisconsin(k), 2, sd),
                 code = colnames(k)
                 
      
      )
}, .id = "Island"), by = c(Island = "Island", Code = "code"))

# data.frame(Mass = colMeans(x$mass), 
#            Sd = apply(x$mass, 2, sd),
#            Abun = colMeans(x$abun),
#            trMass = colMeans(vegan::wisconsin(x$mass)), 
#            trSd = apply(vegan::wisconsin(x$mass), 2, sd),
#            trAbun = colMeans(vegan::wisconsin(x$abun)),
#            code = colnames(x$mass)


#### Test if traits are related to components ####
Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")

env_spp <- lapply(Islands, function(i, x, d) {
  vegan::envfit(x[[i]], display = "species", choices = 1:5,
                d[d$Island == i ,] %>%
                  dplyr::select( c("Mass", "Size")) %>%
                  dplyr::mutate(Size = log(Size)) %>%
                  dplyr::mutate(Mass = log(Mass))) 
}, RDA_boot, plt_spp_dt); names(env_spp) <- Islands

env_spp

colnames(plt_spp_dt)
env_adonis <- lapply(Islands, function(i, d) {
  print(i)
  donis <- vegan::adonis2(d[d$Island == i, paste0("dbRDA", 1:5)] ~ 
                            log(Size) + log(Mass) + FoodTroph,
                          data = d[d$Island == i, ], permutations = 1000,
                          method = 'euclidean')
  donis
  # vegan::envfit(x[[i]], display = "species", choices = 1:5,
  #               d[d$Island == i , c("Troph", "Size", "Fin_AR",
  #                                   "FoodItems", "Bottom",
  #                                   "Mass")],)
}, plt_spp_dt)


env_adonis
# lapply(env_adonis, AIC)
colnames(plt_spp_dt)





#Plot axis scores and color points by variables
library(ggplot2); library(magrittr)

# #Colour by Size
# plt_Spp <- plt_spp_dt %>%
#   dplyr::filter((dbRDA1 > 0.5 | dbRDA1 < -0.5) |
#                   (dbRDA2 > 0.5 | dbRDA2 < -0.5) |
#                   (dbRDA4 > 0.5 | dbRDA4 < -0.5)) %>%
#   reshape2::melt(id.vars = c("Island", "Species", "Code", "Size", 
#                              "FoodTroph", "Mass", "Abun"), 
#                  measure.vars = paste0("dbRDA", 1:6),
#                  variable.name = "Axes", value.name = "Scores") %>%
#   tidyr::unite("ID", c("Island", "Axes"), sep = ".") %>%
#   # dplyr::filter(ID %in% (plt_dt %>%  
#   #                          reshape2::melt(
#   #                            id.vars = c("Island", "Year"), 
#   #                            measure.vars = paste0("dbRDA", 1:5),
#   #                            variable.name = "Axes", value.name = "Scores") %>%
#   #                          signif(variable = "Year",
#   #                                 names = c("Island", "Axes"),
#   #                                 form = as.formula(Scores ~ Year)) %>%
#   #                          dplyr::pull(.id))) %>%
#   tidyr::separate("ID", c("Island", "Axes"), sep = "[.]") %>%
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
# 
#   ggplot(aes(x = Axes, y = Scores, color = log(Size))) +
#   geom_point(alpha = .7) + scale_color_viridis_c() +
#   ggrepel::geom_text_repel(aes(label = Species), force = 2, max.overlaps = 5) +
#   facet_wrap(vars(Island), scales = "free", nrow = 1) +
#   theme_bw(); plt_Spp
# 
# 
# 




# Biplots with species showing average Size
plt_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::mutate(Code = stringr::str_remove(Code, pattern = "year")) %>%
  dplyr::mutate(Year = as.numeric(Code)) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  scale_color_viridis_c(breaks = c(2006, 2013, 2019)) +
  geom_point(aes( color = Year), shape = 3) + 
  ggrepel::geom_label_repel(aes(label = Year,  color = Year), 
                            size = 3) +
  coord_cartesian(xlim = c(-2.5,3), ylim = c(-2,2)) +
  facet_wrap(vars(Island), nrow = 4, scales = 'free') +
  theme_bw() + theme(panel.grid = element_blank()) +
  
plt_spp_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::filter(dbRDA1 > 0.5 | dbRDA1 < -0.5 |
                  dbRDA2 > 0.5 | dbRDA2 < -0.5) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  geom_segment(
    data = plyr::ldply(env_spp, 
                       function(x) {x$vectors$arrows  %>%
                           as.data.frame() %>%
                           tibble::rownames_to_column("Var")}, 
                       .id = "Island") %>%
      plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
      plyr::mutate(Island = plyr::revalue(
        Island, c('noronha' = "Fernando de Noronha", 
                  'rocas' = "Rocas Atoll", 
                  'stpauls_rocks' = "St. Paul's Rocks",
                  'trindade' = "Trindade Island"))), 
    aes(x = 0, y = 0, xend = CAP1, yend = CAP2), color = 'darkgrey',
    arrow = arrow(length = unit(0.05, "npc"), type = "closed")) +
  
  ggrepel::geom_text_repel(
    data = plyr::ldply(env_spp, 
                       function(x) {x$vectors$arrows  %>%
                           as.data.frame() %>%
                           tibble::rownames_to_column("Var")}, 
                       .id = "Island") %>%
      plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
      plyr::mutate(Island = plyr::revalue(
        Island, c('noronha' = "Fernando de Noronha", 
                  'rocas' = "Rocas Atoll", 
                  'stpauls_rocks' = "St. Paul's Rocks",
                  'trindade' = "Trindade Island"))), 
    aes(x = CAP1, y = CAP2, label = Var), color = 'darkgrey') +
  ggrepel::geom_text_repel(aes(label = stringr::str_to_title(Code),
                               color = Size), size = 3) +
  geom_point(aes(color = Size)) +
  scale_color_viridis_c(option = "plasma", trans = "log10")+#, breaks = c(5, 12, 25)) +
  facet_wrap(vars(Island), nrow = 4, scales = 'free') +
  coord_cartesian(xlim = c(-2.5,3), ylim = c(-2,2)) +
  theme_bw() + theme(panel.grid = element_blank()) +

patchwork::plot_layout(ncol = 2, guides = "collect")








# Biplots with species showing average mass
plt_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::mutate(Code = stringr::str_remove(Code, pattern = "year")) %>%
  dplyr::mutate(Year = as.numeric(Code)) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  scale_color_viridis_c(breaks = c(2006, 2013, 2019)) +
  geom_point(aes( color = Year), shape = 3) + 
  ggrepel::geom_label_repel(aes(label = Year,  color = Year), 
                            size = 3) +
  geom_point(
    data = plyr::ldply(env_spp, 
                       function(x) {x$vectors$arrows  %>%
                           as.data.frame() %>%
                           tibble::rownames_to_column("Var")}, 
                       .id = "Island") %>%
      plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
      plyr::mutate(Island = plyr::revalue(
        Island, c('noronha' = "Fernando de Noronha", 
                  'rocas' = "Rocas Atoll", 
                  'stpauls_rocks' = "St. Paul's Rocks",
                  'trindade' = "Trindade Island"))), 
    aes(x = CAP1, y = CAP2), color = 'darkgrey') +
  ggrepel::geom_text_repel(
    data = plyr::ldply(env_spp, 
                       function(x) {x$vectors$arrows  %>%
                           as.data.frame() %>%
                           tibble::rownames_to_column("Var")}, 
                       .id = "Island") %>%
      plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
      plyr::mutate(Island = plyr::revalue(
        Island, c('noronha' = "Fernando de Noronha", 
                  'rocas' = "Rocas Atoll", 
                  'stpauls_rocks' = "St. Paul's Rocks",
                  'trindade' = "Trindade Island"))), 
    aes(x = CAP1, y = CAP2, label = Var), color = 'darkgrey') +
  coord_cartesian(xlim = c(-2.5,2.5), ylim = c(-2,2)) +
  facet_wrap(vars(Island), nrow = 4, scales = 'free') +
  theme_bw() + theme(panel.grid = element_blank()) +
  
  plt_spp_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::filter(dbRDA1 > 0.5 | dbRDA1 < -0.5 |
                  dbRDA2 > 0.5 | dbRDA2 < -0.5) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  ggrepel::geom_text_repel(aes(label = stringr::str_to_title(Code),
                               color = trMass), size = 3) +
  geom_point(aes(color = trMass)) +
  scale_color_viridis_c(option = "plasma", trans = "log10") +
  facet_wrap(vars(Island), nrow = 4, scales = 'free') +
  coord_cartesian(xlim = c(-2.5,2.5), ylim = c(-2,2)) +
  theme_bw() + theme(panel.grid = element_blank()) +
  
  patchwork::plot_layout(ncol = 2, guides = "collect")








# Biplots with species showing standard deviation in mass
plt_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::mutate(Code = stringr::str_remove(Code, pattern = "year")) %>%
  dplyr::mutate(Year = as.numeric(Code)) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  scale_color_viridis_c(breaks = c(2006, 2013, 2019)) +
  geom_point(aes( color = Year), shape = 3) + 
  ggrepel::geom_label_repel(aes(label = Year,  color = Year), 
                            size = 3) +
  geom_point(
    data = plyr::ldply(env_spp, 
                       function(x) {x$vectors$arrows  %>%
                           as.data.frame() %>%
                           tibble::rownames_to_column("Var")}, 
                       .id = "Island") %>%
      plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
      plyr::mutate(Island = plyr::revalue(
        Island, c('noronha' = "Fernando de Noronha", 
                  'rocas' = "Rocas Atoll", 
                  'stpauls_rocks' = "St. Paul's Rocks",
                  'trindade' = "Trindade Island"))), 
    aes(x = CAP1, y = CAP2), color = 'darkgrey') +
  ggrepel::geom_text_repel(
    data = plyr::ldply(env_spp, 
                       function(x) {x$vectors$arrows  %>%
                           as.data.frame() %>%
                           tibble::rownames_to_column("Var")}, 
                       .id = "Island") %>%
      plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
      plyr::mutate(Island = plyr::revalue(
        Island, c('noronha' = "Fernando de Noronha", 
                  'rocas' = "Rocas Atoll", 
                  'stpauls_rocks' = "St. Paul's Rocks",
                  'trindade' = "Trindade Island"))), 
    aes(x = CAP1, y = CAP2, label = Var), color = 'darkgrey') +
  coord_cartesian(xlim = c(-2.5,2.5), ylim = c(-2,2)) +
  facet_wrap(vars(Island), nrow = 4, scales = 'free') +
  theme_bw() + theme(panel.grid = element_blank()) +
  
  plt_spp_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::filter(dbRDA1 > 0.5 | dbRDA1 < -0.5 |
                  dbRDA2 > 0.5 | dbRDA2 < -0.5) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  ggrepel::geom_text_repel(aes(label = stringr::str_to_title(Code),
                               color = Abun), size = 3) +
  geom_point(aes(color = Abun)) +
  scale_color_viridis_c("Density", option = "plasma", trans = "log10") +
  facet_wrap(vars(Island), nrow = 4, scales = 'free') +
  coord_cartesian(xlim = c(-2.5,2.5), ylim = c(-2,2)) +
  theme_bw() + theme(panel.grid = element_blank()) +
  
  patchwork::plot_layout(ncol = 2, guides = "collect")











#### Observe census placements at each component ####

plt_dt_sites <-  plyr::rbind.fill(Map(function(x, y, i) {
  eig <- length(x$CCA$eig)
  d <- data.frame(Code = rownames(vegan::scores(x, choices = 1, display = "sites")),
                  vegan::scores(x, choices = 1:8, display = "sites"),
                  vegan::scores(x, choices = eig + 1:5, display = "sites"))
  d <- data.frame(d, y$pred)
  d$Island <- rep(i, nrow(d))
  d
}, RDA_boot, Dt_boot, names(RDA_boot)))

library(magrittr); library(ggplot2)

#Biplot
plt_dt_sites %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::mutate(Year = as.factor(year)) %>%
  ggplot(aes(x = dbRDA1, y = dbRDA2, color = Year, group = Year)) +
  geom_point() + #ggrepel::geom_text_repel(aes(label = Year)) +
  geom_vline(xintercept = 0, linetype = 3, col = "darkgrey") +
  geom_hline(yintercept = 0, linetype = 3, col = "darkgrey") +
  scale_color_viridis_d() +
  facet_wrap(vars(Island), nrow = 2, scales = 'free') +
  theme_bw() + theme(panel.grid = element_blank())


#Scores as a function of years
plt_dt_sites %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha", 
              'rocas' = "Rocas Atoll", 
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  dplyr::mutate(Year = year) %>%
  reshape2::melt(id.vars = c("Island", "Year"), 
                 measure.vars = paste0("dbRDA", 1:5),
                 variable.name = "Axes", value.name = "Scores") %>%
  # dplyr::filter(scores > 0.2 | scores < -.2) %>%
  ggplot(aes(x = Year - 2000, y = Scores, color = Year)) +
  geom_smooth(method = "glm", col = "darkgrey", fill = "grey", alpha = .2,
              data = plt_dt %>%  
                reshape2::melt(id.vars = c("Island", "Year"), 
                               measure.vars = paste0("dbRDA", 1:5),
                               variable.name = "Axes",value.name = "Scores") %>%
                plyr::mutate(Island = plyr::revalue(
                  Island, c('noronha' = "Fernando de Noronha", 
                            'rocas' = "Rocas Atoll", 
                            'stpauls_rocks' = "St. Paul's Rocks",
                            'trindade' = "Trindade Island"))) %>% 
                signif(variable = "Year", names = c("Island", "Axes"),
                       form = as.formula(Scores ~ Year))) +
  # geom_line(stat = "smooth", alpha = 0.2, method = "loess", se = FALSE) +
  geom_point(alpha = .7) + scale_color_viridis_c() +
  facet_grid(rows = vars(Axes), cols = vars(Island), scales = "free") +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  scale_x_continuous(breaks = c(2*1:10)) +
  theme_bw() + theme(panel.grid = element_blank())

##





















#### OLD STUFF #####
#### Pairwise comparison mof moments in time ####
# Pairwise Adonis
# This function pairwisely perform the Adonis Algorith based on 3 conditions
# First conditions: No pred
# Second: A single pred
# Third: Multiple preds
# Require as input a community matrix (as a data.frame) and a 
# verctor of factors containing the desired groups to compare. It is possible
# to chose the dissimilarity method used by the function and to adjust or not
# the p values of pairwise comparisons
pairwise_adonis <- function(x, factors, preds = NULL, effects = '+',
                            sim.method = 'bray', p.adjust.m ='bonferroni', ...) {
  
  #Vectors to receive outputs
  co <- combn(unique(as.character(factors)),2)
  pairs <- c()
  F.Model <- c()
  R2 <- c()
  Res_R2 <- c()
  p.value <- c()
  DF <- c()
  RDF <- c()
  
  #Is preds not provided?
  if (is.null(preds)) {
    for(elem in 1:ncol(co)){
      #Subset Data, factors and predictors
      i <- x[factors %in% c(co[1, elem], co[2, elem]), ]
      f  <- factors[factors %in% c(co[1, elem], co[2, elem])]
      
      #Convert Data into similarity
      x1 <- vegan::vegdist(i, method = sim.method)
      
      #Perform Adonis
      ad <- vegan::adonis(x1 ~ f, ...)
      
      #Save outputs
      pairs <- c(pairs, paste(co[1, elem], 'vs', co[2, elem]))
      DF <- c(DF, ad$aov.tab['Total', 'Df'])
      RDF <- c(RDF, ad$aov.tab['Residuals', 'Df'])
      F.Model <- c(F.Model, ad$aov.tab[1, 4])
      R2 <- c(R2, ad$aov.tab[1, 5])
      Res_R2 <- c(Res_R2, ad$aov.tab['Residuals', 'R2'])
      p.value <- c(p.value, ad$aov.tab[1, 6])
    }
  } else
    
    #Are there a single preditor?
    if (class(preds) %in% c('character', 'factor', 'numeric', 'integer')) {
      for(elem in 1:ncol(co)){
        #Subset Data, factors and predictors
        i <- x[factors %in% c(co[1, elem], co[2, elem]), ]
        f  <- factors[factors %in% c(co[1, elem], co[2, elem])]
        p  <- preds[factors %in% c(co[1, elem], co[2, elem])]
        
        #Perform Adonis
        ad <- vegan::adonis(i ~ p + f,  method = sim.method, ...)
        
        #Save outputs
        pairs <- c(pairs, paste(co[1, elem], 'vs', co[2, elem]))
        DF <- c(DF, ad$aov.tab['Total', 'Df'])
        RDF <- c(RDF, ad$aov.tab['Residuals', 'Df'])
        F.Model <- c(F.Model, ad$aov.tab[2, 4])
        R2 <- c(R2, ad$aov.tab[2, 5])
        Res_R2 <- c(Res_R2, ad$aov.tab['Residuals', 'R2'])
        p.value <- c(p.value, ad$aov.tab[2, 6])
      }
    } else  
      
      #Are there more than 1 preds?
      if (class(preds) %in% c('data.frame', 'matrix')) {
        for(elem in 1:ncol(co)){
          #Subset Data, factors and predictors
          i <- x[factors %in% c(co[1, elem], co[2, elem]), ]
          f  <- factors[factors %in% c(co[1, elem], co[2, elem])]
          p  <- preds[factors %in% c(co[1, elem], co[2, elem]), ]
          #Bind predictors and factors togeter
          d <- cbind(p, f)
          n <- ncol(d)
          #Generate models character
          model <- paste('i ~ ', paste(colnames(d), collapse = effects))
          
          #Perform Adonis
          ad <- vegan::adonis(as.formula(model), data = d,  
                              method = sim.method,  ...)
          
          #Save outputs
          pairs <- c(pairs, paste(co[1, elem], 'vs', co[2, elem]))
          DF <- c(DF, ad$aov.tab['Total', 'Df'])
          RDF <- c(RDF, ad$aov.tab['Residuals', 1])
          F.Model <- c(F.Model, ad$aov.tab[n, 'F.Model'])
          R2 <- c(R2, ad$aov.tab[n, 'R2'])
          Res_R2 <- c(Res_R2, ad$aov.tab['Residuals', 'R2'])
          p.value <- c(p.value, ad$aov.tab[n, 'Pr(>F)'])
        }
      }
  
  sig = c(rep('', length(p.value)))
  sig[p.value <= 0.1] <-'.'
  sig[p.value <= 0.05] <-'*'
  sig[p.value <= 0.01] <-'**'
  sig[p.value <= 0.001] <-'***'
  pairw.res <- data.frame(pairs, Total.Df = DF, Residual.Df = RDF,
                          F.model = round(F.Model, 2),
                          R2 = round(R2, 2), 
                          Res_R2 = round(Res_R2, 2),
                          p.value = p.adjust(p.value, method = p.adjust.m))
  
  
  class(pairw.res) <- c("pwadonis", "data.frame") 
  return(pairw.res)
}

#Perform Adonis algorithm pairwisely considering several predictors
system.time( #10 seconds for 10,000 permutations and 100 seconds for 100,000
  PW_Perm <- parallel::mclapply(Data_sts, function(x) {
    pairwise_adonis(x[['mass']], 
                    factors = x[['time']], effects = '+',
                    preds = data.frame('c_depth' = x[['c_depth']], 
                                       'sites' = x[['sites']]),
                    p.adjust.m = 'bonferroni', permutations = 10000)
  }, mc.cores = getOption("mc.cores", 4))
)

PW_Perm
#### Composition NMDS ####
#Perform an nmds in a community data matrix. Further arguments are passed to 
#vegan's metaMDS
Data_sts <-readRDS("R_Objects/Data_sts.rds")

Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")

# Perform NMDS on sites composition data
NMDS <- lapply(Data_sts, function(x) {
  vegan::scores(vegan::metaMDS(x[['mass']],
                               distance = 'bray', try = 30, trymax = 10000, 
                               autotransform = TRUE, 
                               parallel = getOption("mc.cores", 8)))
})

# #Change axis sign to make graph 
# NMDS$trindade[,"NMDS1"] <- NMDS$trindade[,"NMDS1"]*-1

#Bind results togeter
NMDS <- plyr::rbind.fill( Map( function(x, y, z) 
  data.frame(x, as.numeric(y[['depth']]), as.character(y[['sites']]), 
             as.numeric(y[['year']]), as.factor(y[['c_depth']]),
             as.character(y[['time']]),
             rep(z, length( y[['depth']]))), NMDS, Data_sts, names(Data_sts)))
#Add names
(colnames(NMDS) <- c('NMDS1', 'NMDS2', 'Depth', 'Sites', 'Year', "C_Depth", 
                     'Time', 'Island'))

#### Simper on Time ####
#Apply a simper analysis to display species in the NMDS
Simper <- lapply(Data_sts, function(x) {
  #Calculate percentage contributions
  smpr <- summary(vegan::simper(x[['mass']], x[['time']]))
  #Filter for contributions < 70%
  spp <- lapply(smpr, function(y) {
    rownames(y)[y$cumsum <= 0.7]
  })
  spp
  
})


#Filter for Times with significant differences
(Simper <- Map(function(y) unique(unlist(y)), 
               Simper))

saveRDS(Simper, "R_Objects/simper.rds")

#Calculate species ordination scores
SppScrs <- plyr::rbind.fill((Map(function(x, y, z) {
  d <- data.frame(vegan::wascores(x = x[, c('NMDS1', 'NMDS2')], y[['mass']]))
  d$species <- rownames(d)
  d <- d[d$species %in% z,]
  d$Island <- rep(unique(x$Island), length(d$species))
  d
}, split(NMDS, NMDS$Island), Data_sts, Simper)))


#### Prepare data for Plotting ####

require(magrittr)
SppScrs <- SppScrs %>%
  plyr::mutate(Island=plyr::revalue(Island,c('stpauls_rocks'="St. Paul's Rocks",
                                             'rocas' = "Rocas Atoll",
                                             'noronha'="Fernando de Noronha",
                                             'trindade' = "Trindade Island")))

#Average NMDS scores within years within Islands
yrNMDS <- dplyr::group_by(NMDS, Island, C_Depth, Year, Time)
yrNMDS <- dplyr::summarise(yrNMDS, NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))
#Average scores within depths to get island centroids
data.table::setDT(yrNMDS)
mnNMDS <- yrNMDS[, list( NMDS1 = mean(NMDS1), SD1 = sd(NMDS1)/length(NMDS1),
                         NMDS2 = mean(NMDS2), SD2 = sd(NMDS2)/length(NMDS2)),
                 by = list(Island, Year, Time)]

mnNMDS <- mnNMDS %>%
  #Change Islands names for the plots
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, c('noronha' = "Fernando de Noronha",
              'rocas' = "Rocas Atoll",
              'stpauls_rocks' = "St. Paul's Rocks",
              'trindade' = "Trindade Island"))) %>%
  plyr::mutate(Time = forcats::fct_relevel(Time, "Previous", "Begin", 
                                           "Mid", "End"))
#### Plot ####

Graph1 <- NMDS %>%  
  #Reorder Islands and  change their names
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island =plyr::revalue(Island,c('stpauls_rocks'="St. Paul's Rocks",
                                              'noronha' = "Fernando de Noronha", 
                                              'rocas' = "Rocas Atoll", 
                                              'trindade'="Trindade Island")))%>%
  plyr::mutate(Time = forcats::fct_relevel(Time, "Previous", "Begin", 
                                           "Mid", "End")) %>%
  # plyr::mutate(C_Depth = forcats::fct_relevel(C_Depth, "Shallow", "Mid", "Deep", 
  #                                             "Open", "Closed")) %>%
  #Call a new plot
  ggplot2::ggplot(ggplot2::aes(x = NMDS1, y = NMDS2)) +
  
  
  # #Add time hulls
  # ggalt::geom_encircle(ggplot2::aes(color = Time, fill = Time), linetype = 1, size = 1,
  #                      alpha = 1, expand = 0.01, s_shape = 1.3) +
  # #Edit hull colors
  # ggplot2::scale_color_viridis_d(option = "C", alpha = 0.4, end = .9) +
  # ggplot2::scale_fill_viridis_d(option = "C", alpha = 0.01, end = .9) +
  # # # ggplot2::scale_color_manual(values = (c("#004C8E33", "#52BF0033",
  # #                                         "#F7880033", "#F9000433"))) +
  # # ggplot2::scale_fill_manual(values = (c("#004C8E10", "#52BF0010",
# #                                        "#F7880010", "#F9000410"))) +
# 
# # # Add thick arrow
# # ggplot2::geom_path(ggplot2::aes(x = NMDS1, y = NMDS2),
# #                    size = 1, alpha = .4, color = 'darkgrey',
# #                    data = mnNMDS, linetype = 2,
# #                    arrow = grid::arrow(angle = 20,
# #                                        length = grid::unit(0.3, 'cm'),
# #                                        type = 'closed')) +
# ggnewscale::new_scale_colour() + 
ggplot2::scale_color_viridis_d(option = "C", alpha = 1, end = .9) +
  # ggplot2::scale_color_manual(values = (c("#004C8EAA", "#52BF00AA",
  #                                         "#F78800AA", "#F90004AA"))) +
  
  #Add horizontal error bars for each year
  ggplot2::geom_segment(ggplot2::aes(x = NMDS1 - SD1, y = NMDS2,
                                     xend = NMDS1 + SD1, yend = NMDS2,
                                     color = Time), data = mnNMDS, alpha = .7) +
  #Add vertical error bars for each year
  ggplot2::geom_segment(ggplot2::aes(x = NMDS1, y = NMDS2 - SD2,
                                     xend = NMDS1, yend = NMDS2 + SD2,
                                     color = Time), data = mnNMDS, alpha = .7) +
  #Add points representing year mean
  ggplot2::geom_point(ggplot2::aes(x = NMDS1, y = NMDS2,
                                   color = Time), alpha = 0.7,
                      data = mnNMDS) +
  #Add year labels
  ggrepel::geom_text_repel(ggplot2::aes(x = NMDS1, y = NMDS2,
                                        color = Time, label = Year), size = 4, alpha = 1,
                           box.padding = .5, label.padding = 0.07, data = mnNMDS) +
  ggplot2::coord_flip() +
  #Add points for species
  # ggplot2::geom_point(ggplot2::aes(x = NMDS1, y = NMDS2),
  #                     data = SppScrs, alpha = 0.7) +
  
  #Add 0 lines
  # ggplot2::geom_hline(yintercept = 0, lty = 3, alpha = 0.3) +
  # ggplot2::geom_vline(xintercept = 0, lty = 3, alpha = 0.3) +
  
  #Split Islands
  ggplot2::facet_wrap(ggplot2::vars(Island), scales = "free", nrow = 2) +
  
  #Make the background white
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "top") +
  ggplot2::labs(title  = "Changes in composition through time")
Graph1

# rm(list = ls()[!ls() %in% c('Graph1', 'Perm', 'PW_Perm')])

