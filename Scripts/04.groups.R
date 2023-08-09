#### Changes on groups through time ####
#Goal here is to observe fluctuations in biomass of groups of species.
#That is, simplify communities to make em easier to understand

#### Load data ####
Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")
# Simper <- readRDS("R_Objects/simper.rds") #Important species
traits <- readODS::read_ods("Data/traits_210606.ods")
Data <-readRDS("R_Objects/Data.rds") #Biomass data
Dt_boot <- readRDS("R_Objects/Dt_boot.rds")
atl_trt_dt <- readxl::read_excel("Data/Base_Juan.xlsx")


#Create groups
traits$groups <- atl_trt_dt$Diet[match(traits$code, atl_trt_dt$Code)]

grps <- data.frame(code = traits$code,
                   #Based on size and trophic level
                   group1 = paste(ifelse(traits$MeanSize < 15, 
                                         "Small", "Large"), 
                                  ifelse(traits$FoodTroph < 3, "1st", 
                                         ifelse(traits$FoodTroph >= 4,"3rd",
                                                "2nd")), sep = "_"),
                   #Based on previous classifications
                   group2 = atl_trt_dt$Diet[match(traits$code, 
                                                  atl_trt_dt$Code)],
                   group3 =ifelse(traits$Planctivorous == 1, "plank",
                                  paste(ifelse(traits$MeanSize < 15,
                                               "Small", "Large"),
                                        ifelse(traits$FoodTroph < 3, "1st",
                                               ifelse(traits$FoodTroph>=4,"3rd",
                                                      "2nd")), sep = "_")))
# [match(Dt$Species,
#                                      traits$code), 'groups']


library(magrittr)
##### Make data into a data.frame ####
Dt <- plyr::rbind.fill(Map(function (x, y) {
  # Combine species into groups
  #Melt data
  m <- data.frame(index = 1:nrow(x[["mass"]]), x[["mass"]]) %>%
    reshape2::melt(id.vars = "index", 
                   variable.name = "code",
                   value.name = "mass")
  #Add groups info
  #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$$#$#$#$#$#$#$#
  #$#$#$#$#$#$# Edit groups here #$#$#$#$#$#$#
  #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$$#$#$#$#$#$#$#
  m$Group <- grps$group1[match(m$code, grps$code)]
  
  #Sum biomass within groups
  m <- m %>% dplyr::group_by(index, Group) %>% 
    dplyr::summarise(mass = sum(mass))
  
  #Transform back into a data.frame
  m <- reshape2::dcast(m, index ~ Group, value.var = "mass", 
                       fun.aggregate = sum, fill = 0)
  
  d <- data.frame(x[["pred"]], m)
  # Melt species biomass into a single column
  d <- reshape2::melt(d, id.var = c("year", "sites", "boot",
                                    "depth", "c_depth",
                                    'obsvr', "transect_id", "index",
                                    "lat", "lon"))
  # Change colnames 
  colnames(d) <- c("Year", "Sites", "Boot",
                   "Depth", "Env",  
                   'Observer', "Transect", 'Index',
                   "Lat", "Lon", 
                   "Group", "Biomass")
  
  d$Island <-  rep(y, nrow(d))
  d
}, Dt_boot, names(Dt_boot)))



#How many censuses are available at each site?
data.frame(Dt %>%
             dplyr::group_by(Island, Sites) %>%
             dplyr::summarise(dplyr::n_distinct(Index)))

# Change characters for factors
Dt[] <- lapply(Dt, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else x
})

#### GLM categorical year ####

#Gams where years are treated as categorical #
glmgrp <- parallel::mclapply(Islands, function(x, d) {
  d$Year <- as.factor(d$Year)
  # require(mgcv)
  # require(parallel)
  
  #Fit a simple lm to get parameters starting points
  g1 <- lme4::lmer(
    #Model
    log10(Biomass + 1) ~ 
      Group * Year + Depth + Depth:Group +
      (1 | Sites),
    #Data
    data = d[d$Island == x, ])
  g1
}, d = Dt, mc.cores = getOption('mc.cores', 4L)); names(glmgrp) <- Islands

lapply(glmgrp, plot)

##### Predict Categorical GLMM Outputs #####
library(magrittr)
pred <- data.frame(Dt %>%
                     dplyr::group_by(Island, Year, Group) %>%
                     dplyr::summarise(Depth = 5))

#Add a custom depth
pred$Depth <- ifelse(pred$Island == "rocas", 2,
                     ifelse(pred$Island == "stpauls_rocks", 15,
                            ifelse(pred$Island == "trindade", 10,
                                   ifelse(pred$Island == "noronha", 10,0))))

pred <- dplyr::arrange(pred, Island, Group, Year)

# #Add an observer
# pred$Observer <- c("gugaw_ferreira", "carlos_ferreira",
#                    "renato_morais", "hudson_pinheiro")[
#                      match(pred$Island, Islands)]
pred$Sites <- c("enseada", "cagarras", "barretinha", "calheta")[
  match(pred$Island, Islands)]


pred


#Predict species biomass plus C.I.. on each year
pred <- parallel::mclapply(Islands, function(i, glms, d) {
  
  d$Year <- as.factor(d$Year)
  #Predict output 
  prd <- merTools::predictInterval(merMod = glms[[i]], 
                                   newdata = d[d$Island == i, ],
                                   level = 0.95, n.sims = 1000, which = "fixed",
                                   stat = "median", type="linear.prediction",
                                   include.resid.var = FALSE)
  
  #Combine with predicted data
  data.frame(d[d$Island == i, ], prd)
  
}, glmgrp, pred, mc.cores = 4)

pred <- plyr::rbind.fill(pred)

head(pred)

#Transform predicted values back to biomass unit
pred[,c("fit", "upr", "lwr")] <- 10^pred[,c("fit", "upr", "lwr")]


head(pred)
library(ggplot2); library(magrittr)


# pred$Size <- ifelse(stringr::str_detect(pred$Group, "Large"), "Large", "Small")

Trend <- pred %>%
  #Change Islands names and edit groups order
  plyr::mutate(Year = as.factor(as.numeric(as.character(Year))-2000)) %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha", 
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
                                            "Large_1st", "plank", "Small_2nd", 
                                            "Small_1st")) %>%
  plyr::mutate(Group = plyr::revalue(Group, 
                                     c("Large_3rd" = "Large 3rd Consumers", 
                                       "Large_2nd" = "Large 2nd Consumers",
                                       "Large_1st" = "Large 1st Consumers", 
                                       "plank" = "Small planktivores",
                                       "Small_2nd" = "Small 2nd Consumers", 
                                       "Small_1st" = "Small 1st Consumers"))) %>%
  
  #Call plot
  ggplot(aes(x = Year, y = fit/40 + 1, 
             color = Group, fill = Group, group = Group)) + 
  #Make a White background
  theme(panel.grid.major.x = element_line(size=.05, 
                                          color = rgb(0.5,0.5,0.5, 0.1)),
        panel.grid.major.y = element_line(size=.05, 
                                          color = rgb(0.5,0.5,0.5, 0.2)),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray", 
                                    fill=NA, size=.5),
        legend.position = "top") +
  #Add SE background from gam with less knots
  geom_ribbon(aes(ymin = lwr/40 + 1,
                ymax = upr/40 + 1,
                colour = NULL), 
            alpha = 0.2) +
  #Add line from gam with less knots
  geom_point(aes(y = fit/40 + 1), alpha = .5) +
  
  geom_line(aes(y = fit/40 + 1), alpha = .8, size = .8) +
  #Scale y axis
  # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #   labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10() +
  #Split Islands
  facet_wrap(vars(Island), scales = "free", ncol = 4) +
  #Labels
  labs(#title = "Trends in biomass",
       x = "Time Series", 
       y = "Biomass (g/m² + 1)"); Trend


#### Net changes in species biomass #####
#Goal here is to test differences in biomass between the first and last 3 years

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
  ggplot(aes(x = Biomass + 1, y = Group)) +
  ggridges::geom_density_ridges() + 
  coord_cartesian(xlim = c(1, NA)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  scale_x_log10() +
  theme_minimal()

#Merge Groups and Time into a single predictor
Dt_diff$GrpTime <- paste0(Dt_diff$Group, "_", Dt_diff$Time) 

glmdiff <- parallel::mclapply(Islands, function(x, d){
  
  # library(lme4); library(magrittr)
  
  
  system.time(
    
    g1 <- lm(
      #Model
      log(Biomass + 1) ~  GrpTime + Group*Depth + Group:Sites
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
  ggplot(aes(y = Resid, x = Pred)) +
  geom_point(alpha = .3, color = "grey") + 
  # coord_cartesian(xlim = c(-1, 1)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  # scale_x_log10() +
  theme_minimal()

#Residual per species
data.frame(Dt_diff, 
           plyr::ldply(glmdiff, function(x) data.frame(Resid = resid(x)))) %>%
  ggplot(aes(x = Resid, y = Group)) +
  ggridges::geom_density_ridges() + 
  coord_cartesian(xlim = c(-2, 2)) +
  facet_wrap(vars(Island), ncol = 4, scales = 'free') +
  # scale_x_log10() +
  theme_minimal()

#Test for heteroskedasticity in parameters estimate
lapply(glmdiff, lmtest::bptest)
#True for all models

# #Estimate parameters considering lack of variance homogeneity
MdlsGrp_cft <- lapply(glmdiff, function(m) {
  lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))})
MdlsGrp_cft


#### Test for Changes ####
# Compare if the parameter before is different from the parameter after.
# Apply a t-test with known mean and SE. The t statistic is calculated as the 
# ratio between biomass and their SEs. 
# By doing so, we are comparing changes in biomass by observing differences 
# in parameter estimates.
# p values uses residual degrees of freedom 


#For each Island
DiffGrp <- parallel::mcMap(function(x, m, grp) {
  
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
  nms <- rownames(s)[grepl("GrpTime", rownames(s)) |
                       grepl("(Intercept)", rownames(s))]
  nms <- stringr::str_remove(nms, "GrpTime")
  nms[1] <- c("Large_1st_Begin")
  
  ss <- `rownames<-`(s[rownames(s)[grepl("GrpTime", rownames(s)) |
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
    
    return(data.frame(Island = x, Group = y, Fit = dif, SE = se, t_value, p))
    
  }, ss, m)
  
}, x = names(glmdiff), m = glmdiff, 
grp = Dt_diff %>% 
  dplyr::group_by(Island, Group) %>% 
  dplyr::summarise() %>% 
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  split(f = .$Island) %>% 
  lapply(FUN = function(x) x %>% dplyr::pull(Group)), 
mc.cores = getOption('mc.cores', 4L))


head(DiffGrp)


#### Plot Net Changes ####

#Bind estimates into a single data.frame
DiffGrp <- plyr::rbind.fill(DiffGrp)
DiffGrp <- plyr::mutate(DiffGrp, 
                        Changed = ifelse(Fit < 0 & p < .05, "Decreased",
                                         ifelse(Fit > 0 & p < .05,
                                                "Increased", "Unchanged")))


Net <- DiffGrp %>%
  plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha",
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  plyr::mutate(Group = forcats::fct_relevel(Group, "Small_1st", "Small_2nd",
                                            "Large_1st", "Large_2nd",
                                            "Large_3rd")) %>%
  plyr::mutate(Group = plyr::revalue(
    Group, c("Large_3rd" = "L3C",
             "Large_2nd" = "L2C",
             "Large_1st" = "L1C",
             "Small_2nd" = "S2C",
             "Small_1st" = "S1C"))) %>%
  ggplot(aes(x = exp(Fit), y = Group, color = Changed)) + 
  #Adjust axes
  scale_x_log10() +
  # 
  # scale_y_discrete(breaks = new_order, 
  #                  labels = gsub("^.*\\.", "", new_order)) +
  # expand_limits(x = c(0.5,1.5)) +
  geom_vline(xintercept = exp(0)) +
  
  #Split data into grids of graphs
  facet_wrap(facets = vars(Island), 
             ncol = 4, scales = 'free') +
  
  #Add points and lines
  geom_pointrange(aes(xmin = exp(Fit - SE),
                      xmax = exp(Fit + SE)), 
                  show.legend = FALSE) +
  
  scale_color_manual(values = c("blue", "red", "grey")) +
  
  #Make a White background
  theme(panel.grid.major.x = element_line(size=.1, 
                                          color = rgb(0.9,0.9,0.9)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "gray", 
                                    fill=NA, size=.5),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank()
        ) +
  
  #Labels
  labs(y = "Group", #title = "Net changes",
       x = "Relative changes in groups standing biomass"); Net



Trend + Net + 
  patchwork::plot_layout(guides = 'keep', design = "
  1
  1
  2")


#











# #### Old stuff #####
# 
# 
# Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")
# # Load data
# Grp_Data <-readRDS("R_Objects/Grp_Data.rds")
# 
# 
# #### Transform into a data.frame ####
# # Transform the list into a data.frame
# plt_grps <- lapply(Grp_Data, function (x) {
#   # Create the data.frame
#   ag <- data.frame("year" = x[["year"]], "sites" = x[["sites"]], 
#                    "depth" = x[["depth"]], 'obsvr' = x[['obsvr']],
#                    "transect" = rownames(x[["biomass"]]), x[["biomass"]])
#   # Melt species biomass into a single column
#   ag <- reshape2::melt(ag, id.var = c("year", "sites", "depth", 'obsvr', "transect"))
#   # Create separate columns for size and trophic level
#   s <- t(simplify2array(strsplit(as.character(ag$variable), split = "_")))
#   r <- cbind(ag, s)
#   # Improve colnames for the plot
#   colnames(r) <- c("Year", "Sites", "Depth", 'Observer', "Transect", "Group",
#                    "Biomass", "Size", "Trophic")
#   r
# })
# 
# 
# # Unite the Islands into a single data.frame
# plt_grps <- plyr::rbind.fill(lapply(Islands, function(x) {
#   #Add Island name vector
#   y <- cbind(plt_grps[[x]], "Island" = rep(x, nrow(plt_grps[[x]])))
#   #Mark y for temporal autocorrelation
#   y <- itsadug::start_event(y, column = 'Year', event = c("Island", "Group"), label.event = "Event")
# }))
# 
# #Change characters for factors
# plt_grps[] <- lapply(plt_grps, function(x) {
#   if (is.character(x)) {
#     return(as.factor(x))
#   } else x
# })
# 
# #### First look using loess ####
# #Observe data before running the model
# require(magrittr); library(ggplot2)
# Year <- plt_grps %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(Island, c('noronha' = "Fernando de Noronha", 
#                                                 'rocas' = "Rocas Atoll", 
#                                                 'stpauls_rocks' = "St. Paul's Rocks",
#                                                 'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#                                             "Large_1st", "Small_2nd", "Small_1st")) %>%
#   #Call a new plot
#   ggplot(aes(x = Year - 2000, y = Biomass + 1, 
#              group = interaction(Group, Sites),
#              color = Group)) +
#   #Add points
#   # geom_jitter(width = 0.25, alpha = 0.1) +
#   #Produce smoothed tendencies for each site
#   geom_smooth(method = 'gam', size = 0.8,
#               formula = y ~ s(x, bs = "cs", k = 4), 
#               se = FALSE) + 
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free_x') + 
#   #Scale y axis to log and clean background
#   scale_y_log10() + 
#   scale_x_continuous(breaks = 06:19)+
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank())
# 
# 
# #Call a second plot
# Depth <-  plt_grps %>%
#   #Change Island names and redefine group order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(Island, c('noronha' = "Fernando de Noronha", 
#                                                 'rocas' = "Rocas Atoll", 
#                                                 'stpauls_rocks' = "St. Paul's Rocks",
#                                                 'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#                                             "Large_1st", "Small_2nd", "Small_1st")) %>%
#   ggplot(aes(x = Depth, y = Biomass + 1, 
#              group = interaction(Group, Sites),
#              color = Group)) +
#   #Add points
#   geom_jitter(width = 0.25, alpha = 0.1) +
#   #Produce smoothed tendencies for each site
#   geom_smooth(method = 'gam', size = 0.8,
#               formula = y ~ s(x, bs = "cs", k = 4), 
#               se = FALSE) + 
#   #Split each island
#   facet_wrap(vars(Island),
#              ncol = 4,
#              scales = 'free_x') + 
#   #Scale y axis to log and clean background
#   scale_y_log10() + 
#   theme(panel.grid = element_blank(), 
#         panel.background = element_blank())
# 
# # Year + Depth +  patchwork::plot_layout(nrow = 2,  guides = 'collect')
# 
# #### GAMs ####
# #### Gams with max knots ####
# #Apply gams with max knots
# gams1 <- lapply(Islands, function(x, d) {
#   require(mgcv)
#   require(parallel)
#   g1 <- mgcv::bam(
#     #Model
#     (Biomass + 1) ~ 
#       Group +
#       te(Depth, k = 5, by = Group, bs = 'cr')  + 
#       te(Year, by = Group, bs = 'cr', 
#          k = ifelse(x == 'noronha', 8, 
#                     ifelse(x == 'rocas', 9, 
#                            ifelse(x == 'stpauls_rocks', 10, 
#                                   ifelse(x == 'trindade', 11, 
#                                          stop('Island not found')))))) + 
#       #Ifelse must be used as visreg only accept explicit integer inputs
#       s(Sites, by = Group, bs = 'fs'), # Sites as random intercepts
#     #Data
#     data = d[d$Island == x, ], 
#     #Family Link
#     family = Gamma(link = 'log'),
#     #Boost fit
#     discrete = TRUE, nthreads = 4,
#     #Print progress
#     control = mgcv::gam.control(trace = TRUE))
#   g1
# }, d = plt_grps)
# 
# #Readd names
# names(gams1) <- Islands
# 
# #### Gams with 4 knots ####
# #Apply gams with min knots
# gams2 <- lapply(Islands, function(x, d) {
#   require(mgcv)
#   require(parallel)
#   g2 <- mgcv::bam(
#     #Model
#     (Biomass + 1) ~ 
#       Group +
#       te(Depth, k = 5, by = Group, bs = 'cr')  + 
#       te(Year, k = 4,  by = Group, bs = 'cr') + 
#       s(Sites, by = Group, bs = 'fs'), # Sites as random intercepts
#     #Data
#     data = d[d$Island == x, ], 
#     #Family Link
#     family = Gamma(link = 'log'),
#     #Boost fit
#     discrete = TRUE, nthreads = 4,
#     #Print progress
#     control = mgcv::gam.control(trace = TRUE))
#   g2
# }, d = plt_grps)
# #Readd names
# names(gams2) <- Islands
# 
# #### GAMs where years are categorical ####
# # Perform a GAM model to observe fluctuations in biomass
# #Use Years as factors
# gams3 <- lapply(Islands, function(x, d) {
#   d$Year <- as.factor(d$Year)
#   require(mgcv)
#   require(parallel)
#   g3 <- mgcv::bam(
#     #Model
#     (Biomass + 1) ~ 
#       Group +
#       te(Depth, k = 5, by = Group, bs = 'cr') +
#       s(Year, by = Group, bs = 'fs') + 
#       s(Sites, by = Group, bs = 'fs'), # Sites as random intercepts
#     #Data
#     data = d[d$Island == x, ], 
#     #Family Link
#     family = Gamma(link = 'log'),
#     #Boost fit
#     discrete = TRUE, nthreads = 4,
#     #Print progress
#     control = mgcv::gam.control(trace = TRUE),)
#   g3
# }, d = plt_grps)#, mc.cores = getOption('mc.cores', 4L))
# 
# #Readd names
# names(gams3) <- Islands
# 
# #### Predict outputs ####
# ##### Categorical GAM #####
# pred <- data.frame(plt_grps %>%
#   dplyr::group_by(Island, Year, Sites, Group, Size, Trophic) %>%
#   dplyr::summarise())
# 
# pred$Depth <- rep(10, nrow(pred))
# 
# pred <- plyr::ldply(Islands, function(i, gam, dt) {
#   #Predict output 
#   prd <- predict(gam[[i]], split(pred, pred$Island)[[i]], type = "response")
#   se <- predict(gam[[i]], split(pred, pred$Island)[[i]],
#                  type = "response", se.fit = TRUE)$se
#   #Return with data
#   data.frame(Pred = prd, SE = se, split(pred, pred$Island)[[i]])
#   }, gams3, pred)
# 
# head(pred)
# 
# par(mfrow = c(4,2))
# 
# pred[pred$Sites %in% c("enseada", "barretinha", "calheta", "cagarras"),] %>%
#  plyr::mutate(Island = forcats::fct_relevel(Island,
#                                            Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha",
#               'rocas' = "Rocas Atoll",
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(
#     Group, "Large_3rd", "Large_2nd",
#     "Large_1st", "Small_2nd", "Small_1st")) %>%
#   plyr::mutate(Group = plyr::revalue(
#     Group, c("Large_3rd" = "Large Top Predators",
#              "Large_2nd" = "Large Mesopredators",
#              "Large_1st" = "Large Herbivores",
#              "Small_2nd" = "Small Mesopredators",
#              "Small_1st" = "Small Herbivores"))) %>%
#   
#   #Call plot
#   ggplot(aes(x = Year - 2000, y = Pred/40 + 1, 
#              color = Group, fill = Group, group = Group)) + 
#   
#   #Add points
#   stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = .1) +
#   geom_ribbon(aes(ymin = (Pred - SE)/40 + 1,
#                   ymax = (Pred + SE)/40 + 1,
#                   colour = NULL), 
#               alpha = 0.2) +
#   stat_summary(fun.data = "mean_cl_boot", geom = "line") +
#   
#   # Adjust axes
#   scale_y_log10() +
#   scale_x_continuous(breaks = c(06:19)) +
#   # coord_cartesian(ylim = c(1, 500)) +
#   
#   #Split Islands
#   facet_wrap(facets = vars(Island), 
#              scales = "free", ncol = 4) +
#   
#   #Labels
#   labs(title = "Mean biomass through time",
#        x = "Time Series", 
#        y = "Biomass (g/m² + 1)") +
#   
#   #Make a White background
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(size=.05, 
#                                           color = rgb(0.5,0.5,0.5, 0.5)),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_rect(colour = "gray", 
#                                     fill=NA, size=.5),
#         legend.position = "top")
# 
# 
# #Extract predicted values for plots
# x = 'noronha'
# 
# plt <- lapply(Islands, function(x, g1, g2, g3, d) {
#   force(x)
#   
#   
#   # Extract values from the smoother gam
#   h1 <- visreg::visreg(g2[[x]], xvar = 'Year', by = 'Group', type = "conditional",
#                        overlay = TRUE, scale = "response")#, cond = list(Depth = 5))
#   Fit <- data.frame(h1$fit[, c('Group', 'Year', "visregFit", "visregLwr", "visregUpr")])
#   colnames(Fit) <- c('Group', 'Year', 'fit', 'lwr', 'upr')
#   Res <- data.frame(h1$res[, c('Group', 'Year', "visregRes")])
#   colnames(Res) <- c('Group', 'Year', 'res')
#   
#   print(paste(x, 'model 1'))
#   #Extract from the rough gam
#   h2 <- visreg::visreg(g1[[x]], xvar = 'Year', by = 'Group',
#                        overlay = TRUE, scale = "response")#, cond = list(Depth = 5))
#   Fit[, c('fit2', 'lwr2', 'upr2')] <- data.frame(h2$fit[, c("visregFit",
#                                                             "visregLwr",
#                                                             "visregUpr")])
#   print(paste(x, 'model 2'))
#   Fit$Island <- rep(x, nrow(Fit))
#   # Find years interval
#   yr <- range(as.numeric(as.character(d[d$Island == x,"Year"])))
#   yr <- seq(yr[1], yr[2], length.out = 101)
#   # Get groups
#   grp <- unique(d[d$Island == x, "Group"])
#   # Set depth as the mean of all censuses
#   dp <- 0 #mean(unique(d[d$Island == x, "Depth"]))
#   # Set sites as the first one
#   # st <- unique(d[d$Island == x, "Sites"])[1]
#   # 
#   # #Get all combinations of Year, Groups, Depth and Sites
#   # Fit <- expand.grid(yr, grp, dp, st)
#   # colnames(Fit) <- c("Year", "Group", "Depth", "Sites")
#   # 
#   # #Predict for the rougher GAM
#   # Fit[, c("pred", "se")] <- data.frame(predict(g2[[x]], Fit, se.fit = TRUE))
#   # Fit$fit <- exp(Fit$pred) - 1
#   # Fit$lwr <- exp(Fit$pred - Fit$se) - 1
#   # Fit$upr <- exp(Fit$pred + Fit$se) - 1
#   # 
#   # #Predict values for the smoother gam
#   # Fit[, c("pred2", "se2")] <- data.frame(predict(g1[[x]], Fit[, c("Year", "Group",
#   #                                                                 "Depth", "Sites")], se.fit = TRUE))
#   # Fit$fit2 <- exp(Fit$pred2) - 1
#   # Fit$lwr2 <- exp(Fit$pred2 - Fit$se2) - 1
#   # Fit$upr2 <- exp(Fit$pred2 + Fit$se2) - 1
#   # 
#   # #print progress
#   # print(paste(x, 'model 3'))
#   
#   
#   # Find years interval
#   yr2 <- unique(as.numeric(as.character(d[d$Island == x,"Year"])))
#   # Set sites as the first one
#   st2 <- unique(d[d$Island == x, "Sites"])[1]
#   
#   #Get all combinations of Year, Groups, Depth and Sites
#   Fit2 <- expand.grid(yr2, grp, dp, st2)
#   colnames(Fit2) <- c("Year", "Group", "Depth", "Sites")
#   
#   #Predict values for the categorical gam
#   Fit2[, c("pred", "se")] <- data.frame(predict(g3[[x]], 
#                                                 Fit2[, c("Year", "Group", 
#                                                          "Depth", "Sites")],
#                                                 se.fit = TRUE))
#   Fit2$fit <- exp(Fit2$pred) - 1
#   Fit2$lwr <- exp(Fit2$pred - Fit2$se) - 1
#   Fit2$upr <- exp(Fit2$pred + Fit2$se) - 1
#   
#   Fit$Island <- rep(x, nrow(Fit))
#   Fit2$Island <- rep(x, nrow(Fit2))
#   
#   
#   return(list('Fit' = Fit, 'Fit2' = Fit2))
# }, g1 = gams1, g2 = gams2, g3 = gams3, d = plt_grps)
# 
# names(plt) <- Islands
# 
# 
# Fit <- plyr::rbind.fill(lapply(plt, function(x) x[['Fit']]))
# Fit2 <- plyr::rbind.fill(lapply(plt, function(x) x[['Fit2']]))
# 
# 
# #### Plot GAMs ####
# #Add blanks on missing data
# # Fit$fit <- ifelse(Fit$Island == "stpauls_rocks" & Fit$Year > 2009.5 & Fit$Year < 2010.5, NA,
# #                    ifelse(Fit$Island == "noronha" & Fit$Year > 2007.5 & Fit$Year < 2012.5, NA,
# #                           ifelse(Fit$Island == "rocas" & Fit$Year > 2006.5 & Fit$Year < 2011.5, NA,
# #                                  ifelse(Fit$Island == "trindade" & (Fit$Year > 2007.5 & Fit$Year < 2008.5 |
# #                                                                       Fit$Year > 2009.5 & Fit$Year < 2010.5), NA,
# #                                         Fit$fit))))
# Fit$fit2 <- ifelse(Fit$Island == "stpauls_rocks" & Fit$Year > 2009.5 & Fit$Year < 2010.5, NA,
#                    ifelse(Fit$Island == "noronha" & Fit$Year > 2007.5 & Fit$Year < 2012.5, NA,
#                           ifelse(Fit$Island == "rocas" & Fit$Year > 2006.5 & Fit$Year < 2011.5, NA,
#                                  ifelse(Fit$Island == "trindade" & (Fit$Year > 2007.5 & Fit$Year < 2008.5 |
#                                                                       Fit$Year > 2009.5 & Fit$Year < 2010.5), NA,
#                                         Fit$fit2))))
# Fit$upr <- ifelse(Fit$Island == "stpauls_rocks" & Fit$Year > 2009.5 & Fit$Year < 2010.5, NA,
#                   ifelse(Fit$Island == "noronha" & Fit$Year > 2007.5 & Fit$Year < 2012.5, NA,
#                          ifelse(Fit$Island == "rocas" & Fit$Year > 2006.5 & Fit$Year < 2011.5, NA,
#                                 ifelse(Fit$Island == "trindade" & (Fit$Year > 2007.5 & Fit$Year < 2008.5 |
#                                                                      Fit$Year > 2009.5 & Fit$Year < 2010.5), NA,
#                                        Fit$upr))))
# Fit$lwr <- ifelse(Fit$Island == "stpauls_rocks" & Fit$Year > 2009.5 & Fit$Year < 2010.5, NA,
#                   ifelse(Fit$Island == "noronha" & Fit$Year > 2007.5 & Fit$Year < 2012.5, NA,
#                          ifelse(Fit$Island == "rocas" & Fit$Year > 2006.5 & Fit$Year < 2011.5, NA,
#                                 ifelse(Fit$Island == "trindade" & (Fit$Year > 2007.5 & Fit$Year < 2008.5 |
#                                                                      Fit$Year > 2009.5 & Fit$Year < 2010.5), NA,
#                                        Fit$lwr))))
# 
# # Compute actual means to compare
# # ptrg <- plt_grps %>% 
# #   dplyr::group_by(Year, Group, Island) %>%
# #   dplyr::summarise(mn = mean(Biomass), 
# #                    lw = quantile(Biomass, c(0.25)),
# #                    up = quantile(Biomass, c(0.75)))
# 
# #Plot smoothed tendencies
# require(magrittr)
# 
# Graph2 <- Fit %>%
#   #Change Islands names and edit groups order
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(Island, 
#                                       c('noronha' = "Fernando de Noronha", 
#                                         'rocas' = "Rocas Atoll", 
#                                         'stpauls_rocks' = "St. Paul's Rocks",
#                                         'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#                                             "Large_1st", "Small_2nd", 
#                                             "Small_1st")) %>%
#   plyr::mutate(Group = plyr::revalue(Group, 
#                                      c("Large_3rd" = "Large Top Predators", 
#                                        "Large_2nd" = "Large Mesopredators",
#                                        "Large_1st" = "Large Herbivores", 
#                                        "Small_2nd" = "Small Mesopredators", 
#                                        "Small_1st" = "Small Herbivores"))) %>%
#   
#   #Call plot
#   ggplot(aes(x = Year - 2000, y = fit/40 + 1, 
#              color = Group, fill = Group)) + 
#   #Make a White background
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(size=.05, 
#                                           color = rgb(0.5,0.5,0.5, 0.5)),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_rect(colour = "gray", 
#                                     fill=NA, size=.5),
#         legend.position = "top") +
#   # #Add points
#   # geom_pointrange(aes(y = mn/40 + 1, ymax = up/40 + 1,
#   #                     ymin = lw/40 + 1,
#   #                     x = Year - 2000,
#   #                     color = Group),
#   #                 alpha = 0.3,  #width = 0.15,
#   #                 data = ptrg %>%
#   #                   #Change Islands names and edit groups order for points
#   #                   plyr::mutate(Island = forcats::fct_relevel(Island,
#   #                                                              Islands)) %>%
#   #                   plyr::mutate(Island = plyr::revalue(
#   #                     Island, c('noronha' = "Fernando de Noronha",
#   #                               'rocas' = "Rocas Atoll",
#   #                               'stpauls_rocks' = "St. Paul's Rocks",
#   #                               'trindade' = "Trindade Island"))) %>%
#   #                   plyr::mutate(Group = forcats::fct_relevel(
#   #                     Group, "Large_3rd", "Large_2nd",
#   #                     "Large_1st", "Small_2nd", "Small_1st")) %>%
#   #                   plyr::mutate(Group = plyr::revalue(
#   #                     Group, c("Large_3rd" = "Large Top Predators",
#   #                              "Large_2nd" = "Large Mesopredators",
#   #                              "Large_1st" = "Large Herbivores",
#   #                              "Small_2nd" = "Small Mesopredators",
#   #                              "Small_1st" = "Small Herbivores")))) +
# 
#   #Add SE background from gam with less knots
#   geom_ribbon(aes(ymin = lwr/40 + 1,
#                   ymax = upr/40 + 1,
#                   colour = NULL), 
#               alpha = 0.2) +
#   #Add line from gam with more knots
#   geom_line(aes(y = fit2/40 + 1), alpha = .8, size = .6, linetype = 2) +
#   
#   
#   #Add line from gam with less knots
#   geom_line(aes(y = fit/40 + 1), alpha = .8, size = .8) +
#   
#   
#   #Scale y axis
#   # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#   #   labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_log10() +
#   scale_x_continuous(breaks = c(06:19)) +
#   # coord_cartesian(ylim = c(1, 500)) +
#   
#   #Split Islands
#   facet_wrap(vars(Island), ncol = 4, scales = "free") +
#   
#   #Labels
#   labs(title = "Mean biomass through time",
#        x = "Time Series", 
#        y = "Biomass (g/m² + 1)")
# 
# Graph2
# 
# #### plot categorical time ####
# # Input missing years
# #Get full combinations of years, groups and islands
# exctg <- expand.grid(unique(2006:2019),
#                      unique(Fit2$Group),
#                      unique(Fit2$Island))
# ctg <- apply(exctg, 1, paste0, collapse = '-')
# 
# #Match to actual data
# octg <- paste(Fit2$Year, Fit2$Group, Fit2$Island, sep = '-')
# 
# Fit2 <- Fit2[match(ctg, octg),]
# 
# #Place cells for years, sites and groups
# Fit2[ , c("Year", "Group", "Island")] <- exctg
# 
# 
# #Remove years previous to samples in some sites
# Fit2 <- Fit2[!((Fit2$Island == "stpauls_rocks" & Fit2$Year %in% c(2006:2008)) |
#                (Fit2$Island == "noronha" & Fit2$Year %in% c(2006)) |
#                (Fit2$Island == "trindade" & Fit2$Year %in% c(2006))), ]
# 
# Graph2_2 <- Fit2[complete.cases(Fit2), ] %>%
#   #Change Islands names and edit groups order for points
#   plyr::mutate(Island = forcats::fct_relevel(Island,
#                                              Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('noronha' = "Fernando de Noronha",
#               'rocas' = "Rocas Atoll",
#               'stpauls_rocks' = "St. Paul's Rocks",
#               'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(
#     Group, "Large_3rd", "Large_2nd",
#     "Large_1st", "Small_2nd", "Small_1st")) %>%
#   plyr::mutate(Group = plyr::revalue(
#     Group, c("Large_3rd" = "Large Top Predators",
#              "Large_2nd" = "Large Mesopredators",
#              "Large_1st" = "Large Herbivores",
#              "Small_2nd" = "Small Mesopredators",
#              "Small_1st" = "Small Herbivores"))) %>%
#   
#   #Call plot
#   ggplot(aes(x = Year - 2000, y = fit/40 + 1, 
#              color = Group, fill = Group)) + 
#   
#   #
#   geom_ribbon(aes(ymin = lwr/40 + 1,
#                   ymax = upr/40 + 1,
#                   colour = NULL), 
#               alpha = 0.2,
#               position = position_dodge(width = 0.5),
#               data = Fit2 %>%
#                 #Change Islands names and edit groups order for points
#                 plyr::mutate(Island = forcats::fct_relevel(Island,
#                                                            Islands)) %>%
#                 plyr::mutate(Island = plyr::revalue(
#                   Island, c('noronha' = "Fernando de Noronha",
#                             'rocas' = "Rocas Atoll",
#                             'stpauls_rocks' = "St. Paul's Rocks",
#                             'trindade' = "Trindade Island"))) %>%
#                 plyr::mutate(Group = forcats::fct_relevel(
#                   Group, "Large_3rd", "Large_2nd",
#                   "Large_1st", "Small_2nd", "Small_1st")) %>%
#                 plyr::mutate(Group = plyr::revalue(
#                   Group, c("Large_3rd" = "Large Top Predators",
#                            "Large_2nd" = "Large Mesopredators",
#                            "Large_1st" = "Large Herbivores",
#                            "Small_2nd" = "Small Mesopredators",
#                            "Small_1st" = "Small Herbivores")))
#               ) +
#   
#   #Add points
#   geom_pointrange(aes(y = fit/40 + 1,  x = Year - 2000,
#                       ymax = upr/40 + 1, ymin = lwr/40 + 1,
#                       color = Group),
#                       alpha = 0.3,
#                   position = position_dodge(width = 0.5)) +
# 
#   #Add lines
#   geom_line(aes(x = Year - 2000, y = fit/40 + 1, color = Group),
#             position = position_dodge(width = 0.5)) +
#   
#   # Adjust axes
#   scale_y_log10() +
#   scale_x_continuous(breaks = c(06:19)) +
#   # coord_cartesian(ylim = c(1, 500)) +
#   
#   #Split Islands
#   facet_wrap(facets = vars(Island), 
#              scales = "free", ncol = 4) +
#   
#   #Labels
#   labs(title = "Mean biomass through time",
#        x = "Time Series", 
#        y = "Biomass (g/m² + 1)") +
#   
#   #Make a White background
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(size=.05, 
#                                           color = rgb(0.5,0.5,0.5, 0.5)),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_rect(colour = "gray", 
#                                     fill=NA, size=.5),
#         legend.position = "top")
# 
# Graph2_2
# #### GAMM residuals ####
# par(mfrow = c(4,4))
# lapply(gams1, summary) # GAM with k = unique(Year)
# lapply(gams2, summary) # GAM with k = 4
# 
# #Make the Same plots with ggplot2
# 
# #Get residuals and predictors from each island
# res <- lapply(Islands, function(x,g1, g2,d) {
#   res <- data.frame('Year' = d[[x]]$Year, 
#                     'Island' = d[[x]]$Island,
#                     'Group' = d[[x]]$Group,
#                     'Biomass' = d[[x]]$Biomass,
#                     'Fitted_1' = fitted(g1[[x]]), 
#                     'Residuals_1' = resid(g1[[x]]),
#                     'Fitted_2' = fitted(g2[[x]]), 
#                     'Residuals_2' = resid(g2[[x]]))
# },gams1, gams2, d = split(plt_grps, plt_grps$Island))
# 
# #Unite predicted values and residuals in a single object
# res <- plyr::rbind.fill(res)
# 
# 
# 
# #Change labels
# require(magrittr)
# res <- res %>% 
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(Island, c('noronha' = "Fernando de Noronha", 
#                                                 'rocas' = "Rocas Atoll", 
#                                                 'stpauls_rocks' = "St. Paul's Rocks",
#                                                 'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(Group, "Large_3rd", "Large_2nd",
#                                             "Large_1st", "Small_2nd", "Small_1st"))
# 
# #### Residual Analysis for Gams 1 ####  
# #LME Residuals vs Fitted  
# res1Graph2 <- ggplot(data = res, aes(x = Fitted_1 + 1,
#                                      y = Residuals_1)) +
#   geom_hline(yintercept = 0, alpha = 0.3) +
#   geom_point(aes(color = Group), alpha = 0.5) +
#   scale_x_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   labs(title = 'Residuals vs Fitted Values') +
#   ylab('Residuals') +
#   xlab('Fitted values') +
#   coord_cartesian(ylim = c(-6, 6))+
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() +
#   
#   # Fitted vs Observed
#   ggplot(data = res, aes(x = Fitted_1 + 1,
#                          y = Biomass + 1)) +
#   #Place log(x + 1) to represent the log link in the model
#   geom_point(aes(color = Group), alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0) +
#   scale_x_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   geom_smooth(method = 'loess') +
#   theme_minimal() + 
#   labs(title = 'Fitted vs Observed') +
#   ylab('Observed values') +
#   xlab('Fitted values') +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() +
#   
#   # Resuduals vs Years
#   ggplot(data = res, aes(x = Year - 2000, y = Residuals_1)) +
#   geom_hline(yintercept = 0, alpha = 0.3) +
#   geom_point(aes(color = Group), alpha = 0.5) +
#   scale_x_continuous(breaks = c(10, 15)) +
#   coord_cartesian(ylim = c(-6, 6)) +
#   theme_minimal() + labs(title = 'Residuals vs X axis') +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() + 
#   #Residuals density  
#   ggplot(aes(x = Residuals_1), data = res) +
#   geom_histogram(aes(y =..density..), alpha=0.8, bins = 23,
#                  fill = rgb(0.6, 0.6, 0.8), color =  rgb(0.5, 0.5, 0.8)) +
#   stat_function(fun = dnorm, args = list(mean = mean(res$Residuals_1), 
#                                          sd = sd(res$Residuals_1))) +
#   labs(title = 'Residuals Density vs Normal Density') + 
#   xlab( 'Residuals') + ylab ('Density') +
#   # coord_cartesian(xlim = c(-3,3), ylim = c(0,0.6)) +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free_x') +
#   theme_minimal() +
#   
#   # QQ plot
#   ggplot(aes(sample = Residuals_1), data = res) +
#   qqplotr::stat_qq_band(bandType = "ks", fill = "#8DA0CB", alpha = 0.4) +
#   qqplotr::stat_qq_line(colour = "#8DA0CB") +
#   qqplotr::stat_qq_point(alpha = 0.2) +
#   xlab("Theoretical quantiles") + ylab("Sample Quantiles") +
#   labs(title = 'QQ Plot with Kolmogorov conf. band') +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() +
#   patchwork::plot_layout(nrow = 5, guides = 'collect') &
#   theme(legend.position='top')
# 
# res1Graph2
# #### Residual Analysis for Gams 2 ####  
# #LME Residuals vs Fitted  
# res2Graph2 <- ggplot(data = res, aes(x = Fitted_2 + 1,
#                                      y = Residuals_2)) +
#   geom_hline(yintercept = 0, alpha = 0.3) +
#   geom_point(aes(color = Group), alpha = 0.5) +
#   scale_x_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   labs(title = 'Residuals vs Fitted Values') +
#   ylab('Residuals') +
#   xlab('Fitted values') +
#   coord_cartesian(ylim = c(-6, 6))+
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() +
#   
#   # Fitted vs Observed
#   ggplot(data = res, aes(x = Fitted_2 + 1,
#                          y = Biomass + 1)) +
#   #Place log(x + 1) to represent the log link in the model
#   geom_point(aes(color = Group), alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0) +
#   scale_x_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   geom_smooth(method = 'loess') +
#   theme_minimal() + 
#   labs(title = 'Fitted vs Observed') +
#   ylab('Observed values') +
#   xlab('Fitted values') +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() +
#   
#   # Resuduals vs Years
#   ggplot(data = res, aes(x = Year - 2000, y = Residuals_2)) +
#   geom_hline(yintercept = 0, alpha = 0.3) +
#   geom_point(aes(color = Group), alpha = 0.5) +
#   scale_x_continuous(breaks = c(10, 15)) +
#   coord_cartesian(ylim = c(-6, 6)) +
#   theme_minimal() + labs(title = 'Residuals vs X axis') +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() + 
#   #Residuals density  
#   ggplot(aes(x = Residuals_2), data = res) +
#   geom_histogram(aes(y =..density..), alpha=0.8, bins = 23,
#                  fill = rgb(0.6, 0.6, 0.8), color =  rgb(0.5, 0.5, 0.8)) +
#   stat_function(fun = dnorm, args = list(mean = mean(res$Residuals_2), 
#                                          sd = sd(res$Residuals_2))) +
#   labs(title = 'Residuals Density vs Normal Density') + 
#   xlab( 'Residuals') + ylab ('Density') +
#   # coord_cartesian(xlim = c(-3,3), ylim = c(0,0.6)) +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free_x') +
#   theme_minimal() +
#   
#   # QQ plot
#   ggplot(aes(sample = Residuals_2), data = res) +
#   qqplotr::stat_qq_band(bandType = "ks", fill = "#8DA0CB", alpha = 0.4) +
#   qqplotr::stat_qq_line(colour = "#8DA0CB") +
#   qqplotr::stat_qq_point(alpha = 0.2) +
#   xlab("Theoretical quantiles") + ylab("Sample Quantiles") +
#   labs(title = 'QQ Plot with Kolmogorov conf. band') +
#   facet_wrap(vars(Island), ncol = 4, scales = 'free') +
#   theme_minimal() +
#   patchwork::plot_layout(nrow = 5, guides = 'collect') &
#   theme(legend.position='top')
# 
# res2Graph2
# #Observe if the residuals provide any temporal autocorrelation
# #Use the mean residual of each year to see
# 
# #Average bt Island and group
# tmp <- dplyr::group_by(res, Island, Group, Year)
# tmp <- dplyr::summarise(tmp, 
#                         Residuals_1 = mean(Residuals_1),
#                         Residuals_2 = mean(Residuals_2),
#                         Biomass = mean(Biomass))
# 
# #Plot an acf for each group on each island
# #Model 1
# itsadug::acf_n_plots(x = tmp$Residuals_1, split_by = list(tmp$Group, tmp$Island), 
#                      n = 20, print.summary = TRUE)
# # No signs of autocorrelation. Maybe due to the small series
# 
# #Model 2
# itsadug::acf_n_plots(x = tmp$Residuals_2, split_by = list(tmp$Group, tmp$Island), 
#                      n = 20, print.summary = TRUE)
# 
# # Weak signs of autocorrelation in Model 2, regarding delays in 1 and 2 years
# #I did not figured out how to order the plots
# 
# #Clear console but important things
# # rm(list = ls()[!ls() %in% c('Graph1', 'Graph2', 'res1Graph2', 'gams1', 'gams2',
# #                             'res2Graph2','Perm', 'PW_Perm')])
# 
# #### Test differences before vs after in groups biomass ####
# #Goal here is to test differences in biomass between the first and last 3 years
# Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")
# 
# #Load Group biomass data
# Grp_Data <-readRDS("R_Objects/Grp_Data.rds")
# 
# #Keep only the first and last 4 years
# plt_grps2 <- lapply(names(Grp_Data), function(i, Data) {
#   x <- Data[[i]]
#   
#   #Create a list to subset
#   sbst <- sort(unique(x[['year']]))
#   
#   #Select the first and last 3 years
#   sbst <- sbst[c(1:4, (length(sbst) - 3):length(sbst))]
#   
#   #Subset
#   mass <- x[["biomass"]][x$year %in% sbst, ]
#   obsvr <- x[["obsvr"]][x$year %in% sbst]
#   year <- x[["year"]][x$year %in% sbst]
#   sites <- x[["sites"]][x$year %in% sbst]
#   depth <- x[["depth"]][x$year %in% sbst]
#   island <- rep(i, length(year))
#   
#   #Add a new vector to indicate 'begin' and 'end'
#   time <- x[["year"]][x$year %in% sbst]
#   time[year %in% sbst[1:4]] <- 'Begin'
#   time[year %in% sbst[5:8]] <- 'End'
#   
#   rtrn <- data.frame(island, time, year, sites, depth, obsvr, mass)
#   rtrn
# }, Grp_Data)
# 
# #Melt biomass data to a single column
# plt_grps2 <- lapply(plt_grps2, reshape2::melt,
#                     id.var = c("island", "time", "year", "sites", 
#                                "year", "obsvr", "depth"))
# 
# #Change rownames
# plt_grps2 <- lapply(plt_grps2,`colnames<-`, c("Island", "Time", "Year", "Sites",
#                                               "Year", "Observer", "Depth",
#                                               "Groups", "Biomass"))
# 
# #Merge again
# plt_grps2 <- plyr::rbind.fill(plt_grps2)
# 
# 
# #Observe biomass distribution within groups
# require(magrittr)
# plt_grps2 %>%
#   #Reorder groups
#   plyr::mutate(Groups = forcats::fct_relevel(Groups, "Small_1st", "Small_2nd",
#                                              "Large_1st", "Large_2nd", 
#                                              "Large_3rd")) %>%
#   ggplot(aes(x = Biomass + 1, y = Groups, 
#              fill = Groups)) +
#   #Add stacked density plots
#   ggridges::geom_density_ridges() +
#   #Adjust x axis
#   scale_x_log10() +
#   coord_cartesian(xlim = c(1,NA)) +
#   #Wrap groups within Islands
#   facet_grid(cols = vars(Island)) +
#   #Add thin background lines
#   theme_minimal()
# 
# #Distribution lies between log-normal and logarithmic
# 
# #Create a model matrix
# plt_grps2$GTime = paste0(plt_grps2$Groups, "_", plt_grps2$Time)
# Mmx <- as.data.frame(model.matrix(log(Biomass + 1) ~ GTime,
#                                   data = plt_grps2))
# 
# #Replace intercept with "GTimeLarge_1st_Begin"
# nm <- colnames(Mmx) 
# nm[1] <- "GTimeLarge_1st_Begin"
# nm <- stringr::str_remove(nm, "GTime")
# colnames(Mmx) <- nm
# 
# plt_grps2  <- cbind(plt_grps2, Mmx)
# 
# colnames(plt_grps2)
# # Apply a lm model (log(biomass) ~ time + depth + sites)
# Mdls <- lapply(Islands, function(x, d) {
#   lm(log10(Biomass + 1) ~  scale(Depth) + Sites + GTime, 
#      data = d, subset = d$Island == x)
# }, plt_grps2)
# names(Mdls) <- Islands
# 
# #Test for heteroskedasticity in parameters estimate
# lapply(Mdls, lmtest::bptest)
# #True for all models
# 
# 
# #Observe residuals frequency distribution
# par(mfrow = c(2,2))
# lapply(Mdls, function(x) hist(resid(x)))
# #Acceptable
# 
# #Return Estimates to the original level for comparison
# Groups <- c("Large_1st", "Large_2nd", "Large_3rd", "Small_1st", "Small_2nd")
# Time <- c("Begin", "End")
# 
# # Compare if the parameter repreenting the group before is different from the
# #parameter after.
# # To do so, we'll apply a t-test with known mean and SE. The t statistic will
# #be calculated from the differences in parameters before and after devided by 
# #the standard error after. By doing so, we are comparing changes in biomass
# #by observing differences in parameter estimates
# # We are testing the t value under the residual degrees of freedom value
# 
# 
# #For each Island
# Diff <- lapply(Islands, function(x, mdl, grp) {
#   #Save model as a different object
#   m <- mdl[[x]]
#   
#   #Estimate Std Errors from covariance matrix
#   s <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
#   
#   #Summarise the model
#   s <- as.data.frame(s[,])
#   
#   #Change the name of the first parameter to in
#   s[!rownames(s) %in% c("(Intercept)"), "Estimate"] <- 
#     s[!rownames(s) %in% c("(Intercept)"), "Estimate"] + 
#     #Add the value of the fixed intercept to all parameters
#     s["(Intercept)", "Estimate"] 
#   
#   #Relabel parameters
#   nms <- rownames(s)
#   nms[1] <- c("Large_1st_Begin")
#   rownames(s) <- nms
#   
#   plyr::ldply(grp, function(y, s, m) {
#     #Capture estimate values
#     k <- s[grepl(y, nms),]
#     
#     #Calculate difference
#     diff <- k[grepl("End", rownames(k)), "Estimate"] - 
#       k[grepl("Begin", rownames(k)), "Estimate"]
#     
#     #Get SE
#     se <- sqrt((k[grepl("End", rownames(k)), "Std. Error"])^2 +
#                  (k[grepl("Begin", rownames(k)), "Std. Error"])^2)
#     
#     #Get t value
#     t_value <- diff / se
#     
#     #Calcualte p
#     p <- ifelse(t_value > 0, round(pt(-t_value, df.residual(m)), 4), 
#                 round(pt(t_value, df.residual(m)), 4))
#     
#     return(data.frame(Group = y, Fit = diff, SE = se, t_value, p))
#     
#   }, s, m)
#   
# }, mdl = Mdls, grp = Groups)
# 
# #Bind estimates into a single data.frame
# Diff <- plyr::rbind.fill(Diff)
# Diff$Island = rep(Islands, each = length(Groups))
# 
# #### Plot After vs Before ####
# percen_trans <- function(){
#   scales::trans_new(name = 'percen', 
#                     transform = function(x) ifelse(x <= 1, x, log(x + 1, base = 10) + 1), 
#                     inverse = function(x) ifelse(x <= 1, x, 10 ^ (x - 1) - 1))
# }
# 
# require(magrittr)
# Graph3 <- Diff %>%
#   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
#   plyr::mutate(Island = plyr::revalue(
#     Island, c('stpauls_rocks' = "St. Paul's Rocks",
#               'noronha' = "Fernando de Noronha", 
#               'rocas' = "Rocas Atoll", 
#               'trindade' = "Trindade Island"))) %>%
#   plyr::mutate(Group = forcats::fct_relevel(Group, "Small_1st", 
#                                             "Small_2nd", "Large_1st", 
#                                             "Large_2nd", "Large_3rd")) %>%
#   plyr::mutate(Group = plyr::revalue(
#     Group, c('Small_1st' = "Small Herbivores",
#              'Small_2nd' = "Small Mesopredators",
#              'Large_1st' = "Large Herbivores",
#              'Large_2nd' = "Large Mesopredators",
#              'Large_3rd' = "Large Top Predators"))) %>%
#   ggplot(aes(color = Group, y = Group, x = 10^(Fit))) + 
#   # #Make a colored backgruond
#   # geom_rect(aes(fill = Group,
#   #               ymin = seq(0, length(Group),
#   #                          length.out = length(Group)),
#   #               ymax = 1 + seq(0, length(Group),
#   #                              length.out = length(Group)),
#   #               xmin = -Inf,
#   #               xmax = Inf), alpha = 0.3) +
# 
#   #0 line
#   geom_vline(xintercept = 1) +
#   #Add points and lines
#   geom_pointrange(aes( y = Group, x = 10^(Fit),
#                        xmin = 10^(Fit - SE),
#                        xmax = 10^(Fit + SE)), 
#                   show.legend = FALSE, alpha = 0.5, size = 0.2) +
#   geom_pointrange(aes(xmin = 10^(Fit - SE),
#                       xmax = 10^(Fit + SE)),
#                   show.legend = FALSE, size = 0.5,
#                   data = Diff %>%
#                     plyr::mutate(Island = forcats::fct_relevel(
#                       Island,Islands)) %>%
#                     plyr::mutate(Island = plyr::revalue(
#                       Island, c('noronha' = "Fernando de Noronha",
#                                 'rocas' = "Rocas Atoll",
#                                 'stpauls_rocks' = "St. Paul's Rocks",
#                                 'trindade' = "Trindade Island"))) %>%
#                     plyr::mutate(Group = forcats::fct_relevel(
#                       Group, "Small_1st", "Small_2nd", "Large_1st",
#                       "Large_2nd", "Large_3rd")) %>%
#                     plyr::mutate(Group = plyr::revalue(
#                       Group, c('Small_1st' = "Small Herbivores",
#                                'Small_2nd' = "Small Mesopredators",
#                                'Large_1st' = "Large Herbivores",
#                                'Large_2nd' = "Large Mesopredators",
#                                'Large_3rd' = "Large Top Predators")
#                     )) %>%
#                     plyr::mutate(Fit = ifelse(p < .05, Fit, NA)) %>%
#                     plyr::mutate(SE = ifelse(p < .05, SE, NA))) +
#   scale_color_discrete(direction = -1) +
#   
#   #Adjust x axis
#   scale_x_continuous(breaks = scales::breaks_pretty()) +
#   # expand_limits(x = c(0,2)) +
#   
#   #Make a White background and keep only x grid
#   theme_bw() +
#   theme(panel.grid.major.x = element_line(size=.1, 
#                                           color = rgb(0.95,
#                                                       0.95,
#                                                       0.95)),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   #Split data into grids of graphs
#   facet_wrap(facets = vars(Island), 
#              scales = 'free_x', ncol = 4) +
#   #Labels
#   labs(title = "Changes in biomass through time",
#        y = "Groups", 
#        x = "Proportional changes in biomass")
# 
# 
# Graph3
# 
# 
# 
# # #Apply permutational linear models with mixed effects within each Island
# # Mdls2 <- lapply(unique(plt_grps2$Island), function(x, d) {
# #   #Within each group 
# #   k <- lapply(as.character(unique(plt_grps2$Groups)), function(y, d) {
# #     #Test differences after vs before
# #    
# #     lm(log(Biomass + 1) ~ Time + scale(Depth) + Sites, 
# #        data = d, subset = d$Island == x & d$Groups == y)
# #     
# #     
# #   }, d)
# #   names(k) <- unique(plt_grps2$Groups)
# #   return(k)
# # }, d = plt_grps2)
# # 
# # names(Mdls2) <- unique(plt_grps2$Island)
# # 
# # lapply(Mdls2$trindade, plot)
# # 
# # lapply(Mdls2, function(x) lapply(x, lme4::isSingular))
# # 
# # # Singular fits for 
# # #noronha: Large_2nd
# # #stpauls_rocks: Large_1st
# # 
# # #Observe parameters in those models
# # #If there are no differences in sites between after and before, 
# # #the time estimate should approach 0
# # coef(Mdls$noronha$Large_2nd)
# # coef(Mdls$stpauls_rocks$Large_1st)
# # # In both cases, there is no differences in site slopes 
# # 
# # 
# # #Summarise results
# # summ <- lapply(Mdls, lapply, summary)
# # 
# # #Get p values
# # lapply(summ, sapply, function(x) {x$coefficients['TimeEnd', 'Pr(>|z|)']})
# # #Get corrected p values
# # lapply(summ, function(y) p.adjust(sapply(y, function(x) x$coefficients['TimeEnd', 'Pr(>|z|)'])))
# # 
# # # Predict values
# # 
# # #Predict mean values for display
# # mn <- vapply(Islands, function(x, grps, mdls, d) {
# #   #Create inputs
# #   prd <- data.frame(dplyr::summarise(dplyr::group_by(d[[x]], Time, Sites), 
# #                                      Depth = mean(Depth)))
# #   #Predict for each group
# #   m <- `names<-`(vapply(grps, function(y) {
# #     #Whish sites approaches the mean the most?
# #     s <- prd[,'Sites'][aggregate(predict(mdls[[x]][[y]], prd), list(prd$Time), 
# #                                  function(z) which(abs(z - mean(z)) == min(abs(z - mean(z)))))[1,'x']]
# #     p <- prd[prd$Sites == s,]
# #     #As depth may change between ocasions, make sure the depth in before == after
# #     p[1,'Depth'] <- p[2,'Depth']
# #     
# #     mn <- merTools::predictInterval(mdls[[x]][[y]], p, stat = 'mean', include.resid.var=0,
# #                                     type = 'linear.prediction', fix.intercept.variance = 0)
# #     
# #     #Scale by the mean at the beginning
# #     as.numeric(exp(mn[2,])/exp(mn[1,1]))
# #   }, numeric(3)), grps)
# #   
# #   return(`colnames<-`(matrix(m, nrow = 3), grps))
# #   
# # }, matrix(0, ncol = 5, nrow = 3), mdls = Mdls, d = split(plt_grps2, plt_grps2$Island),
# # grps = as.character(unique(plt_grps2$Groups)))
# # 
# # warnings()
# # 
# # #Add names to estimates
# # dimnames(mn)[[1]] <- c("Fit", "Upper", "Lower")
# # 
# # #Melt
# # Diff <- reshape2::melt(mn)
# # colnames(Diff) <- c('Parameter', 'Group', "Island", "Estimate")
# # #Reshape to make Estimates separate columns
# # Diff <- reshape2::dcast(Diff, Island + Group ~ Parameter, value.var = 'Estimate')
# # Diff[, c('Fit', "Upper", "Lower")] <- Diff[, c('Fit', "Upper", "Lower")] - 1
# # Diff <- dplyr::arrange(Diff, match(Diff$Island, c('noronha', 'rocas', 'stpauls_rocks', 'trindade')))
# # 
# # Diff <- Diff %>%
# #   plyr::mutate(Changed = ifelse(Upper < 0, TRUE, 
# #                                 ifelse(Lower > 0, TRUE, FALSE)))
# # # plyr::mutate(Changed = ifelse(Upper < 0, TRUE, 
# # #                               ifelse(Lower > 0, TRUE, FALSE)))
# # 
# # #### Plot After vs Before ####
# # percen_trans <- function(){
# #   scales::trans_new(name = 'percen', 
# #                     transform = function(x) ifelse(x <= 1, x, log(x + 1, base = 10) + 1), 
# #                     inverse = function(x) ifelse(x <= 1, x, 10 ^ (x - 1) - 1))
# # }
# # 
# # require(magrittr)
# # Graph3 <- Diff %>%
# #   plyr::mutate(Island = forcats::fct_relevel(Island, Islands)) %>%
# #   plyr::mutate(Island = plyr::revalue(Island, c('noronha' = "Fernando de Noronha", 
# #                                                 'rocas' = "Rocas Atoll", 
# #                                                 'stpauls_rocks' = "St. Paul's Rocks",
# #                                                 'trindade' = "Trindade Island"))) %>%
# #   plyr::mutate(Group = forcats::fct_relevel(Group, "Small_1st", "Small_2nd", "Large_1st", 
# #                                             "Large_2nd", "Large_3rd")) %>%
# #   ggplot(aes(color = Group, y = Group, x = Fit)) + 
# #   #Adjust x axis
# #   scale_x_continuous(trans = 'percen', breaks = c(-1,-0.5,0,0.5,1, 3)) +
# #   expand_limits(x = c(-1,1)) +
# #   
# #   #Split data into grids of graphs
# #   facet_wrap(facets = vars(Island), 
# #                       scales = 'free', ncol = 4) +
# #   #Make a White background and keep only x grid
# #   theme(panel.grid.major.x = element_line(size=.05, color = rgb(0.95,0.95,0.95)),
# #                  panel.grid.major.y = element_blank(),
# #                  panel.grid.minor = element_blank(),
# #                  panel.background = element_blank(),
# #                  panel.border = element_rect(colour = "gray", 
# #                                                       fill=NA, size=.5)) +
# #   #0 line
# #   geom_vline(xintercept = 0) +
# #   #Add points and lines
# #   geom_pointrange(aes(xmin = Lower,
# #                                         xmax = Upper), 
# #                            show.legend = FALSE, alpha = 0.5, size = 0.2) +
# #   geom_pointrange(aes(xmin = Lower,
# #                                         xmax = Upper), 
# #                            show.legend = FALSE, size = 0.5,
# #                            data = Diff %>% 
# #                              plyr::mutate(Island = plyr::revalue(Island, 
# #                                                                  c('noronha' = "Fernando de Noronha", 
# #                                                                    'rocas' = "Rocas Atoll", 
# #                                                                    'stpauls_rocks' = "St. Paul's Rocks",
# #                                                                    'trindade' = "Trindade Island"))) %>%
# #                              plyr::mutate(Group = forcats::fct_relevel(Group, 
# #                                                                        "Small_1st", "Small_2nd", "Large_1st", 
# #                                                                        "Large_2nd", "Large_3rd")) %>%
# #                              plyr::mutate(Fit = ifelse(Changed, Fit, NA)) %>%
# #                              plyr::mutate(Upper = ifelse(Changed, Upper, NA)) %>%
# #                              plyr::mutate(Lower = ifelse(Changed, Lower, NA))) +
# #   scale_color_discrete(direction = -1) +
# #   
# #   
# #   #Labels
# #   labs(title = "Differences in groups biomass",
# #                 y = "Groups", 
# #                 x = "Diff. in biomass (%)")
# # 
# # Graph3
# # 
# # 
# # #Get p values
# # lapply(summ, function(y) p.adjust(sapply(y, function(x) x$coefficients['TimeEnd', 'Pr(>|z|)'])))
# 
