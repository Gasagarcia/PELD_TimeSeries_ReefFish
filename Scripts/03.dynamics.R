#### Load data ####
Islands <- c("stpauls_rocks", "noronha", "rocas", "trindade")
#Load data
Data <-readRDS("R_Objects/Dt_boot.rds")

#Average censuses within sites
Data_sts <- Map(function(i, x) {
  force(x)
  
  l <- list(paste(x[["pred"]][['sites']], x[["pred"]][['year']],
                  x[["pred"]][['boot']], sep = "_"))

  x[['abun']] <- aggregate(x[['abun']], l, mean)[ , -1]
  x[['mass']] <- aggregate(x[['mass']], l, mean)[ , -1]
  x[['size']] <- aggregate(x[['size']], l, mean)[ , -1]
  x[["pred"]] <- aggregate(x[["pred"]], l,
                           FUN = function(y) {
                             if (is.numeric(y)) {
                               mean(y, na.rm = TRUE)
                               } else {
                               paste(unique(y), collapse = '_&_')
                                 }})[ , -1]

  x
}, names(Data), Data)


#### Community dynamic metrics ####
# Reshape data to a dt.fm with time, species, abundance and replicates as columns
mdata <-lapply(Data_sts, #On each island
         function(x) { 
           d <- data.frame(x$mass, year = as.numeric(x$pred$year), 
                           #Combine sites and depth
                      sites = paste(x$pred$sites, x$pred$boot, sep = "-")) 
           # Remove sites with a single record
           sing <- vapply(by(d, list(d$sites), nrow), 
                          function(y) y > 1, logical(1))
           
           # Subset by removing unique sites
           d <- d[d$sites %in% names(sing)[sing], ]
           #Melt the dt.fm according to year and transect id
           reshape2::melt(d, id.vars = c("year", "sites"),
              variable.name = "spp", value.name = "mass")
           #Store names as "spp" for species and "mass" for biomass
           })

rm(list = ls()[!ls() %in% c("mdata", "Islands")])



#Calculate ecological metrics for the whole data
even <- function(x, formula, Spp, ...) {
  # Reshape into a matrix
  Dt <- reshape2::dcast(x, formula = formula, ...)
  
  #Save entries to return at the end
  return <- Dt[!colnames(Dt) %in% unique(x[, Spp])]
  
  #Transform flat data into a matrix
  Dt <- tidyr::unite(Dt, col = rows, 
                     colnames(Dt)[!colnames(Dt) %in% unique(x[, Spp])],
                     sep = ".")
  #Make predictors rownames
  Dt <- `rownames<-`(Dt[,-1], Dt[,1])
  
  #Calculate shannon diversity and richness
  return$H <- vegan::diversity(Dt)
  return$S <- vegan::specnumber(Dt)
  
  #Calculate Pielou's evenness (J)
  return$J <- return$H/log(return$S)
  return
  
}
head(mdata$stpauls_rocks)


#Calculate diversity metrics for combinations of years and sites
Div_ys <- plyr::ldply(mdata, even, formula = year + sites ~ spp, 
            value.var = "mass", Spp = "spp", .id = "island")


#Calculate metrics for sites only
Div_s <- plyr::ldply(mdata, even, formula = sites ~ spp, 
                      fun.aggregate = mean, fill = 0,
                      value.var = "mass", Spp = "spp", .id = "island")



require(ggplot2); library(magrittr)

##### Temporal turnover ####
#Apply on each island separately
tn_dt <- dplyr::left_join(Div_ys,
  plyr::ldply(mdata, codyn::turnover, metric = "total",
                     time.var = "year", species.var = "spp",
                     abundance.var = "mass", replicate.var = "sites", 
                     .id = "island")) %>%
  
  #Add disappearance turnover
  dplyr::left_join(plyr::ldply(mdata, codyn::turnover, metric = "disappearance",
                               time.var = "year", species.var = "spp",
                               abundance.var = "mass", replicate.var = "sites", 
                               .id = "island")) %>%
  
  #Add appearance turnover
  dplyr::left_join(plyr::ldply(mdata, codyn::turnover, metric = "appearance",
                               time.var = "year", species.var = "spp",
                               abundance.var = "mass", replicate.var = "sites", 
                               .id = "island"))

#Split sites into sites and depth condition
tn_dt <- tidyr::separate(tn_dt, sites, c("sites", "c_depth"), sep = "-")

require(ggplot2); library(magrittr)

#Plot turnover time series
Graph6 <- tn_dt[complete.cases(tn_dt),] %>%
  reshape2::melt(id.vars = c("island", "year", "sites", "c_depth"), 
                     measure.vars = c("total", "disappearance", "appearance"), 
                 value.name = "Scores", variable.name = "Turnover") %>%
  plyr::mutate(Island = forcats::fct_relevel(island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha", 
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  plyr::mutate(Turnover = plyr::revalue(Turnover, 
                                      c('total' = "Total", 
                                        'appearance' = "Appearance", 
                                        'disappearance' = "Disappearance"))) %>%
  
  ggplot(aes(x = as.factor(year - 2000), y = Scores, #color = Turnover,
             group = interaction(Island, Turnover))) +
  # geom_smooth(se = FALSE, ) +
  # geom_point(position = position_jitterdodge(jitter.width = .2,
  #                                            dodge.width = 0.3)) +
  #Add means and their sds
  # stat_summary(aes(fill = Turnover),fun.data = "mean_cl_boot", geom = "ribbon", 
  #              alpha = .1) +
  stat_summary(aes(color = Turnover), fun = mean, geom = "line") +
  stat_summary(aes(color = Turnover), fun = mean, geom = "point") +
  # stat_summary(aes( color = c_depth), geom = "line", 
  #              alpha = .4) +
  #              
  scale_color_viridis_d(name = "Turnover", option = "D") +
  scale_fill_viridis_d(name = "Turnover", option = "D") +
  #Relabel axes
  labs(x = "Time series", y = "Species turnover") +
  facet_wrap(vars(Island), #rows = vars(Turnover),
             scales = "free_x", nrow = 1) + theme_bw() +
  theme(panel.grid.major.y =  element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # legend.background = element_blank(),
        legend.position = c(.13, .78)); Graph6






# Test effect of temporal metrics on ecological descriptors
head(tn_dt[complete.cases(tn_dt),])



#Total turnover
anova(lm(total ~ H * island, data = tn_dt[complete.cases(tn_dt),]))
#H** I*** HI 
anova(lm(total ~ S * island, data = tn_dt[complete.cases(tn_dt),]))
#S*** I*** HI*
anova(lm(total ~ J * island, data = tn_dt[complete.cases(tn_dt),]))
#J I*** HI

#Appearance turnover
anova(lm(appearance ~ H * island, data = tn_dt[complete.cases(tn_dt),]))
#H** I*** HI
anova(lm(appearance ~ S * island, data = tn_dt[complete.cases(tn_dt),]))
#S** I*** SI
anova(lm(appearance ~ J * island, data = tn_dt[complete.cases(tn_dt),]))
#J I*** JI

#Disappearance turnover
anova(lm(disappearance ~ H * island, data = tn_dt[complete.cases(tn_dt),]))
#H*** I*** HI***
anova(lm(appearance ~ S * island, data = tn_dt[complete.cases(tn_dt),]))
#S*** I*** SI
anova(lm(appearance ~ J * island, data = tn_dt[complete.cases(tn_dt),]))
#J I*** JI



#Plot turnover as a function of richness

Graph10 <- tn_dt[complete.cases(tn_dt),] %>%
  reshape2::melt(id.vars = c("island", "year", "sites", "c_depth",
                             "H", "S", "J"), 
                 measure.vars = c("total", "disappearance", "appearance"), 
                 value.name = "Scores", variable.name = "Turnover") %>%
  reshape2::melt(id.vars = c("island", "year", "sites", 
                             "c_depth", "Scores", "Turnover"),
                 measure.vars = c("H", "S", "J"),
                 value.name = "Values", variable.name = "Predictor") %>%
  plyr::mutate(Predictor = plyr::revalue(
    Predictor, 
    c('H' = "Shannon Diversity", 
      'S' = "Species Richness", 
      'J' = "Pielou Evenness"))) %>%
  plyr::mutate(Island = forcats::fct_relevel(island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(
    Island, 
    c('noronha' = "Fernando de Noronha", 
      'rocas' = "Rocas Atoll", 
      'stpauls_rocks' = "St. Paul's Rocks",
      'trindade' = "Trindade Island"))) %>%
  plyr::mutate(Turnover = plyr::revalue(
    Turnover, 
    c('total' = "Total", 
      'appearance' = "Appearance", 
      'disappearance' = "Disappearance"))) %>% 
  
  ggplot(aes(x = Values, y = Scores, col = Island, 
             fill = Island, shape = Island)) +
  geom_point(size = 3, alpha = .4) + theme_bw() +
  scale_color_discrete(name = NULL, direction = -1) +
  scale_shape_discrete(name = NULL) +
  scale_fill_discrete(name = NULL, direction = -1) +
  scale_y_continuous(position = "right") +
  geom_smooth(method = "lm", formula = y ~ x, alpha = .2) +
  facet_grid(Turnover ~ Predictor, scales = "free", switch = "y") +
  # labs(x = "Preditores", y = "Turnover") +
  theme(legend.position = "top"); Graph10


##### Community stability and synchrony ####
#Apply on each island separately
#Stability
dyn_dt <- dplyr::left_join(Div_s, plyr::ldply(mdata, codyn::community_stability, 
                                    time.var = "year", abundance.var = "mass", 
                                    replicate.var = "sites", .id = "island")) %>%
  #Gross synchrony
  dplyr::left_join(plyr::ldply(mdata, codyn::synchrony, 
                               time.var = "year", species.var = "spp",
                               abundance.var = "mass", replicate.var = "sites",
                               metric = "Gross", .id = "island")) %>%
  dplyr::mutate(Gross = synchrony) %>%
  dplyr::mutate(synchrony = NULL) %>%
  #Loreau synchrony
  dplyr::left_join(plyr::ldply(mdata, codyn::synchrony, 
                               time.var = "year", species.var = "spp",
                               abundance.var = "mass", replicate.var = "sites",
                               metric = "Loreau", .id = "island")) %>%
  dplyr::mutate(Loreau = synchrony) %>%
  dplyr::mutate(synchrony = NULL)

dyn_dt <- tidyr::separate(dyn_dt, sites, c("sites", "c_depth"), sep = "-")


dyn_dt <- tn_dt[complete.cases(tn_dt), ] %>%
  # dplyr::select(island, year, sites, c_depth, total) %>%
  dplyr::group_by(island, sites, c_depth) %>%
  dplyr::summarise(Turnover = mean(total, na.rm = TRUE)) %>%
  dplyr::full_join(dyn_dt)






# Test relation between metrics
anova(lm(Turnover ~ (log(S) + H + J) * island, data = dyn_dt)) # H # I ***
anova(lm(log(stability) ~ (log(S) + H + J) * island, data = dyn_dt)) # H # I ***
anova(lm(Loreau ~ (log(S) + H + J) * island, data = dyn_dt)) # H ** # I ***

anova(lm(Turnover ~ H * island, data = dyn_dt)) # H # I ***
anova(lm(Turnover ~ log(S) * island, data = dyn_dt)) # S # I ***
anova(lm(Turnover ~ J * island, data = dyn_dt)) # J. # I ***

anova(lm(log(stability) ~ H * island, data = dyn_dt)) # H # I ***
anova(lm(log(stability) ~ log(S) * island, data = dyn_dt)) # S *** # I ***
anova(lm(log(stability) ~ J * island, data = dyn_dt)) # J * # I ***

anova(lm(Loreau ~ H * island, data = dyn_dt)) # H ** # I ***
anova(lm(Loreau ~ log(S) * island, data = dyn_dt)) # S # I ***
anova(lm(Loreau ~ J * island, data = dyn_dt)) # J *** # I **












# Plot site-based metrics

#Mean turnover
Graph7 <- tn_dt[complete.cases(tn_dt), ] %>%
  # dplyr::select(island, year, sites, c_depth, total) %>%
  dplyr::group_by(island, sites, c_depth) %>%
  dplyr::summarise(Turnover = mean(total, na.rm = TRUE)) %>%
  dplyr::full_join(dyn_dt) %>%
  reshape2::melt(id.vars = c("island", "sites", "c_depth"), 
                 measure.vars = c("stability", "Turnover", "Loreau", "Gross"), 
                 value.name = "Scores", variable.name = "Metric") %>%
  plyr::mutate(Metric = plyr::revalue(Metric,
                                      c(stability = "Stability",
                                        Turnover = "Average turnover",
                                        Gross = "Gross Synchrony",
                                        Loreau = "Loreau Synchrony"))) %>% 
  plyr::mutate(Island = forcats::fct_relevel(island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "FN", 
                                        'rocas' = "RA", 
                                        'stpauls_rocks' ="SP",
                                        'trindade' = "TI"))) %>%
  
  ggplot(aes(y = Island, x = Scores, shape = Island,
             color = Island, fill = Island)) +
  # geom_smooth(se = FALSE, ) +
  geom_point(position = position_jitter(height = .1, width = 0), 
             alpha = .3, size = 1) +
  scale_color_discrete(name = NULL, direction = -1) +
  scale_shape_discrete(name = NULL) +
  scale_fill_discrete(name = NULL, direction = -1) +
  #Add means and their sds
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", 
               size = .5) +
  # coord_flip() +
  #Relabel axes
  labs(x = NULL, y = NULL) +
  facet_wrap(~Metric, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.position = 'none'); Graph7






#Test plot
Graph8 <- dyn_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha", 
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  # dplyr::filter(synchrony > -0.2) %>%
  ggplot(aes(x = Loreau, y = stability)) +
  geom_point(aes(color = Island, fill = Island, shape = Island),
             size = 1, alpha = .5) + 
  scale_color_discrete(name = NULL, direction = -1) +
  scale_shape_discrete(name = NULL) +
  scale_fill_discrete(name = NULL, direction = -1) +
  scale_y_log10() + scale_x_log10() +
  # geom_smooth(aes(color = Island, fill = Island),
  #             method = "lm", alpha = .1,  se = FALSE, formula = y ~ x) + 
  geom_smooth(method = "lm", alpha = .1, formula = y ~ x,
              color = "darkgrey") + 
  xlab("Synchrony\n(Loreau)") + ylab("Stability") +
  theme_bw() + theme(panel.grid = element_blank(), 
                     panel.border = element_blank(),
                     axis.line = element_line(color = rgb(.2,.2,.2,.9)),
                     legend.position = "none", 
                     legend.background = element_blank(),
                     legend.spacing = grid::unit(0.5, units = "inches")); Graph8




Graph9 <- 
  
  tn_dt[complete.cases(tn_dt), ] %>%
  # dplyr::select(island, year, sites, c_depth, total) %>%
  dplyr::group_by(island, sites, c_depth) %>%
  dplyr::summarise(Turnover = mean(total, na.rm = TRUE)) %>%
  dplyr::full_join(dyn_dt, by = c("island", "sites", "c_depth", "Turnover")) %>%
  reshape2::melt(id.vars = c("island", "sites", "c_depth", 
                             "stability", "Turnover", "Loreau"), 
                 measure.vars = c("S", "H", "J"), 
                 value.name = "Values", variable.name = "Predictor") %>%
  dplyr::mutate(stability = log(stability)) %>%
  reshape2::melt(id.vars = c("island", "sites", "c_depth", "Values", "Predictor"), 
                 measure.vars = c("stability", "Turnover", "Loreau"), 
                 value.name = "Scores", variable.name = "Metric") %>%
  plyr::mutate(Island = forcats::fct_relevel(island, Islands)) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha", 
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  plyr::mutate(Predictor = plyr::revalue(Predictor, 
                                      c('S' = "Species Richness", 
                                        'H' = "Shannon Diversity",
                                        'J' = "Pielou Evenness"))) %>%
  plyr::mutate(Metric = plyr::revalue(Metric, 
                                      c('stability' = "log(Stability)", 
                                        'Turnover' = "Average Turnover",
                                        'Loreau' = "Synchrony (Loreau)"))) %>%
  # dplyr::filter(synchrony > -0.2) %>%
  ggplot(aes(x = Values, y = Scores)) +
  geom_point(aes(color = Island, fill = Island, shape = Island),
             size = 1, alpha = .5) + 
  scale_color_discrete(name = NULL, direction = -1) +
  scale_shape_discrete(name = NULL) +
  scale_fill_discrete(name = NULL, direction = -1) +
  scale_y_continuous(position = "right") +
  facet_grid(Metric ~ Predictor, 
             scales = 'free', switch = "y") +
  geom_smooth(method = "lm", alpha = .1, formula = y ~ x,
              color = "darkgrey") + 
  xlab(NULL) + ylab(NULL) +
  theme_bw() + theme(strip.background = element_blank(),
                     strip.text = element_text(size = 11),
                     axis.text = element_text(size = 10),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(colour = rgb(.1,.1,.1,.05)),
                     legend.position = "none",
                     legend.background = element_blank()); Graph9

Graph789 <- Graph7 + Graph8 + Graph9 + 
  patchwork::plot_layout(guides = 'keep',
                         design = "AACCC\nBBCCC"); Graph789

# ggpubr::ggarrange(Graph78, Graph6, ncol = 1, heights = c(11, 10))

# "DDD\nDDD\nABC"


cs_dt %>%
  plyr::mutate(Island = forcats::fct_relevel(island, (Islands))) %>%
  plyr::mutate(Island = plyr::revalue(Island, 
                                      c('noronha' = "Fernando de Noronha", 
                                        'rocas' = "Rocas Atoll", 
                                        'stpauls_rocks' = "St. Paul's Rocks",
                                        'trindade' = "Trindade Island"))) %>%
  dplyr::filter(ifelse(island == "trindade", synchrony > -0.2, TRUE)) %>%
  ggplot(aes(x = log(synchrony + 1), y = log(stability), 
             color = island, fill = island)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .2) +
  
              # data = cs_dt %>%  
              #   dplyr::filter(ifelse(island == "trindade", 
              #                        synchrony > -0.2, TRUE)) %>%
              #   plyr::mutate(Island = forcats::fct_relevel(island, Islands)) %>%
              #   plyr::mutate(Island = plyr::revalue(
              #     Island, c('noronha' = "Fernando de Noronha", 
              #               'rocas' = "Rocas Atoll", 
              #               'stpauls_rocks' = "St. Paul's Rocks",
              #               'trindade' = "Trindade Island"))) %>%
              #   signf(variable = "log(synchrony + 1)", names = c("Island"),
              #         form = as.formula(log(stability) ~ log(synchrony + 1)))) +
 
  facet_wrap(vars(Island), scales = 'free', nrow = 1) +
  theme_bw() + theme(panel.grid = element_blank())

#Build a model
mod1 <- lm(stability ~ synchrony * island,
                 data = cs_dt)  #%>%  
             # dplyr::filter(ifelse(island == "trindade",
             #                      synchrony > -0.2, TRUE)))
summary(mod1)
anova(mod1)

#Check adjustment
par(mfrow = c(2,2)); plot(mod1) #Seems Ok
