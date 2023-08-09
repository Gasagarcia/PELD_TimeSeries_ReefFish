#### Edit database ####
#Load data
d <- read.csv("Data/Final_data_peld_2019_11_18.csv", 
              header = TRUE, sep = ",", dec = ".")
str(d)

#Separate islands
Islands <- unique(d$locality)

#### Inspect entries ####
data.frame(dplyr::summarise(dplyr::group_by(d[d$locality == "trindade",], code), 
                      abun = sum(abun, na.rm = TRUE), size = max(size_cm)))

  
#### General adjustments ####
{
#Change "," for "."
d$size_cm <- as.numeric(gsub(",", ".", as.character(d$size_cm)))

#Adjust coordinates
d$depth_m <- as.numeric(gsub(",", ".", as.character(d$depth_m)))

d$sites <- as.character(d$sites)
d$locality <- as.character(d$locality)

unique(d$lon)
unique(d$lat)

d$lat <- as.numeric(gsub("488.079", "488615", as.character(d$lat)))
d$lon <- as.numeric(gsub("340.493", "338621", as.character(d$lon)))


#Check coordinates


require(magrittr)
#Noronha
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "noronha") )

#Correct cagarras coordinate in Noronha 
#(cagarras coordinate match stpaul_rocks' enseada)
d[d$locality == "noronha" & d$lat == 0.917081 & 
    d$lon == -29.345528, c("lat", "lon")] <- data.frame(lat = -3.814548,
                                                        lon = -32.390165)
#Check
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "noronha") )






#Rocas
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "rocas") )
#All look ok



#Penedos
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "stpauls_rocks") )
#All look ok



#Trindade
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "trindade") )
#Correct exchanged lat and lon in trindade 
#(tartarugas_trin coordinates match roca's tartarugas)
d[d$locality == "trindade" & d$sites == "tartarugas_trin", 
  c("lat", "lon")] <- data.frame(lat = -20.517113, lon = -29.30096)
#Check
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "trindade") )

#Correct Farol's coordinates (lat and lon were exchanged)
d[d$locality == "trindade" & d$sites == "farol", 
  c("lat", "lon")] <- data.frame(lat = -20.49853, lon = -29.32092)
#Check
as.data.frame(d %>% dplyr::group_by(locality, sites, lat, lon) %>%
                dplyr::summarise() %>% dplyr::filter(locality == "trindade") )



unique(d$lon)
unique(d$lat)

d$code <- as.character(d$code)
d$code[d$code == "lab_kal"] <- "gob_kal"
d$code[d$code == "oph_atl"] <- "oph_tri"
d$code[d$code == "par_fur"] <- "cep_fur"
d$code[d$code == "das_ame" | d$code =="daysatis_americana"] <- "hyp_ame"
d$code[d$code == "men_pun"] <- "cep_hib"
d$code[d$code == "bel_bel"] <- "pla_arg"
d$code[d$code == "kyp_sp"] <- "kyp_spp"
d$code[d$code %in% c("kyp_sec", "kyp_vai", "kyp_cin", 
                     "kyp_inc", "kyp_big")] <- "kyp_spp"

#Remove entries with dubious size
d$size_cm[d$code == "chr_mul" & d$size_cm > 15] <- 15
d$size_cm[d$code == "tha_nor" & d$size_cm > 14] <- 13

#### Noronha ####

#Change spp names
d$code[d$locality=="noronha" & d$code == "cep_pul"] <- "cep_ful"
d$code[d$locality=="noronha" & 
         (d$code == "cor_gla" | d$code == "cor_spb")] <- "cor_sp"
d$code[d$locality=="noronha" & d$code == "ela_fig"] <- "ela_pht"
d$code[d$locality=="noronha" & (
  d$code %in% c("mal_sp1", "mal_sp2", 
                "mal_sp3", "mal_sp",
                "mal_lia", "malacoctenus_lianae"))] <- "mal_lia"


# d$code[d$locality=="noronha" & d$code == "alu_mon"] <- "alu_scr"
d$code[d$locality=="noronha" & d$code == "aul_mac"] <- "aul_str"

# #Select deep sites only
# d<-d[!(d$locality=="noronha" & as.numeric(as.character(d$depth_m)) < 10),]

# Select only the three continuously sampled sites
d <- d[!(d$locality=="noronha" & !(d$sites=="cagarras" |
                                     d$sites=="laje_dois_irmaos" |
                                     d$sites == "ponta_da_sapata")),]
d <- d[!(d$locality=="noronha" & d$year == 2011), ]


#### Rocas ####
d$code[d$locality=="rocas" & d$code == "cha_osc"] <- "cha_oce"
d$code[d$locality=="rocas" & 
         (d$code == "cor_gla" | d$code == "cor_spb")] <- "cor_sp"
d$code[d$locality=="rocas" & d$code == "mal_sp2"] <- "mal_sp"
# d$code[d$locality=="rocas" & d$code == "enn_alt"] <- "gob_kal"
d$code[d$locality=="rocas" & d$code == "str_tim"] <- "pla_arg"

#Remove sites rarely sampled
summarise <- function(x, ...) as.data.frame(dplyr::summarise(
  dplyr::group_by(x, ...)))
summarise(d[d$locality == "rocas",], locality, sites, year)
d <- d[!(d$locality=="rocas" & d$site %in% c("laguna", "piscinas_nove", 
                                             "garoupinha", "poita_do_zeca",
                                             "zulu")), ]

#Remove 2006
# d <- d[!(d$locality=="rocas" & d$year == 2006), ]

#### ASPSP ####
# d$code[d$locality=="stpauls_rocks" & d$code == "chi_ret"] <- "dio_hol"
d$code[d$locality=="stpauls_rocks" & d$code == "mal_sp3"] <- "mal_sp"
d$code[d$locality=="stpauls_rocks" & d$code == "ste_roc"] <- "ste_san"
d$code[d$locality=="stpauls_rocks" & d$code == "cep_pul"] <- "can_pul"
d$code[d$locality=="stpauls_rocks" & d$code == "ant_sal"] <- "alu_scr"

#Remove problematic sites
#Remove cabeco_tartaruga in 2014
d <- d[!(d$locality=="stpauls_rocks" & d$sites %in% c("boia", 'naufragio')),]


#### Trindade ####

#Remove a species with no size data
d <- d[!(d$locality=="trindade" & is.na(d$size_cm)),]

#Correct names
d$code[d$locality=="trindade" & (d$code %in% c("cor_gla", "cor_sp", "cor_spb"))] <- "cor_thr"
d$code[d$locality=="trindade" & d$code == "ent_spp"] <- "ent_vom"
d$code[d$locality=="trindade" & d$code == "hol_asc"] <- "hol_ads"
d$code[d$locality=="trindade" & (d$code == "mal_sp1" | d$code == "mal_sp2")] <- "mal_bru"
d$code[d$locality=="trindade" & d$code == "oph_spp"] <- "oph_tri"
d$code[d$locality=="trindade" & d$code == "syn_sp"] <- "syn_syn"
d$code[d$locality=="trindade" & (d$code == "hal_sp" | d$code == "hal_rad")] <- "hal_bra"
d$code[d$locality=="trindade" & (d$code == "hal_rub" | d$code == "hal_poe")] <- "hal_spp"
d$code[d$locality=="trindade" & (d$code %in% c("spa_roc", "spa_fro"))] <- "spa_roc"


#Remove sites rarely sampled
dplyr::summarise(dplyr::group_by(d[d$year == "2007",], locality, sites, year))
d <- d[!(d$locality=="trindade" & d$site %in% c("martin_vaz_oeste", "shing", 
                                                "crista_do_galo", "parcel", 
                                                'cabritas', 'farilhoes', 
                                                'monumento', 'orelhas', 
                                                'paredao', 'tunel')), ]

#Remove entries with missing data (size)
d <- d[!(d$locality=="trindade" & is.na(d$size_cm)),]

#Remove a census with a single species
d <- d[!(d$locality == "trindade" & d$sites == "cabritas" & d$transect_id == 4839),]
}
# Save the full dataset
saveRDS(d, "R_Objects/edited.rds")

rm(list = ls())
#### Prepare Community Data ####

# Flat to Matrix
#This function reshape the data recorded on visual censuses into the comunity 
#matrix format. It also inform the site, year, depth and observer(s) of a census.
#If suppliedd with a list of a and b constants, the function can also built 
#a community matrix based on biomass (must inform biom = TRUE)
#This function was designed for the PELD ILOC database and requires the collumns
#site, transect_id, year and code (as code for species names). To apply it on 
#other datasets, just change columns names


#Load data
d <- readRDS("R_Objects/edited.rds")

flat_to_matrix <- function(Dt, allm_consts = NULL, 
                           tr_method = "tot", transform = TRUE) {
  
  library(magrittr)
  #Check data
  if (!is.data.frame(Dt)) '<-'(Dt, as.data.frame(Dt))
  
  if (!is.null(allm_consts)) {
    #Are input files data.frames?
    stopifnot(is.data.frame(Dt), is.data.frame(allm_consts))
    
    #Is allm_consts complete with a and b?
    if ((ncol(allm_consts) < 2)) 
      stop("'allm_consts' requires at least 2 columns")
    
    #Are codes somehow provided in allm_consts object?
    
    #Check if any column match the codes
    spp_column <- vapply(allm_consts, function(x) any(unique(Dt$code) %in% x),
                         logical(1))
    #Check if the rownames match the codes
    spp_rows <- unique(Dt$code) %in% rownames(allm_consts)
    names(spp_column) <- NULL; names(spp_rows) <- NULL
    
    column <- rownames <- NULL
    #Are codes for species provided in a column?
    if (any(spp_column)) {
      #If provided as a column, are records complete?
      if (!all(unique(Dt$code) %in% allm_consts[,spp_column]))  
        stop("There are species in the database that are not in 'allm_consts' \n    Check for: ",
             paste0(unique(Dt$code)[!unique(Dt$code) %in% as.character(
               allm_consts[,vapply(allm_consts, function(x) 
                 any(unique(Dt$code) %in% x), logical(1))])], colapse = ", "))
      
      code <- 'column'
      
      #Is it provided as rownames? 
    } else if (any(spp_rows)) {
      #If provided as rownames, are records complete?  
      if (!all(unique(Dt$code) %in% rownames(allm_consts))) 
        stop("There are species in the database that are not in 'allm_consts' \n    Check for: ",
             paste0(unique(Dt$code)[!unique(Dt$code) %in% rownames(allm_consts)], 
                    colapse = ", "))
      
      code <- 'rownames'
    } else stop("Species codes are not provided in allm_consts. Please provide it either by adding row names or a column with labels")
    
    # Transform allm_consts to a single shape containing codes in rownames and a and b in 2 columns
    if (code == 'rownames') {
      allm_consts <- tryCatch(allm_consts[, c('a','b')],  error = function(e) {
        allm_consts[, 1:2]
        warning("No columns named after 'a' or 'b' found. Using the first column as 'a' and the second as 'b'")
      })
      
    } else if (code == 'column') {
      rownames(allm_consts) <- allm_consts[, Position(isTRUE, spp_column)]
      allm_consts <- tryCatch(allm_consts[, c('a','b')],  error = function(e) {
        allm_consts[, 1:2]
        warning("No columns named after 'a' or 'b' found. Using the first column as 'a' and the second as 'b'")
      })
    }
    
    #Calculate biomass
    Dt$bio <- Dt$abun * allm_consts$a[match(Dt$code, rownames(allm_consts))] * 
      (Dt$size_cm ^ allm_consts$b[match(Dt$code, rownames(allm_consts))])
    
    #Sum mass within transects
    bio <- Dt %>%
      dplyr::group_by(sites, transect_id, year, code) %>%
      
      #Summarise data
      dplyr::summarise(bio = sum(bio)) %>%
      
      #Reshape data into a matrix 
      reshape2::dcast(sites + year + transect_id ~ code, 
                      value.var = "bio", fun.aggregate = mean, 
                      fill = 0)
    
    
    
    #Get predictors
    r <- bio[,c("sites", "year", "transect_id")]
    rownames(bio) <- apply(bio[,c("sites", "year", "transect_id")], 1,
                           paste, collapse = "_")
    bio <- bio[ , !colnames(bio) %in% c("sites", "year", "transect_id")]
  }
  
  #Sum abundances within transects
  abun <- Dt %>% dplyr::group_by(sites, year, transect_id, code) %>%
    dplyr::summarise(abun = sum(abun)) %>%
    #Cast to input 0s
    reshape2::dcast(sites + year + transect_id ~ code, 
                    value.var = "abun", fun.aggregate = sum, fill = 0)
  
  #Adding names
  rownames(abun) <- apply(abun[,c("sites", "year", "transect_id")], 1,
                          paste, collapse = "_")
  abun <- abun[ , !colnames(abun) %in% c("sites", "year", "transect_id")]
  
  
  #If biomass is available, return it, else return only abundance
  retrn <- tryCatch(list("abun" = abun, "mass" = bio, "pred" = r),  
                    error = function(e) list("abun" = abun, "pred" = r))
  
  
  #Add extra infos
  if (any(colnames(Dt) == "size_cm")) {
    #Add size as as matrix too
    retrn$size <- dplyr::group_by(Dt, sites, year, transect_id, code) %>%
      dplyr::summarise(size = weighted.mean(size_cm, abun, na.rm = TRUE)) %>%
      reshape2::dcast(sites + year + transect_id ~ code, 
                      value.var = "size", fun.aggregate = mean, fill = 9999)
    retrn$size <- retrn$size[,!colnames(retrn$size) %in% c("sites", "year", "transect_id")]
  }
  
  
  if (any(colnames(Dt) == "depth_m")) {
    
    #Summarise data by averaging depth in transects
    retrn$pred$depth <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id),
                                         depth = mean(depth_m, na.rm = TRUE))$depth
  }
  
  if (any(colnames(Dt) == "observer")) {
    #Usse summarise function to get each census observer
    retrn$pred$obsvr <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id), 
                                         observer = paste(unique(observer), 
                                                          collapse = "_"))$observer
  }
  
  if (any(colnames(Dt) == "lat")) {
    
    #Summarise data by averaging depth in transects
    retrn$pred$lat <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id),
                                       lat = mean(lat, na.rm = TRUE))$lat
  }
  
  if (any(colnames(Dt) == "lon")) {
    
    #Summarise data by averaging depth in transects
    retrn$pred$lon <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id),
                                       lon = mean(lon, na.rm = TRUE))$lon
  }
  attr(retrn, "class") <- "censusftm"
  
  return(retrn)
}


#Split by Island
d <- split(d, d$locality)

#Load a and b constants
traits <- readODS::read_ods("Data/traits_210613.ods")

allm_consts <- as.data.frame(traits[ , c("a", "b")])

rownames(allm_consts) <- traits$code

# Apply flat_to_matrix function
Data <- lapply(d, flat_to_matrix, allm_consts = allm_consts)

#Remove censuses with no fishes
Data <- lapply(Data, function(x) {
  x[['mass']] <- x[['mass']][!apply(x[['abun']], 1, function(y) all(y == 0)),]
  x[['abun']] <- x[['abun']][!apply(x[['abun']], 1, function(y) all(y == 0)),]
  x[['size']] <- x[['size']][!apply(x[['abun']], 1, function(y) all(y == 0)),]
  x
})

#### Subset Species ####
require(magrittr)
#Keep only core species through detection and abundance criteria
Data <- Map(function(x, Dt, i) {
  
  
  
  #Presence criteria
  crt_abs <- data.frame(Dt %>% 
                           dplyr::group_by(code, year) %>% 
                           dplyr::summarise(.groups = "keep")) %>% 
    reshape2::dcast(code ~ year, fun.aggregate = function(x) {1}, fill = 0,
                    value.var = "year")
  
  #Make codes rownames
  rownames(crt_abs) <- crt_abs$code
  crt_abs <- crt_abs[,-1]
  #The species must be present in at least 70% of the series
  #aka. absent in only 30
  crt_abs <- rowSums(crt_abs) == ncol(crt_abs)
  
  
  
  
    #Minimum records criteria
  #How much years recorded a given species in at least 5 transects?
  crt_det <- data.frame(Dt %>% 
                            dplyr::group_by(code, year) %>% 
                            dplyr::summarise(n = rep(1, length(year)),
                                             .groups = "keep")) %>% 
    reshape2::dcast(code ~ year, value.var = "n", fun.aggregate = sum, fill = 0)
  #Make code rownames
  rownames(crt_det) <- crt_det$code
  crt_det <- crt_det[,-1]
  crt_det <- rowSums(crt_det > 5) > ((50/100)*ncol(crt_det))
  
  
  
  #Abundance criteria
  crt_abun <- data.frame(Dt %>% 
               dplyr::group_by(transect_id, year, code) %>% 
               dplyr::summarise(abun = sum(abun), .groups = "keep") %>%
    reshape2::dcast(transect_id + year ~ code, fill = 0, value.var = "abun") %>%
    reshape2::melt(id.vars = c("transect_id", "year"), value.var = "abun") %>%
    dplyr::group_by(year, variable) %>%
    dplyr::summarise(abun = mean(value), .groups = "keep") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(abun = sd(abun)), .groups = "keep")
  crt_abun <- `names<-`(crt_abun$abun, as.character(crt_abun$variable))
  crt_abun <- crt_abun > 0.5
  
  #Print  criterias
  cat(i,"\n")
  print(paste("Total of species:", length(crt_abs)))
  print(paste("Presence:", sum(crt_abs)))
  print(paste("Detectability:", sum(crt_det)))
  print(paste("Abundance:", sum(crt_abun)))
  print(paste("Combined:", sum(crt_abs | crt_abun | crt_det)))
  
  #Select species that met at least 1 criteria
  RM <- crt_abs | crt_abun | crt_det
  x[['mass']] <- x[['mass']][,RM[names(x[["mass"]])]]
  x[['abun']] <- x[['abun']][,RM[names(x[["abun"]])]]
  x[['size']] <- x[['size']][,RM[names(x[["size"]])]]
  x
}, Data, d, names(Data))

#Transforms size 9999 into NAs
Data <- lapply(Data, function(x) {
  x$size[x$size == 9999] <- NA
  x
})

saveRDS(Data, 'R_Objects/Data.rds')

rm(list = ls())




#### Prepare Groups Data ####
d <- readRDS("R_Objects/edited.rds")

d <- split(d, d$locality)

#Define groups
traits <- readODS::read_ods("Data/traits_210613.ods")

allm_consts <- as.data.frame(traits[ , c("a", "b")])

rownames(allm_consts) <- traits$code

groups <- data.frame(groups1 = paste(ifelse(traits$MeanSize < 15, "Small", "Large"), 
                                    ifelse(traits$FoodTroph < 3, "1st", ifelse(traits$FoodTroph >= 4, "3rd",
                                                                               "2nd")), sep = "_"),
                     code = traits$code)
# groups[groups$code == 'lut_joc', 'groups'] <- 'Large_3rd'
groups[groups$groups1 == 'Small_3rd', 'groups1'] <- 'Large_3rd'


#Load groups from quimbayo et al for comparison
atl_trt_dt <- read.csv2("Data/Fish_aspects_EasternPacific_Atlantic_Realms.csv")
groups$groups2 <- atl_trt_dt$Diet[match(groups$code, 
                                        paste(substr(atl_trt_dt$Genus, 1, 3),
                                              substr(atl_trt_dt$Species, 1, 3),
                                              sep = "_"))]

#Fill in blanks
groups <- groups %>% 
  dplyr::mutate(groups2 = ifelse(is.na(groups2), 
                                 dplyr::case_when(code %in% "car_per" ~ "fc",
                                                  code %in% "neg_bre" ~ "fc",
                                                  code %in% "cep_fur" ~ "pk",
                                                  code %in% "cep_hib" ~ "fc",
                                                  code %in% "can_und" ~ "fc",
                                                  code %in% "eng_anc" ~ "pk",
                                                  code %in% "kyp_inc" ~ "hm",
                                                  code %in% "apo_sp" ~ "pk",
                                                  code %in% "cor_sp" ~ "om",
                                                  code %in% "lac_trg" ~ "is",
                                                  code %in% "hyp_ame" ~ "im",
                                                  code %in% "mal_lia" ~ "im",
                                                  code %in% "mal_sp" ~ "im",
                                                  code %in% "ant_sal" ~ "pk",
                                                  code %in% "spa_spp" ~ "hm",
                                                  code %in% "hal_spp" ~ "im",
                                                  code %in% "nar_bra" ~ "im",
                                                  code %in% "kyp_spp" ~ "hm",
                                                  code %in% "aet_nar" ~ "im",
                                                  code %in% "gin_cir" ~ "im",
                                                  code %in% "spa_ssp" ~ "hm"), 
                                 groups2))
  
  
all(complete.cases(groups))


# groups$code[which(is.na(complete.cases(groups)))]


# This function sum the biomass of all fishes included within groups and average
#them, providing the mean biomass of the groups in each site.
#Allometric constants should be provided as a dataframe with rows named after species.
#The first column are take as the 'a' constant, whereas the second is taken as 'b'
#'groups' object should consist of a dataframe containing 2 columns. The first is the 
#code atributed to each species and the second should contain the groups

grp_bio_matrix <- function(Dt, allm_consts, groups, 
                           tr_method = "tot", transform = TRUE) {
  
  #Chack data
  if (!is.data.frame(Dt)) '<-'(Dt, as.data.frame(Dt))
  
  #Is allm_consts complete with a and b?
  if ((ncol(allm_consts) < 2)) 
    stop("'allm_consts' requires at least 2 columns")
  
  #Are codes somehow provided in allm_consts object?
  
  #Check if any column match the codes
  spp_column <- vapply(allm_consts, 
                       function(x) any(unique(Dt$code) %in% x), logical(1))
  #Check if the rownames match the codes
  spp_rows <- unique(Dt$code) %in% rownames(allm_consts)
  
  names(spp_column) <- NULL; names(spp_rows) <- NULL
  
  column <- rownames <- NULL
  
  #Are codes for species provided in a column?
  if (any(spp_column)) {
    #If so, are records complete?
    if (!all(unique(Dt$code) %in% allm_consts[,spp_column]))  
      stop("There are species in the database that are not in 'allm_consts' \n
           Check for: ",
           paste0(unique(Dt$code)[!unique(Dt$code) %in% as.character(
             allm_consts[,vapply(allm_consts, function(x) 
               any(unique(Dt$code) %in% x), logical(1))])], colapse = ", "))
    
    code <- 'column'
    
    #Are codes for species provided as rownames? 
  } else if (any(spp_rows)) {
    #If so, are records complete?  
    if (!all(unique(Dt$code) %in% rownames(allm_consts))) 
      stop("There are species in the database that are not in 'allm_consts' 
           \n    Check for: ",
           paste0(unique(Dt$code)[!unique(Dt$code) %in% rownames(allm_consts)], colapse = ", "))
    
    code <- 'rownames'
  } else {
    stop("Species codes are not provided in allm_consts. 
              Please provide it either by adding row names or a column with labels")}
  
  # Transform allm_consts to a single data.frame containing codes in rownames
  #  and a and b in 2 columns
  if (code == 'rownames') {
    allm_consts <- tryCatch(allm_consts[, c('a','b')],  error = function(e) {
      allm_consts[, 1:2]
      warning("No columns named after 'a' or 'b' found. Using the first column as 'a' and the second as 'b'")
    })
    
  } else if (code == 'column') {
    rownames(allm_consts) <- allm_consts[, Position(isTRUE, spp_column)]
    allm_consts <- tryCatch(allm_consts[, c('a','b')],  error = function(e) {
      allm_consts[, 1:2]
      warning("No columns named after 'a' or 'b' found. Using the first column as 'a' and the second as 'b'")
    })
  }
  
  
  # Filter for group data
  #Are codes somehow provided in group object
  compare <- function(v) all(sapply(as.list(v[-1]), FUN=function(z) {identical(z, v[1])}))
  
  spp_column <- vapply(groups, function(x) any(unique(Dt$code) %in% x), logical(1))
  spp_names <- unique(Dt$code) %in% names(groups)
  names(spp_column) <- NULL; names(spp_names) <- NULL
  
  if (any(spp_column) || any(spp_names)) {
    # True
    #If provided as rownames, are they complete?
    if (is.null(dim(groups))) if (!compare(spp_names)) 
      stop("There are species in the database that are not in 'groups' \n    Check for: ",
           paste0(unique(Dt$code)[!spp_names], colapse = ", "))
    
    #If provided as a column, are they complete?
    if (length(dim(groups)) == 2) if (!compare(unique(Dt$code) %in% groups[,spp_column]))  
      stop("There are species in the database that are not in 'groups' \n    Check for: ",
           paste0(unique(Dt$code)[!unique(Dt$code) %in% as.character(groups[,vapply(groups, function(x) 
             any(unique(Dt$code) %in% x), logical(1))])], colapse = ", "))
  }
  
  
  #Does groups dataframe provide the codes of fishes? (consider names or a dataframe)
  if (is.null(ncol(groups))) { 
    #If the objects contains a single column, test to see if codes are provided as names
    if (any(unique(Dt$code) %in% names(groups))) { #If rownames contains the codes
      warning("Fish codes are taken from groups names")
      groups <- data.frame(code = names(groups), groups = groups) 
    } else{
      warning("fish codes were not provided neither by name or by a separate column. 
              Considering them in the same order  as in allm_consts")
      groups <- data.frame(code = rownames(allm_consts), groups = groups) 
    }
  } else {
    
    #If the data contain 2 columns, select the one that matches species codes
    if (ncol(groups) == 2) {
      groups <- data.frame(code = as.character(groups[,vapply(groups, function(x) 
        any(unique(Dt$code) %in% x), logical(1))]), 
        groups = as.character(groups[,!vapply(groups, function(x) 
          any(unique(Dt$code) %in% x), logical(1))]))
    } else {
      #If the data contains more than 2 data take the first 2 columns
      warning("The dataframe contain more than 2 columns. Using the fisrt as 'code' and the second as 'groups'")
      groups <- data.frame(code = groups[,1],
                           groups = groups[,2])
    }
  }
  
  
  Dt$bio <- Dt$abun * allm_consts$a[match(Dt$code, rownames(allm_consts))] * 
    (Dt$size_cm ^ allm_consts$b[match(Dt$code, rownames(allm_consts))])
  
  #Estimate biomass
  bio <- Dt %>%
    dplyr::group_by(sites, transect_id, year, code) %>%
    
    #Summarise data
    dplyr::summarise(bio = sum(bio)) %>%
    
    #Reshape data into a matrix 
    reshape2::dcast(sites + year + transect_id ~ code, 
                    value.var = "bio", fun.aggregate = mean, 
                    fill = 0)
  
  
  
  #Get predictors
  r <- bio[,c("sites", "year", "transect_id")]
  rownames(bio) <- apply(bio[,c("sites", "year", "transect_id")], 1,
                         paste, collapse = "_")
  bio <- bio[ , !colnames(bio) %in% c("sites", "year", "transect_id")]
  
  
  #Create a return object
  retrn <- list("biomass" = bio, "pred" = r)
  
  #Add extra infos
  if (any(colnames(Dt) == "size_cm")) {
    #Add size as as matrix too
    retrn$size <- dplyr::group_by(Dt, sites, year, transect_id, code) %>%
      dplyr::summarise(size = weighted.mean(size_cm, abun, na.rm = TRUE)) %>%
      reshape2::dcast(sites + year + transect_id ~ code, 
                      value.var = "size", fun.aggregate = mean, fill = 9999)
    retrn$size <- retrn$size[,!colnames(retrn$size) %in% c("sites", "year", "transect_id")]
  }
  
  
  if (any(colnames(Dt) == "depth_m")) {
    
    #Summarise data by averaging depth in transects
    retrn$pred$depth <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id),
                                         depth = mean(depth_m, na.rm = TRUE))$depth
  }
  
  if (any(colnames(Dt) == "observer")) {
    #Usse summarise function to get each census observer
    retrn$pred$obsvr <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id), 
                                         observer = paste(unique(observer), 
                                                          collapse = "_"))$observer
  }
  
  if (any(colnames(Dt) == "lat")) {
    
    #Summarise data by averaging depth in transects
    retrn$pred$lat <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id),
                                       lat = mean(lat, na.rm = TRUE))$lat
  }
  
  if (any(colnames(Dt) == "lon")) {
    
    #Summarise data by averaging depth in transects
    retrn$pred$lon <- dplyr::summarise(dplyr::group_by(Dt, sites, year, transect_id),
                                       lon = mean(lon, na.rm = TRUE))$lon
  }
  attr(retrn, "class") <- "censusftm"
  return(retrn)
}


rownames(groups) <- groups$code

#Perform on trophic data and size
Grp_Data1 <- lapply(d, grp_bio_matrix, allm_consts = allm_consts, 
                    groups = groups[, c("code", "groups1")], transform = FALSE)
#Perform on classical groups
Grp_Data2 <- lapply(d, grp_bio_matrix, allm_consts = allm_consts, 
                    groups = groups[, c("code", "groups2")], transform = FALSE)

#Replace 9999 with NAs
Grp_Data1 <- lapply(Grp_Data1, function(x) {
  x$size[x$size == 9999] <- NA
  x
})

Grp_Data2 <- lapply(Grp_Data2, function(x) {
  x$size[x$size == 9999] <- NA
  x
})


saveRDS(Grp_Data1, 'R_Objects/Grp_Data1.rds')
saveRDS(Grp_Data2, 'R_Objects/Grp_Data2.rds')

rm(list = ls())
