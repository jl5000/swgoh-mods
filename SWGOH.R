library(tidyverse)

# Hardcoded data #################################################################
slots <- c("Square","Diamond","Circle",
           "Arrow","Triangle","Cross")
stat_names <- c("Speed", 
                "Speed %", 
                "Potency %",
                "Tenacity %",
                "Offense",
                "Offense %",
                "Protection",
                "Protection %",
                "Critical Chance %",
                "Critical Damage %",
                "Defense",
                "Defense %",
                "Health",
                "Health %")

# Generate theoretical maximum stat enhancements to normalise enhancements later
max_prim <- c(30,0,24,24,0,5.88,0,23.5,12,36,0,11.75,0,5.88)
max_nprim <- c(1,0,1,1,0,4,0,4,1,1,0,4,0,4)
max_sec <- c(27,0,9.63,10.19,201,2.44,3630,9.74,10.14,0,41,7.27,1916,5.01)
max_nsec <- c(5,0,6,6,6,6,6,6,6,0,6,6,6,6)
max_set_bonus <- c(0,10,30,30,0,10,0,0,15,30,0,15,0,15)
max_stats <- data.frame(Max.Primary = max_prim * max_nprim,
                        Max.Secondary = max_sec * max_nsec,
                        Max.Set.Bonus = max_set_bonus,
                        row.names = stat_names,
                        stringsAsFactors = FALSE)
rm(max_prim, max_nprim, max_sec, max_nsec, max_set_bonus)

# Generate set bonus enhancement rules
set_bonus_n <- c(0,4,2,2,0,4,0,0,2,4,0,2,0,2)
set_bonus_lower <- c(0,5,5,5,0,5,0,0,2.5,15,0,2.5,0,2.5)
set_bonus_upper <- c(0,10,10,10,0,10,0,0,5,30,0,5,0,5)
set_bonus_rules <- data.frame(Number = set_bonus_n,
                              Bonus = set_bonus_lower,
                              Max.Bonus = set_bonus_upper,
                              row.names = stat_names,
                              stringsAsFactors = FALSE)
rm(set_bonus_n, set_bonus_lower, set_bonus_upper)

###################################################################################
# SWGOH.gg data - Parse data later
library(stringr)
swgoh.gg.name <- "Jamie"
swgoh.gg.url <- paste0("https://swgoh.gg/u/",swgoh.gg.name,"/collection/")
swgoh.toon.page <- readLines(swgoh.gg.url)
#href="/u/jamie/collection/bodhi-rook/" rel="nofollow">Bodhi Rook</a></div>
match <- "jamie.+rel=\"nofollow\">(.+)</a>"
toon_list <- str_match(swgoh.toon.page, match) %>% subset(select=2) 
toon_list <- subset(toon_list, subset=!is.na(toon_list)) 
toon_list <- str_replace_all(toon_list, "&quot;", "'")


################################
library(jsonlite)
mods_json <- fromJSON("data/swgoh-mods-sample.json")

mods_info <- data.frame(Mod.ID = mods_json$all_mods$mod_uid,
                        Initial.Toon = mods_json$all_mods$characterName,
                        Shape = mods_json$all_mods$slot,
                        Set = mods_json$all_mods$set,
                        Level = mods_json$all_mods$level,
                        Primary = mods_json$all_mods$primaryBonusType,
                        Primary.Value = mods_json$all_mods$primaryBonusValue,
                        Secondary1 = mods_json$all_mods$secondaryType_1,
                        Secondary1.Value = mods_json$all_mods$secondaryValue_1,
                        Secondary2 = mods_json$all_mods$secondaryType_2,
                        Secondary2.Value = mods_json$all_mods$secondaryValue_2,
                        Secondary3 = mods_json$all_mods$secondaryType_3,
                        Secondary3.Value = mods_json$all_mods$secondaryValue_3,
                        Secondary4 = mods_json$all_mods$secondaryType_4,
                        Secondary4.Value = mods_json$all_mods$secondaryValue_4,
                        Pips = mods_json$all_mods$pips,
                        stringsAsFactors = FALSE)

rm(mods_json)
mods_info <- mods_info %>% filter(Pips == "5", 
                                  Level == "15", 
                                  (Primary == "Speed"
                                   |Secondary1=="Speed"
                                   |Secondary2=="Speed"
                                   |Secondary3=="Speed"
                                   |Secondary4=="Speed")) 

mod_list <- mods_info$Mod.ID
toon_list <- unique(mods_info$Initial.Toon) %>% sort() 
toon_list <- toon_list[toon_list != "unassigned"]

# CLEAN UP ####################################################################
toon_list <- stringr::str_replace_all(toon_list, "\"", "'")
mods_info$Initial.Toon <- stringr::str_replace_all(mods_info$Initial.Toon, "\"", "'")
mods_info$Shape <- tools::toTitleCase(mods_info$Shape)
mods_info$Set <- tools::toTitleCase(mods_info$Set)
mods_info$Set <-stringr::str_replace_all(mods_info$Set, "Critdamage", "Critical Damage")
mods_info$Set <-stringr::str_replace_all(mods_info$Set, "Critchance", "Critical Chance")
mods_info$Set <- mods_info$Set %>% paste("%") #all set bonuses are % increases
mods_info$Primary <- mods_info$Primary %>% paste("%")
mods_info$Primary <- stringr::str_replace_all(mods_info$Primary, "Speed %", "Speed")

mods_info[,c("Secondary1",
             "Secondary2",
             "Secondary3",
             "Secondary4")] <- mods_info %>%
  select(Secondary1,Secondary2,
         Secondary3,Secondary4) %>%
  sapply(function(x) {gsub("Potency","Potency %",x)}) %>%
  sapply(function(x) {gsub("Tenacity","Tenacity %",x)}) %>%
  sapply(function(x) {gsub("Critical Chance","Critical Chance %",x)})

mods_info[,c("Primary.Value",
             "Secondary1.Value",
             "Secondary2.Value",
             "Secondary3.Value",
             "Secondary4.Value")] <- mods_info %>%
  select(Primary.Value,
         Secondary1.Value,
         Secondary2.Value,
         Secondary3.Value,
         Secondary4.Value) %>%
  sapply(function(x) {gsub("\\+","",x)}) %>%
  sapply(function(x) {gsub("%","",x)}) 

mods_info$Level <- as.numeric(mods_info$Level)
mods_info$Primary.Value <- as.numeric(mods_info$Primary.Value)
mods_info$Secondary1.Value <- as.numeric(mods_info$Secondary1.Value)
mods_info$Secondary2.Value <- as.numeric(mods_info$Secondary2.Value)
mods_info$Secondary3.Value <- as.numeric(mods_info$Secondary3.Value)
mods_info$Secondary4.Value <- as.numeric(mods_info$Secondary4.Value)
mods_info$Pips <- as.numeric(mods_info$Pips)

# Need to create ghost mods for unused slots and
# ghost toons for unused mods
# Aim is to have NoMods = NoToons*6

# Find number of mods of each shape
NoShapes <- map_int(slots, function(x) {mods_info %>% filter(Shape == x) %>% nrow()})

NoGhostToons <- max(max(NoShapes) - length(toon_list),0)

# We need a toon for each shape
if (NoGhostToons > 0) {
  # we need to create ghost toons
  ghost_toon <- "Ghost toon"
  ghost_toons <- paste(ghost_toon, 1:NoGhostToons)
  toon_list <- c(toon_list, ghost_toons)
  rm(ghost_toon, ghost_toons)
}

NoGhostShapes <- pmax(length(toon_list) - NoShapes,0)

# We need a mod of each type for each toon
for (i in 1:length(slots)) {
  
  if (NoGhostShapes[i] > 0) {
    # we need to create ghost mods
    ghost_mod <- paste("Ghost", slots[i], "mod")
    ghost_mods <- paste(ghost_mod, 1:NoGhostShapes[i])
    mod_list <- c(mod_list, ghost_mods)
    extra_rows <- data.frame(ghost_mods,
                             "unassigned",
                             slots[i],
                             "None",
                             1,
                             NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                             stringsAsFactors = FALSE)
    names(extra_rows) <- names(mods_info)
    mods_info <- mods_info %>% bind_rows(extra_rows)
  }
}
rm(extra_rows, ghost_mod, ghost_mods)

################################################################################

# User defined inputs ##########################################################
set.seed(50)
toon_priority <- sample(0:20, 
                        size=length(toon_list),
                        replace = TRUE)
names(toon_priority) <- toon_list

toon_stat_priority <- matrix(sample(0:10,
                                    size = length(toon_list)*length(stat_names),
                                    replace = TRUE),
                             nrow = length(toon_list),
                             ncol = length(stat_names))
rownames(toon_stat_priority) <- toon_list
colnames(toon_stat_priority) <- stat_names
###############################################################################
# End of reading in data



#Define working/dynamic data structures
#toon_stats(1 to NoToons, 1 to NoStats) 2D df consists of numerics
#mod_assignment(1 to NoToons, 1 to NoShapes) 2D matrix consists of mod number

#######################################################################################
# Calculates new stat for toon based on current mod assignment
new_stat <- function(toon, stat, mod_assignment) {
  
  # get Mod IDs for this toon
  toon_mods <- mod_list[mod_assignment[mod_assignment[toon,]>0]]
  
  # sum stat across all mods
  new_stat <- mods_info %>% filter(Mod.ID %in% toon_mods) %>% 
    filter(Primary == stat) %>% select(Primary.Value) %>% colSums() +
    mods_info %>% filter(Mod.ID %in% toon_mods) %>% 
    filter(Secondary1 == stat) %>% select(Secondary1.Value) %>% colSums() +
    mods_info %>% filter(Mod.ID %in% toon_mods) %>% 
    filter(Secondary2 == stat) %>% select(Secondary2.Value) %>% colSums() +
    mods_info %>% filter(Mod.ID %in% toon_mods) %>% 
    filter(Secondary3 == stat) %>% select(Secondary3.Value) %>% colSums() +
    mods_info %>% filter(Mod.ID %in% toon_mods) %>% 
    filter(Secondary4 == stat) %>% select(Secondary4.Value) %>% colSums()
  
  # calculate total number of set bonuses
  if (calc_bonuses <- TRUE) {
    
    num_bonuses <- ifelse(set_bonus_rules[stat,"Number"]==0,0,mods_info %>% 
                            filter(Mod.ID %in% toon_mods, Set == stat) %>% 
                            nrow() %/% set_bonus_rules[stat,"Number"])
    
    # calculate number of max set bonuses
    num_max_bonuses <- ifelse(set_bonus_rules[stat,"Number"]==0,0,mods_info %>% 
                                filter(Mod.ID %in% toon_mods, Set == stat) %>% 
                                filter(Level == 15) %>% 
                                nrow() %/% set_bonus_rules[stat,"Number"])    
  } else {
    num_bonuses <- 0
    num_max_bonuses <- 0
  }
  
  new_stat <- new_stat + num_max_bonuses * set_bonus_rules[stat,"Max.Bonus"] +
    (num_bonuses - num_max_bonuses) * set_bonus_rules[stat,"Bonus"]
  return(as.numeric(new_stat))
}

######################################################################################
overall_score <- function(mod_assignment) {
  
  # refresh stats of toons - THIS IS THE TIME CONSUMING BIT
  # omit ghost toons???
  toon_stats <- setNames(object = data.frame(sapply(stat_names, 
                                  function(x) sapply(toon_list[1:(length(toon_list)-NoGhostToons)],
                                function(y) new_stat(y, x, mod_assignment))),
                                  row.names = toon_list[1:(length(toon_list)-NoGhostToons)]),
                         nm = stat_names) %>% as.matrix()
  
  # normalise objective variables
  toon_stats_norm <- sweep(toon_stats, 2, rowSums(max_stats), '/')
  
  # calculate objective function
  overall_score <- toon_priority[1:(length(toon_list)-NoGhostToons)] * rowSums(toon_stat_priority[1:(length(toon_list)-NoGhostToons)] * toon_stats_norm)
  return(sum(overall_score))
}
######################################################################################
top_n <- function(x, n) {
  top_n <- which(x >= -sort(-x, partial=n)[n])
  return(top_n[1:n])
}
######################################################################################
# create slot specific vectors as dataframe
mod_list_shape <- map(slots, function(x) {mods_info$Mod.ID[mods_info$Shape == x]}) %>%
  as.data.frame()
names(mod_list_shape) <- slots

permutations <- length(toon_list)*length(mod_list) #function of toons and mods
top_permutations <- permutations %/% 10

slot_perms <- matrix(0, nrow=permutations, ncol=length(toon_list))
mod_assignment <- matrix(0, nrow=length(toon_list), ncol=length(slots))
rownames(mod_assignment) <- toon_list
colnames(mod_assignment) <- slots

# record of top scores from each slot, and their assignment
top_perms <- array(0, c(top_permutations, length(toon_list), length(slots)))
perm_scores <- matrix(0, nrow=permutations, ncol=1)
calc_bonuses <- FALSE

#####################################################################################
#Loop through each slot
for (i in 1:length(slots)) {
  
  # put ghost mods with ghost toons  
  for (j in 1:NoGhostShapes[i]) {
    NoPreAssign <- min(NoGhostToons, NoGhostShapes[i])
    if (NoPreAssign > 0 ) {
      for (k in 1:permutations) {
        slot_perms[k, (length(toon_list) - NoPreAssign + 1):length(toon_list)] <- 
          (nrow(mod_list_shape) - NoPreAssign + 1):nrow(mod_list_shape)   
      }
    }
  }
  
  # fill out remaining slots of slot_perms with random permutations
  #  slot_perms[, 1:(length(toon_list) - NoPreAssign)] <-
  #                        map(1:permutations, ~ 
  #                              {sample(1:(nrow(mod_list_shape) - NoPreAssign),
  #                                         nrow(mod_list_shape) - NoPreAssign)}) %>%
  #                        as.matrix()
  for (k in 1:permutations) {
    slot_perms[k, 1:(length(toon_list) - NoPreAssign)] <- 
      sample(1:(nrow(mod_list_shape) - NoPreAssign),
             nrow(mod_list_shape) - NoPreAssign)
  }
  
  # get score of permutations
  # fill out i'th column of mod_assignment
  for (k in 1:permutations) {
    message(slots[i], ", ", k, " out of ", permutations, " permutations")
    mod_assignment[,i] <- slot_perms[k,]
    perm_scores[k] <- overall_score(mod_assignment)
  }
  
  # get the top X and store in a 3D array (perm, toon, slot)
  top_perms[1:top_permutations, 1:length(toon_list), i] <- 
    slot_perms[top_n(perm_scores, top_permutations),] 
}




# By now we will have a set of the top permutations for each shape
# Now randomly combine these - 1 from each of the 3rd dimension
calc_bonuses <- TRUE
for (k in 1:permutations) {
  mod_assignment <- matrix(c(top_perms[sample(1:top_permutations,1),,1],
                             top_perms[sample(1:top_permutations,1),,2],
                             top_perms[sample(1:top_permutations,1),,3],
                             top_perms[sample(1:top_permutations,1),,4],
                             top_perms[sample(1:top_permutations,1),,5],
                             top_perms[sample(1:top_permutations,1),,6]), 
                           nrow=length(toon_list), ncol=length(slots))
  rownames(mod_assignment) <- toon_list
  colnames(mod_assignment) <- slots
  perm_scores[k] <- overall_score(mod_assignment)
}
##################################################################################

#Find the best


# After optimisation, return data frame showing;
# Mod, Shape, Initial Toon, Final Toon


# Optimisation is too big a problem
# How about a tool to assign your mods, showing you what is required in
# visual form
# facet grid of toons showing all 6 slots, with traffic light system:
# Secondary speed 
# 0-5 Red, 6-12 Amber, 12-25 Green (Black means no mod meets the criteria)
# Need mod recommendations list
# Need to prioritise toons still




