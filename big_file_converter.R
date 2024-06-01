library(iotools)
library(data.table)
library(dplyr)

#####################
# SET DIRECTORY
#####################
dir <- "C:/Users/Viktor/Documents/Dokumente/Studium/Semester5/T3201/IntmapAnalyzer"
setwd(dir)
getwd()

#####################
# COMMON STATS
#####################
common_files_list <- list.files(path = "csv_common_stats")
common_stat_lst <- vector("list", length=length(common_files_list))
i = 1

# READ FILES AND (IN DARKNESS) BIND THEM
for (file_name in common_files_list) {
  file_path <- paste("csv_common_stats/", file_name, sep='')
  first_file <- read.csv(file_path, sep=";", header=TRUE, row.names=NULL)
  common_stat_lst[[i]] <- first_file
  i = i+1
}
common_stats <- do.call(rbind, common_stat_lst)

# DATA PREPARATION - COMMON STATS
common_stats$Reason[common_stats$Reason == " "] <- "Empty"
common_stats$Problem = gsub(" ", "", common_stats$Problem)

# CONVERT INTO RDS
saveRDS(common_stats, "common_stats.rds")
common_stats <- readRDS("common_stats.rds")
str(common_stats)


#####################
# PROBLEMS
#####################
problem_files_list <- list.files(path = "csv_files")
problem_lst = vector("list", length=length(problem_files_list))
i = 1

# READ FILES AND BIND THEM
for (file_name in problem_files_list) {
   file_path <- paste("csv_files/", file_name, sep='')
   first_file <- fread(file_path, sep=";", header=TRUE) 
#   first_file$Problem <- sub("\\.csv", "", file_name)
   problem_lst[[i]] <- first_file
   i = i+1
}
problems <- do.call(rbind, problem_lst)

# problem_summary <- summary(problems)
# str(problems)
# problem_summary

#CONVERT INTO RDA
# DATA PREPARATION - ARRAY
problems_array <- problems %>% dplyr::filter(IntMapType == "IMArray")
problems_array <- problems_array %>% dplyr::select(-c("IntMapType", "TreeHeight", "Linearity"))

# DATA PREPARATION - TREE
problems_tree <- problems %>% dplyr::filter(IntMapType == "IMTree")
problems_tree <- problems_tree %>% dplyr::select(-c("IntMapType", "Offset", "Size", "NumberOfEmptyCells"))
problems_tree$Density <- problems_tree$NumberOfItems / (problems_tree$MinMaxDistance + 1)
# summary(problems_array)
# head(problems_array, n=3)
summary(problems_tree)
# nrow(problems_tree) 
# nrow(problems_tree_alt)


# SAVE 
saveRDS(problems_array, "problems_array.rds")
saveRDS(problems_tree, "problems_tree.rds")


#???
#problems_tree_alt2 <- problems_tree_alt2 %>% filter(NumberOfItems > 3)
#problems_array_alt2 <- problems_array_alt2 %>% filter(NumberOfItems > 3)

#saveRDS(problems_array_alt2, "problems_array_alt_2.rds")
#saveRDS(problems_tree_alt2, "problems_tree_alt_2.rds")
problems_array <- readRDS("problems_array.rds")
problems_tree <- readRDS("problems_tree.rds")
str(problems_array)
str(problems_tree)

summary(problems_array)
summary(problems_tree)

#####################
# PROBLEMS - ALTERED
#####################
# GET LIST OF FILES FILTERED BY TOTALTIME 
common_stats_filtered_lst <- common_stats %>% dplyr::filter(!is.na(TotalTime)) %>% dplyr::filter(TotalTime >= 60) %>% dplyr::filter(TotalTime <= 65)
files_maxtime_lst <- gsub("\\.p", ".p.csv", common_stats_filtered_lst$Problem)
files_maxtime_lst <- gsub(" ", "", files_maxtime_lst) 
files_maxtime_lst <- intersect(files_maxtime_lst, problem_files_list)
#head(files_maxtime_lst)
# head(problem_files_list)
problem_alt_lst = vector("list", length=length(files_maxtime_lst))
i = 1

# READ FILES AND BIND THEM
for (file_name in files_maxtime_lst) {
  file_path <- paste("csv_files/", file_name, sep='')
  first_file <- fread(file_path, sep=";", header=TRUE) 
  #   first_file$Problem <- sub("\\.csv", "", file_name)
  problem_alt_lst[[i]] <- first_file
  i = i+1
}
problems_alt <- do.call(rbind, problem_alt_lst)

#Bei Arithmetischen Problemen auftretend
#Monomorphe Symbole
#Typfehler zw. Int und Float

# IMSingle wird gecastet zu IMEmpty, darum wird nie gelöscht

#Hauptschleife teilweise lange durchiteriert, damit große Ausreißer nach oben


#Je tiefer wir kommen desto dünner werden die Tries
#Starke Gewichtung auf erste Ebene der Tries, damit hohe Dichte und deswegen Array 
#Sobald Eindutig dann IMSingle
#Bei Einfügung immer
#Beim Lesen 

#Intmap realisiert Verzweigung im Tree
#Ursprünglich Arrays 
#Vergrößerung des Arrays bei GetRef

#logarithmische Skala

#Rekursive Function für Rückgabe Anzahl Intmap-type
#Print Aufsplitten

# DATA PREPARATION - ARRAY
problems_array_alt <- problems_alt %>% dplyr::filter(IntMapType == "IMArray")
problems_array_alt <- problems_array_alt %>% dplyr::select(-c("IntMapType", "TreeHeight", "Linearity"))

# DATA PREPARATION - TREE
problems_tree_alt <- problems_alt %>% dplyr::filter(IntMapType == "IMTree")
problems_tree_alt <- problems_tree_alt %>% dplyr::select(-c("IntMapType", "Offset", "Size", "NumberOfEmptyCells"))
problems_tree_alt$Density <- problems_tree_alt$NumberOfItems/ ( problems_tree_alt$MinMaxDistance + 1 )

summary(problems_array_alt)
# head(problems_array, n=3)
summary(problems_tree_alt)
# nrow(problems_tree) 
# nrow(problems_tree_alt)
#nrow(problems_tree_alt)
# write.csv(common_stats, "common_stats.csv", row.names = TRUE)
# write.csv(problems_array, "problems_array.csv", row.names = TRUE)
# write.csv(problems_tree, "problems_tree.csv", row.names = TRUE)

# SAVE 
saveRDS(problems_array_alt, "problems_array_alt.rds")
saveRDS(problems_tree_alt, "problems_tree_alt.rds")

write.csv(problems_array_alt, "problems_array_alt.csv", row.names = TRUE)
write.csv(problems_tree_alt, "problems_tree_alt.csv", row.names = TRUE)

problems_array_alt <- readRDS("problems_array_alt.rds")
problems_tree_alt <- readRDS("problems_tree_alt.rds")

#str(problems_array_alt)
#library(ggplot2)
#ggplot(problems_array_alt, aes(x = NumberOfItems, y = CountGetRef)) +  stat_density_2d()
#ggplot(problems_tree_alt, aes(x = NumberOfItems, y = CountGetRef)) + geom_point(alpha = 0.2)