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

# READ FILES AND BIND THEM
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

problems_tree_alt2 <- problems_tree_alt2 %>% filter(NumberOfItems > 3)
problems_array_alt2 <- problems_array_alt2 %>% filter(NumberOfItems > 3)

saveRDS(problems_array_alt2, "problems_array_alt_2.rds")
saveRDS(problems_tree_alt2, "problems_tree_alt_2.rds")
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

str(problems_array_alt)

library(ggplot2)

ggplot(problems_array_alt, aes(x = NumberOfItems, y = CountGetRef)) +
  stat_density_2d()

ggplot(problems_tree_alt, aes(x = NumberOfItems, y = CountGetRef)) +
  geom_point(alpha = 0.2)



# LINKS
#https://statistik-dresden.de/daten-mit-r-in-bloecken-verarbeiten-mit-iotools-big-data-werkzeug/
#https://cran.r-project.org/web/packages/iotools/iotools.pdf
#https://campus.datacamp.com/courses/scalable-data-processing-in-r/working-with-iotools?ex=8
#https://statistik-dresden.de/daten-mit-r-in-bloecken-verarbeiten-mit-iotools-big-data-werkzeug/
#<http://rmarkdown.rstudio.com>.

########################################################
#readfile <- read.csv("immo_data.csv")
#str(readfile)
#head(readfile, n=3)
#save(readfile, file='test.rda')

#load('test.rda')
#str(readfile)
#head(readfile, n=3)

#immo_data.csv


# flights.csv
# readfile <- fread("outData.csv")
# col_types <- sapply(readfile, typeof)
# print(col_types)
# results <- chunk.apply("outData.csv", function(x) {
#    d <- dstrsplit(x, col_types = col_types, sep = ",")
#    c(quantile(d[, 16], c(0.25, 0.5, 0.75), na.rm = TRUE), n = nrow(d))
#    # Hier können andere Funktionen zur Datenaufbereitung oder Datenanalyse stehen
#    },
# CH.MAX.SIZE = 8e6)
#, parallel = 2

# str(results)
# head(results, n=3)

#R < scriptName.R --no-save

#col_types <- sapply(flights, typeof)

#install.packages('dplyr')
#install.packages('R.utils')
#install.packages(iotools)
#install.packages(data.table)

#in Rds-file df speichern

#Verarbeitung

#
#save(df, plot, info, file="diamonds.Rda")
#rm(list = ls()) #  remove objects from global environments
#load("diamonds") # lädt das Objekt ohne Zuweisung in den Workspace
#ls()



#Boxplots
##CountGetVal
##CountGetRef
##CountAssign
##CountDelKey
##CountArrayToTree
##CountTreeToArray
##NumberOfItems
##MinNode
##MaxNode
##MinMaxDistance
##Density


# problem stats

# problem_files_list <- list.files(path = "csv_files")
# problem_lst = vector("list", length=length(problem_files_list))
# i = 1
# 
# for (file_name in problem_files_list) {
#   file_path <- paste("csv_files/", file_name, sep='')
#   first_file <- read.csv(file_path, sep=";", header=TRUE, row.names=NULL)
#   first_file$ProblemClass <- substr(file_name, 1, 3)
#   first_file$Problem <- sub("\\.csv", "", file_name)
#   problem_lst[[i]] <- first_file
#   i = i+1
# }
# problems <- do.call(rbind, problem_lst)

#./eprover -R --print-statistics --auto --soft-cpu-limit=60 --cpu-limit=300 ../../TPTP-v8.2.0/Problems/LCL/LCL524+1.p
#./eprover -R --print-statistics --auto --soft-cpu-limit=60 --cpu-limit=300 ../../TPTP-v8.2.0/Problems/SWW/SWW237+1.p
# nohup python3 examine_intmap.py ../../TPTP-v8.2.0/Problems/LCL/ ../../csv_files f &
# nohup python3 examine_intmap.py ../../TPTP-v8.2.0/Problems/SWW/ ../../csv_files f &
#install.packages('dplyr')
#install.packages('R.utils')
#install.packages('iotools')
#install.packages('data.table')

# problem_files_list <- list.files(path = "empty")
# fileConn<-file("output.txt")
# writeLines(problem_files_list, fileConn)
# close(fileConn)

# write.csv(problems_array_alt, "array_alt.csv")
# write.csv(problems_tree_alt, "tree_alt.csv")