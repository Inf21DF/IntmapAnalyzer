library(iotools)
library(data.table)
library(dplyr)

dir <- "C:/Users/Viktor/Documents/Dokumente/Studium/Semester5/T3201/IntmapAnalyzer"
setwd(dir)
getwd()
problem_files_list <- list.files(path = "csv_files")
problem_lst = vector("list", length=length(problem_files_list))
i = 1

#READ FILE 
for (file_name in problem_files_list) {
   file_path <- paste("csv_files/", file_name, sep='')
   first_file <- fread(file_path, sep=";", header=TRUE) 
#   first_file$ProblemClass <- substr(file_name, 1, 3)
#   first_file$Problem <- sub("\\.csv", "", file_name)
   problem_lst[[i]] <- first_file
   i = i+1
}
problems <- do.call(rbind, problem_lst)

problem_summary <- summary(problems)
str(problems)

#CONVERT INTO RDA
problems_array <- problems %>% filter(IntMapType == "IMArray")
problems_array <- problems_array %>% select(-c("IntMapType", "TreeHeight", "Linearity"))
problems_array_alt <- problems_array %>% filter(CountGetVal >= 0)
problems_array_alt <- problems_array_alt %>% filter(CountGetRef >= 0)
problems_array_alt <- problems_array_alt %>% filter(CountAssign >= 0)
problems_array_alt <- problems_array_alt %>% filter(CountDelKey >= 0)
problems_array_alt <- problems_array_alt %>% filter(CountArrayToTree >= 0)
problems_array_alt <- problems_array_alt %>% filter(CountTreeToArray >= 0)

summary(problems_array_alt)
head(problems_array_alt, n=3)

nrow(problems_array) - nrow(problems_array_alt)

# COMPARISON - ARRAY
# All Arrays: 11782197
# Cleaned: 11569590
# DIFF: 212607
# PERC: 1.804477

# SAVE 
saveRDS(problems_array, "problems_array.rds")
saveRDS(problems_array_alt, "problems_array_alt.rds")


# PROCESSING TREE
problems_tree <- problems %>% filter(IntMapType == "IMTree")
problems_tree <- problems_tree %>% select(-c("IntMapType", "Offset", "Size", "Grow", "NumberOfEmptyCells"))

problems_tree_alt <- problems_tree %>% filter(CountGetVal >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(CountGetRef >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(CountAssign >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(CountDelKey >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(CountArrayToTree >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(CountTreeToArray >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(Density >= 0)
problems_tree_alt <- problems_tree_alt %>% filter(Density <= 1)

summary(problems_tree_alt)
nrow(problems_tree) - 
nrow(problems_tree_alt)

# COMPARISON - TREE
#All Trees: 5738005
#Cleaned: 5664310
#DIFF: 73695
#PERC: 1.284331

saveRDS(problems_tree, "problems_tree.rds")
saveRDS(problems_tree_alt, "problems_tree_alt.rds")


########################################################
#readfile <- read.csv("immo_data.csv")
#str(readfile)
#head(readfile, n=3)
#save(readfile, file='test.rda')

#load('test.rda')
#str(readfile)
#head(readfile, n=3)

#immo_data.csv

#https://statistik-dresden.de/daten-mit-r-in-bloecken-verarbeiten-mit-iotools-big-data-werkzeug/

# flights.csv



readfile <- fread("outData.csv")
col_types <- sapply(readfile, typeof)
print(col_types)
results <- chunk.apply("outData.csv", function(x) {
   d <- dstrsplit(x, col_types = col_types, sep = ",")
   c(quantile(d[, 16], c(0.25, 0.5, 0.75), na.rm = TRUE), n = nrow(d))
   # Hier können andere Funktionen zur Datenaufbereitung oder Datenanalyse stehen
   },
CH.MAX.SIZE = 8e6)
#, parallel = 2


str(results)
head(results, n=3)

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



#https://cran.r-project.org/web/packages/iotools/iotools.pdf
#https://campus.datacamp.com/courses/scalable-data-processing-in-r/working-with-iotools?ex=8

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



