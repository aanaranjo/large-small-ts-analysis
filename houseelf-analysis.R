#This script is meant to analyze the DNA of house elves in my living room
#Commit 1 was random
houseelf_earlength_dna <- read.csv("houseelf-earlength-dna-data-1.csv")
#Commit 2 was to add CSV

#Get GC Content
library(stringr)
library(ggplot2)

Get_GC_Content <- function(houseelf_earlength_dna){
  seq_upper <- str_to_upper(houseelf_earlength_dna)
  Gs <- str_count(seq_upper, "G")
  Cs <- str_count(seq_upper, "C")
  gc_content <- (Gs +Cs) / str_length(seq_upper) *100
  return(gc_content)
}
