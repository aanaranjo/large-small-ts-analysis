#Script that determines if there is a correlation between house else ear size and GC content of their DNA

library("stringr")
library("dplyr")

#GC content  calculator function
GC_content_calc <- function(a){
  Gs <- str_count(a, str_to_upper('g', 'en'))
  Cs <- str_count(a, str_to_upper('c', 'en'))
  gc_content <- (Gs + Cs) / str_length(a) * 100
}

#import elf dataset
elf <- read.csv("houseelf-earlength-dna-data-1.csv", header=TRUE)

#function to determine small and large ears
ear_size <- function(length){
  if (length > 10){
    size = "large"
  } else {
    size = "small"
  }
  return(size)
}

add_size <- function(df){
  #add size to data frame
  #df will be the earlength column from the dataset 
  ear_sizes <-  
    df %>%
    na.omit() %>%
    rowwise() %>%
    mutate(size = ear_size(earlength))
  return(ear_sizes)
}

# add the size categories to the elf dataframe
elf <- add_size(elf)

#GC content calculation for each elf
add_GC_content <- function(df){
  elf_GC <-  
    df %>%
    na.omit() %>%
    rowwise() %>%
    mutate(GC_percents = GC_content_calc(dnaseq))
  return(elf_GC)
}

get_size_class <- function(seq){
  #Calculate the GC-content for one or more sequences
  ear_lengths <- ifelse(seq > 10, "large", "small")
  return(ear_lengths)
}

#GC content to the dataframe
elf <- add_GC_content(elf)

#id, size, and GC_percents from larger elf data frame
elf_subset <- subset(elf, select=c("id", "size", "GC_percents"))

#Writing CSV file that contains ID, size, and GC_percents
write.csv(elf_subset, file = "grangers_analysis.csv", row.names=FALSE)
