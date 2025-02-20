# Master script for "Overseeing" - replication

# Install libraries
library(tidyverse)
library(tidytext)

# Read the dataset 

# Uncomment if trying to replicate this specific section, otherwise download the public file 
# from Harvard Dataverse and read it in as set out in code for workflow step 2
# overseeing_input <- readRDS("overseeing_input.RDS")

# drop 174 issuer delisting notices
depdoc_missing_1 <- overseeing_input %>%
  filter(!str_detect(text, "Issuer Delisting")) |>
  filter(is.na(depdoc)) |>
  mutate(releasenum = str_extract(text, "Release No\\..*?;"),
         filenum_19b4 = str_extract(text, "File No\\..*?[\\w–\\-]+")) 

# manually correct missing (NA) 19b-4 file numbers
depdoc_missing_2 <- depdoc_missing_1 |>
  filter(is.na(filenum_19b4)) |>
  mutate(filenum_19b4 = 
           c("SR–PHLX–00–08",
             "SR–Amex–2007–117; SR–BSE–2007–44; SR–CBOE–2007–121; SR–ISE–2007–92; SR–NYSEArca–2007–109; SR–Phlx–2007–86",
             "SR–ODD–2009–01",
             "SR–CBOE–2010–008",
             "SR–ODD–2010–01",
             "SR-CBOE-2010-066",
             "SR–C2–2011–014",
             "SR–CBOE–2011–081",
             "SR–CBOE–2011–079",
             "SR–CBOE–2011–102",
             "SR–NYSEMKT–2012–40",
             "SR–NYSEArca–2012–89",
             "SR–CBOE–2012–087",
             "SR–ISE–2012–75",
             "SR–NYSE–2012–74",
             "SR–EDGA–2013–02",
             "SR–BOX–2013–15")) |>
  select(index, filenum_19b4)

depdoc_missing_1 <- depdoc_missing_1 |>
  filter(!index %in% depdoc_missing_2$index) |>
  select(index, text) |>
  mutate(filenum_19b4 = str_extract(text, "(SR|)[-––\\s]([0-9A-Za-z]+)[-––\\s](([0-9a-z]+|\\d+))(?:[-––\\s](\\d+))")) 

# filter 7 more delistings for a total of 181
depdoc_missing_1[c(6,7,11,12,14:17),3] <- c("SR–NASD–2001–72", NA, NA, NA,  NA, NA, NA, NA)
# "1-9120", "1-7167", "1-13471", "1-12031", "1-03551", "1-03822", "1-06841"

# 55 new filenums   
depdoc_missing_1 <- depdoc_missing_1 |>
  select(!text) |>
  rbind(depdoc_missing_2) |>
  filter(!is.na(filenum_19b4)) 

overseeing_input <- overseeing_input |>
  rows_patch(depdoc_missing_1) |>
  filter(!is.na(filenum_19b4))

# 31877 observations

# cleaning up some NAs in the clearing agencies

clearing_agencies_clean <- overseeing_input |>
  filter(is.na(category)) |>
  select(index, category) 

clearing_agencies_clean$category <- 
  c(rep("registered clearing agencies", 14), "notice registered securities future product exchanges")

overseeing_input <- overseeing_input |>
  rows_patch(clearing_agencies_clean) |>
  filter(!is.na(category))

# count of unique filenums

overseeing_input |>
  filter(category %in% c("national securities exchanges", "registered securities associations")) |>
  select(index, currentname, category, filenum_19b4) |>
  group_by(currentname, filenum_19b4) |>
  summarize(count = n()) |>
  ungroup() |>
  group_by(currentname) |>
  summarize(count = n()) |>
  print(n=Inf)  
  
#=====================================
# Clean the dataset for analysis
#=====================================

overseeing_input <- overseeing_input |>
#  mutate(immediate_effectiveness_text = str_extract(subject_cleaned, ".{0,25}(immediat).{0,25}"),
#       immediate_effectiveness = !is.na(immediate_effectiveness_text)) |>
  mutate(instituting_proceedings_text = str_extract(subject_cleaned, ".{0,25}institut.{0,25}proc.{0,25}"),
         instituting_proceedings = !is.na(instituting_proceedings_text)) |>
    select(index, 
         fedreg_date, 
         category,
         filenum_19b4,
         currentname,
         subject_cleaned,
         immediate_effectiveness,
         instituting_proceedings,
         fee_filing,
         fedreg_vol,
         fedreg_num,
         prtpage,
         text
  ) 

# comment out if not trying to save this or use it from here on out
saveRDS(overseeing_input, "cleaned_dataset_for_public.RDS")
