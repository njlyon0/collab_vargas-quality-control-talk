## ---------------------------------------- ##
          # Bee Data Quality Control
## ---------------------------------------- ##
# Written by Nick J Lyon

# Purpose
## Wrangle raw native bee data from 2018 into analyzable format

# Housekeeping -------------------------------

# Clear environment
rm(list = ls())

# Save working directory
myWD <- getwd()
myWD

# Load libraries
# install.packages("devtools")
# devtools::install_github("james-thorson/FishLife") # non-CRAN dependency of freeR that must be installed separately
# devtools::install_github("cfree14/freeR")
# devtools::install_github("njlyon0/helpR")
library(plyr); library(tidyverse); library(freeR); library(helpR)

# Read in raw data
bees_v0 <- read.csv(file = file.path("Data", "raw_bee_data.csv"))
## See end of script for column definitions (i.e., data dictionary)

# Identifying Errors -------------------------

# Check structure of data
str(bees_v0)

# Look at summary of all columns
summary(bees_v0)
## Note: not especially useful for character columns

# Look at min/max of numeric column
range(bees_v0$Sampling_Event_ID, na.rm = TRUE)

# Counting NAs
plyr::count(is.na(bees_v0$Pinned))
## NA's aren't an issue in that column but nice to check

# Can check the "completenes" of all columns in a dataframe with `freeR`
freeR::complete(bees_v0)
## Value is number of NAs per column

# Look at contents of a character column
unique(bees_v0$Height)
## Quickly identified space typo in at least one instance of "High" that would be invisible to human eye

# For contents with more unique entries, wrapping with `sort` can be really helpful!
sort(unique(bees_v0$Bowl_Color))
## Several typos but alphabetization makes visually grouping them easier

# Looking specifically for non-numbers?
helpR::num_chk(data = bees_v0, col = "Number")
## Returns only unique 'problem' entries so you don't need to parse through correct values from a `unique` call

# Can also provide a vector of columns that should be numeric
helpR::multi_num_chk(data = bees_v0, col_vec = c("Capture_Date", "Number"))

# Fixing Errors ------------------------------

# Often a good call to make a duplicate data object before implementing changes
bees_v1 <- bees_v0

# That way, if you mess something up...
bees_v1$Number <- NA
str(bees_v1)

# You don't need to go all the way to the 'reading in the data' step
bees_v1 <- bees_v0
str(bees_v1)

# Can unconditionally replace when there is a global wrong value (i.e., one that doesn't depend on values in other columns)
## `gsub()`
# Check contents
sort(unique(bees_v1$Bowl_Color))
# Fix errors
bees_v1$Bowl_Color <- gsub(pattern = " Yellow|Yellow |Yelow",
                           replacement = "Yellow",
                           x = bees_v1$Bowl_Color)
bees_v1$Bowl_Color <- gsub("Bue", "Blue", bees_v1$Bowl_Color)
bees_v1$Bowl_Color <- gsub("Whie", "White", bees_v1$Bowl_Color)
# Smart to re-check after implementing a fix
sort(unique(bees_v1$Bowl_Color))

## `stringr_str_replace()`
unique(bees_v1$Height)
bees_v1$Height <- stringr::str_replace(string = bees_v1$Height,
                                      pattern = "High ",
                                      replacement = "High")
unique(bees_v1$Height)

# I (Nick) duplicate the data frequently to preserve upstream changes
bees_v2 <- bees_v1

# If conditional replacement is necessary, there are several options

## `ifelse()`
# Find a bad date
unique(bees_v2$Capture_Date)
# Find the correct value that is in the same row as the bad value (diff column)
unique(paste(bees_v2$Capture_Date, bees_v2$Sampling_Event_ID, sep = '-'))
# Fix it conditionally
bees_v2$Capture_Date <- with(data = bees_v2,
                             expr = ifelse(test = Sampling_Event_ID == 44 &
                                 Capture_Date == "6.oe",
                               yes = "6.04",
                               no = Capture_Date))
unique(bees_v2$Capture_Date)

# Duplicate the data
bees_v3 <- bees_v2

## `dplyr::case_when()`
# Find bad numbers
( bad_nums <- helpR::num_chk(data = bees_v3, col = "Number") )
# Find a correct value that links to them
unique(dplyr::filter(bees_v3, Number %in% bad_nums)$Genus)
# Fix those errors conditionally
bees_v4 <- bees_v3 %>%
  dplyr::mutate(Number = dplyr::case_when(
    # Format is:
    ## [condition check] ~ [if TRUE, what should be done]
    Number %in% bad_nums & Genus == "Agapostemon" ~ '1',
    Number %in% bad_nums & Genus == "Augochlorella" ~ '1',
    Number %in% bad_nums & Genus == "Ceratina" ~ '1',
    Number %in% bad_nums & Genus == "Lasioglossum" ~ '1',
    Number %in% bad_nums & Genus == "X" ~ '0',
    # Format of final line is:
    ## [do above if TRUE] ~ [otherwise, what should be done]
    TRUE ~ Number))
# Re-check that it worked
helpR::num_chk(data = bees_v4, col = "Number")

# Finalizing Data Structure ------------------
# Now that some of these are fixed, we can change their structure to be more appropriate
bees_v5 <- bees_v4 %>%
  dplyr::mutate(
    Capture_Date = as.numeric(Capture_Date),
    Height = as.factor(Height),
    Bowl_Color = as.factor(Bowl_Color),
    Number = as.numeric(Number)
  ) %>%
  as.data.frame()

# Re-check structure of dataframe
str(bees_v5)

# Re-evaluate the summary
summary(bees_v5)

# Data Dictionary ----------------------------

# Sampling_Event_ID: numeric code for each unique combination of year, date, round, site, and patch
# Capture_Year: integer, year the data were collected
# Capture_Date: numeric, decimal sampling date (m.dd)
# Round: character, number of iterations of sampling on that patch
# Site: character, abbreviation of site name (three letters)
# Patch: character, `Site` + a one-letter abbreviation for the "patch" (sites are divided into three equally-sized patches and treatments are applied at this scale)
# Height: character, either "High" or "Low" for the placement of the bee bowl on a given point on the transect
# Bowl_Position: character, which of the six points along the transect the bowl was placed (each 20m apart, first is southernmost)
# Bowl_Color: character, one of white, yellow, or blue depending on color of bowl
# Bowl_Size: character, number of fluid ounces bowl can contain
# Bowl_Status: character, whether a bowl was retrieved or something happened to it
# Specimen_ID: numeric, unique four-digit ID for each individual bee retrieved from the site
# Family: character, family of the identified bee
# Genus: character, genus of the identified bee
# Species: character, specific epithet (second part of Latin name) of the identified bee
# Sex: character, either "F" or "M" for female/male bees or "X" for placeholder row for a row without a bee ID
# Number: numeric, either 1 or 0 depending on whether a bee was identified or if the row is a placeholder for a completely empty bowl
# Pinned: character, "Y" for yes, the specimen was pinned or NA for not applicable
# ID_Check: character, initials of the person who double-checked a given bee's ID (some bees are very tricky to ID and a second opinion is needed)

# End ----

