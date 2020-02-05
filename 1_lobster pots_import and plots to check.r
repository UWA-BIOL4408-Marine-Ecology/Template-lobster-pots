

# 1. Import lobster pot data

# 1. Import data - Tim has done tricky stuff in another script
# 2. Plot and describe the data.


library(tidyr) #to tidy data
library(dplyr) #to transform data
library(readr) #to write data
library(ggplot2) #to plot data
library(ggmap)
library(readr) #to write data
library(here) #to make robust links to files

# It is useful to set a study name
study<-"lobster.pots"



# We are going to use the here() function, which creates a shortcut to your location.

here::here()


# 
# As long as the names on the folders are consistent this function will enable us to work across computers and operating systems.
# 
# If you are using an ecocloud server - we will have to add folder names to the here() function - but this should work

#here("workspace","Template-lobster-pots","Data")

# Let's make a shortcut to that "Data" directory.

data.dir <- here::here("Data")

#or for ecocloud

#data.dir <- here("workspace","Template-lobster-pots","Data")

# Let's make a plots directory and then shortcut to that directory.

dir.create(file.path(here::here(), "Plots")) #create Data folder

plot.dir <- here::here("Plots")

#or for ecocloud
#dir.create(file.path(here(), "workspace","Template-lobster-pots","Plots")) #create Data folder

#data.dir <- here("workspace","Template-lobster-pots","Plots")




# Read in polygons of NTR------

setwd(data.dir)
dir()
ntr<-read_csv("ntr.2019-01-23.csv")%>%
  glimpse()






# Read in pot data

setwd(data.dir)
dat<-read_csv("lobster.pots.2019-01-23.csv")%>%
  glimpse()






## Basic plots to check the data


# Get map of Rottnest

# Bounding box
bbox <- c(115.435,-32.04,  115.57,-31.975)

# make the map
rotto.map<-(get_stamenmap(as.vector(bbox), zoom = 14, maptype="terrain"))

ggmap(rotto.map)


