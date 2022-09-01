library(ggplot2)
library(dplyr)
require(raster)
#install.packages("geodata")
require("geodata")
#install.packages("tidyr")
library(tidyr)
library("sf")
#install.packages(c("rnaturalearth","rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")
theme_set(theme_bw())

setwd() #your path

# Get map from the "North" of South America
Colo <- ne_countries(country = c("colombia", "ecuador","peru", "brazil", "venezuela", "panama"), scale = "medium", returnclass = "sf")
class(Colo)

#Dots data
mut<- read.table("mut.tsv", header = TRUE)
mut<- as.data.frame(mut)
head(mut)

mutEr<- mut %>% group_by(Genus) %>% filter(Genus == "Erythrolamprus") %>% filter(species=="reginae" | species == "epinephelus"|species == "sp")
summary(mut$Latitude)
summary(mut$Longitude)

#precipitation data using geodata
prec <- worldclim_country(country="COL", var= "prec", path = "./")
class(prec)
summary(prec)

prec1<- as(prec, "Raster")
class(prec1)


#--- convert to data.frame ---#
prec2 <-
  as.data.frame(prec1, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()

#--- take a look ---#
colnames(prec2)

# Adding a variable call ID to calculate the precipitation mean
dim(prec2)
ID<- as.character(c(1:2747510)) 
prec2$ID <- ID
colnames(prec2)

# Using tidyverse to change the database from wide to long
prec_long <- prec2 %>% pivot_longer(COL_wc2.1_30s_prec_1:COL_wc2.1_30s_prec_12)
colnames(prec_long)
head(prec_long)

# Group by ID and summarize the precipitation mean (one dot per location)
prec_long_mean <- prec_long %>% group_by(ID) %>% summarise(x = mean(x), y = mean(y), mean_prec = mean(value) )

#I recommend to save it, it takes long to run
save(prec_long_mean, file= "prec_long_mean.Rdata")
load("prec_long_mean.Rdata")
head(prec_long_mean)

# plot

#creating the map
pd<- ggplot() +
  geom_sf(data = Colo, color = "NA", fill = "NA") + xlim(c(-79, -68)) + ylim(c(-5,10))
# adding the precipitation data
pd2<- pd + geom_tile(data=prec_long_mean, aes(x=x, y=y, fill=mean_prec))
# adding scale color viridis
pd3<- pd2 + scale_fill_viridis_c()
# adding dots
pd3 + geom_point(data=mutEr, 
                aes(x=Longitude, y=Latitude, size=Number_convergent_AA, colour = species), 
                fill="white",pch=21, alpha=I(0.7))
