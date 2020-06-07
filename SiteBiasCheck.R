###Looking for biases in MBrown sites###
library(raster)
library(rgdal)
library(ggplot2)
library(gghighlight)
library(dplyr)
library(sf)

#Set working directory
setwd("C:/Users/cedge/Documents/V Drive Copy/Grad Students/Brown Marika/NB Site Selection/NBSites")

#Read in the shapfiles
Herb_Block <- st_read("2005_2017_SelectedHerbBlocks.shp")
Cont_Block <- st_read("ControlBlocks2005-2018.shp")

#Map polygons
ggplot() + 
  geom_sf(data = Herb_Block, fill = "red", color = "red") +
  geom_sf(data = Cont_Block, fill = "blue", color = "blue") +
  coord_sf()
#Spread of herbicide v control blocks looks ok

#Herbicide blocks
#Check unique values for variables
unique(Herb_Block$FST) #Forest Stand type, No Variation
unique(Herb_Block$ORIGIN) #Origin of stand, No Variation
unique(Herb_Block$TRTYR) #Last silvivulture treatment, some variation, should check
unique(Herb_Block$H1) #1st harvst treatment, no variation
unique(Herb_Block$H1YR) #year of first harvest treatment, Check
unique(Herb_Block$H2) #Second harvest treatment, no second treatment
unique(Herb_Block$RF) #Reforestation, no variation
unique(Herb_Block$RFYR) #year of reforestation treatment, Check
unique(Herb_Block$SI) #Stand Improvement treatment, there are some TI, what is TI?
unique(Herb_Block$SIYR) #Stand Improvment years are all before most recent harvest...do we care?
unique(Herb_Block$L1DATAYR) #Year of mechantable layer. Check
unique(Herb_Block$L1FUNA) #Merchantable layer forest unit.  We expect merchant in older stands
unique(Herb_Block$L2DATAYR) #Year of unmerchantable layer
unique(Herb_Block$L2FUNA) #Unmerchantable layer forest unit


#Histograms to look at spread of individual variables
ggplot() +
  geom_histogram(data = Herb_Block, aes(H1YR)) #Looks OK, spread is even
ggplot() +
  geom_histogram(data = Herb_Block, aes(RFYR)) #Looks OK, spread is even
ggplot() +
  geom_histogram(data = Herb_Block, aes(TRTYR)) #Looks OK, spread is even

#remove the values =0 for merchantable layer because it may not have developed yet
Herb_Block_n0L1DATAYR <- Herb_Block %>%
  filter(L1DATAYR > 0)
ggplot() +
  geom_histogram(data = Herb_Block_n0L1DATAYR, aes(L1DATAYR)) #I didn't expect a bell curve
ggplot() +
  geom_point(data = Herb_Block, aes(x = H1YR, y = L1DATAYR))
#Lets check the unmerchantable layer
ggplot() +
  geom_point(data = Herb_Block, aes(x = H1YR, y = L2DATAYR)) #Expected pattern


#Control Blocks
#Check unique values for variables
unique(Cont_Block$FST) #Forest Stand type, No Variation
unique(Cont_Block$ORIGIN) #Origin of stand, No Variation
unique(Cont_Block$TRTYR) #Last silvivulture treatment, some variation, should check
unique(Cont_Block$H1) #1st harvst treatment, no variation
unique(Cont_Block$H1YR) #year of first harvest treatment, Check
unique(Cont_Block$H2) #Second harvest treatment, Clear Cut occurs
unique(Cont_Block$RF) #Reforestation, no variation
unique(Cont_Block$RFYR) #year of reforestation treatment, Check
unique(Cont_Block$SI) #Stand Improvement treatment, there are some TI, what is TI?
unique(Cont_Block$SIYR) #Stand Improvment years are all before most recent harvest...do we care?
unique(Cont_Block$L1DATAYR) #Year of mechantable layer. Check
unique(Cont_Block$L1FUNA) #Merchantable layer forest unit.  We expect merchant in older stands
unique(Cont_Block$L2DATAYR) #Year of unmerchantable layer
unique(Cont_Block$L2FUNA) #Unmerchantable layer forest unit

#Histograms to look at spread of individual variables
ggplot() +
  geom_histogram(data = Cont_Block, aes(H1YR)) #Looks OK, spread is even
ggplot() +
  geom_histogram(data = Cont_Block, aes(RFYR)) #Looks OK, spread is even
ggplot() +
  geom_histogram(data = Cont_Block, aes(TRTYR)) #Looks OK, spread is even,

#remove the values =0 for merchantable layer because it may not have developed yet
Cont_Block_n0L1DATAYR <- Cont_Block %>%
  filter(L1DATAYR > 0)
ggplot() +
  geom_histogram(data = Cont_Block_n0L1DATAYR, aes(L1DATAYR)) #OK
ggplot() +
  geom_point(data = Cont_Block, aes(x = H1YR, y = L1DATAYR))
#Lets check the unmerchantable layer
ggplot() +
  geom_point(data = Cont_Block, aes(x = H1YR, y = L2DATAYR)) #Expected pattern


#Making comparisons between the control and herbicide blocks

#Checking how long after harvest are plots planted
ggplot() +
  geom_point(data = Herb_Block, aes(x = H1YR, y = (RFYR-H1YR))) +
  ggtitle("Herbicide")
ggplot() +
  geom_point(data = Cont_Block, aes(x = H1YR, y = (RFYR-H1YR))) +
  ggtitle("Control")
#Some control plots are planted 4 years after harvest, looks ok though

#Checking how long after harvest the last silviculture treatment was applied
ggplot() +
  geom_point(data = Herb_Block, aes(x = H1YR, y = (TRTYR-H1YR))) +
  ggtitle("Herbicide")
ggplot() +
  geom_point(data = Cont_Block, aes(x = H1YR, y = (TRTYR-H1YR))) +
  ggtitle("Control")
#Some control plots are four years post whereas treatment are 3. I don't see a weird pattern.
#Last year of silviculture produces the same figure as harvest as expected.

#Checking spatial distribution
ggplot() + 
  geom_sf(data = Herb_Block, aes(fill = factor(H1YR)), color = NA) +
  theme_bw() +
  ggtitle("Herbicide") +
  coord_sf()

ggplot() + 
  geom_sf(data = Cont_Block, aes(fill = factor(H1YR)), color = NA) +
  theme_bw() +
  ggtitle("Control")
  coord_sf()
#lots of years so it is hard to see patterns

#Create a plot for each year
for (i in unique(Herb_Block$H1YR)){
  plot <- ggplot() + 
    geom_sf(data = Herb_Block, aes(fill = factor(H1YR)), color = NA) +
    ggtitle(paste("Herbicide", i, sep=' ')) +
    gghighlight(H1YR == i) + #change year to represent each year
    theme_bw() +
    coord_sf()
  
  #save the plot as a pdf
  ggsave(plot, file=paste("Herb_", i, ".pdf", sep=''), scale=2)
  print(plot)
}

for (i in unique(Cont_Block$H1YR)){
  plot <- ggplot() + 
    geom_sf(data = Herb_Block, aes(fill = factor(H1YR)), color = NA) +
    ggtitle(paste("Control", i, sep=' ')) +
    gghighlight(H1YR == i) + #change year to represent each year
    theme_bw() +
    coord_sf()
  
  #save the plot as a pdf
  ggsave(plot, file=paste("Cont_", i, ".pdf", sep=''), scale=2)
  print(plot)
}

#can also facet wrap, but each plot becomes pretty small. I created a pdf to zoom in
herb_plot <- ggplot() + 
  geom_sf(data = Herb_Block, aes(fill = factor(H1YR), color = factor(H1YR))) +
  ggtitle("Herbicide") +
  gghighlight() +
  facet_wrap(~H1YR) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())
ggsave(herb_plot, file="Herbicide_AllYears.pdf", scale=2)

cont_plot <- ggplot() + 
  geom_sf(data = Cont_Block, aes(fill = factor(H1YR), color = factor(H1YR))) +
  ggtitle("Control") +
  gghighlight() +
  facet_wrap(~H1YR) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank())
ggsave(cont_plot, file="Control_AllYears.pdf", scale=2)
#there is some clustering in each year. I think this is somewhat expected.