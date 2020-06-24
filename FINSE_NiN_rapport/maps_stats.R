library(rgdal)
library(sp)
library(ggplot2)
library(rgeos)
library(dplyr)

#path="C:/Users/peterhor/Google Drive/2015 - University of Oslo/GEco/2019 - NiN - Finse SNOW FLUX/"
path="C:/Users/peterhor/OneDrive - Universitetet i Oslo/Finse - NiN kartlegging/FINSE_NiN_rapport"
#### FINSE SNOW ####
finse_snow <- readOGR(dsn = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Finse - NiN kartlegging/VECTOR_5000", "Finse-SNOW_2018_merged_PH_ANB")
#finse_snow <- readOGR(dsn = "D:/Finse - NiN kartlegging_SNOW_FLUX", "Finse-SNOW_2018_merged_PH_ANB")
plot(finse_snow)
plot(finse_snow$kode_enhet)

barplot(finse_snow$kode_enhet~finse_snow$Area)
names(finse_snow@data)

finse_snow_data <- finse_snow@data[,c(3,5,17,47,48,51)]
names(finse_snow_data) <- c("kode_hoved", "kode_enhet", "busk", "lav", "blokkmark", "area"  )
str(finse_snow_data)
SNOW_kode_enhet_sums <- aggregate(finse_snow_data$area,list(finse_snow_data$kode_enhet),sum)
SNOW_kode_enhet_sums[,3] <- round(SNOW_kode_enhet_sums[,2]/sum(SNOW_kode_enhet_sums[,2])*100, 4)
colnames(SNOW_kode_enhet_sums) <- c("kode_enhet", "area", "percent")
SNOW_kode_hoved_sums <- aggregate(finse_snow_data$area,list(finse_snow_data$kode_hoved),sum)
SNOW_kode_hoved_sums[,3] <- round(SNOW_kode_hoved_sums[,2]/sum(SNOW_kode_hoved_sums[,2])*100, 4)
colnames(SNOW_kode_hoved_sums) <- c("kode_hoved", "area", "percent")
#this accomplishes the same #tapply(finse_snow_data$area,list(finse_snow_data$kode_hoved),FUN=sum)
#save
write.csv2(SNOW_kode_enhet_sums, file = paste0(path,"tables/", "SNOW_e.csv"))
write.csv2(SNOW_kode_hoved_sums, file = paste0(path,"tables/", "SNOW_h.csv"))
# add to data a new column termed "id" composed of the rownames of data
finse_snow@data$id <- rownames(finse_snow@data)
proj4string(finse_snow)
# create a data.frame from our spatial object
finse_snow_df <-fortify(finse_snow)

map <- ggplot() +
  geom_path(data = finse_snow_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2)
print(map) 

#ploting graphs
#area vs nin type or main group
ggplot(finse_snow_data, aes(x=kode_enhet, y=area, fill=kode_hoved)) + geom_boxplot() + scale_fill_brewer(palette = "Spectral")

ggplot(finse_snow_data, aes(x=kode_hoved, y=area, fill=kode_hoved)) + geom_boxplot() + scale_fill_brewer(palette = "Spectral")

#group area by
names(finse_snow_data)
sum_finse_snow_H <- summarize(group_by(finse_snow_data, kode_hoved), area = sum(area))
sum_finse_snow_H[,2] <- round(sum_finse_snow_H[,2])
sum_finse_snow_H[,3] <- round(sum_finse_snow_H[,2]/sum(sum_finse_snow_H[,2]), 4)
colnames(sum_finse_snow_H) <- c("kode_hoved", "area", "percent")
# ggplot(sum_finse_snow, aes(x=kode_hoved, y=area, fill=kode_hoved)) + geom_boxplot() + scale_fill_brewer(palette = "Spectral")

# read in csv with proper labels
htyper <- read.csv2(file = paste0(path,"rel_kode_hoved_5000.csv"))

#join with proper NIN names
colnames(sum_finse_snow_H) <- c("ntype_id", "area", "percent")
sum_finse_snow_H1 <- left_join(sum_finse_snow_H, htyper, by="ntype_id")
sum_finse_snow_H1$ntype_id <- as.factor(sum_finse_snow_H1$ntype_id)
sum_finse_snow_H1 <- as.data.frame(sum_finse_snow_H1)
# hovedtypegrupper ####
# with legend
snow_h1 <- ggplot(sum_finse_snow_H1, aes(x=ntype_full, y=area, fill=ntype_full)) 
snow_h1 <- snow_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_discrete(labels = NULL) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title="FINSE SNOW - Areal dekket av hovedtypegrupper", 
       fill="Hovedtypegruppe",
       x="NiN hovedtypegrupper",
       y=expression ("Areal i"~m^2)) +
   geom_text(aes( label = scales::percent(percent),
                  y= percent ),  vjust = -0.5)
snow_h1
ggsave(filename = "snow_h1.png",plot = snow_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#hovedtypegrupper
# 2 without legend
snow_h2 <- ggplot(sum_finse_snow_H1, aes(x=ntype_full, y=area, fill=ntype_full)) 
snow_h2 <- snow_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=50, hjust=1), 
        legend.position = "none") +
  labs(title="FINSE SNOW - Areal dekket av hovedtypegrupper", 
       fill="Hovedtypegruppe",
       x="NiN hovedtypegrupper",
       y=expression ("Areal i"~m^2)) +
  geom_text(aes( label = scales::percent(percent),
                 y= percent ),  vjust = -0.5)
snow_h2
ggsave(filename = "snow_h2.png",plot = snow_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )
# # As percentage
# g <- ggplot(sum_finse_snow_H, aes(x=kode_hoved))
# g + geom_bar(aes(y=percent, fill=kode_hoved), stat = "count") + scale_fill_brewer(palette = "Spectral") +
#   geom_text(aes( label = scales::percent(percent),
#                  y= percent ), stat= "count", vjust = -.5)+
#   labs(title="Areal dekket av hovedtypegrupper", 
#        fill="Hovedtypegruppe",
#        x="NiN hovedtypegrupper",
#        y=expression ("Areal i"~m^2)) 


#group area by
names(finse_snow_data)
sum_finse_snow <- summarize(group_by(finse_snow_data, kode_hoved, kode_enhet), area = sum(area))
# read in csv with proper labels
path="C:/Users/peterhor/Google Drive/2015 - University of Oslo/GEco/2019 - NiN - Finse SNOW FLUX/"
enhet <- read.csv2(file = paste0(path,"rel_kode_enhet_5000.csv"))
sum_finse_snow$area <- round(sum_finse_snow$area)
sum_finse_snow$percent <- round(sum_finse_snow$area/sum(sum_finse_snow$area), 4)
colnames(sum_finse_snow) <- c("kode_hoved", "enhet_id", "area", "percent")

sum_finse_snow_E <- left_join(sum_finse_snow, enhet, by="enhet_id")
sum_finse_snow_E <- left_join(sum_finse_snow_E, htyper, by="ntype_id")
sum_finse_snow_E$enhet_id <- as.factor(sum_finse_snow_E$enhet_id)
sum_finse_snow_E$ntype_id <- as.factor(sum_finse_snow_E$ntype_id)

# enheter - NiN typer ####
snow_e1 <- ggplot(sum_finse_snow_E, aes(x=enhet_id, y=area, fill=ntype_full)) 
snow_e1 <-snow_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal dekket av NiN typer", 
       fill="Hovedtypegruppe",
       x="NiN typer",
       y=expression ("Areal i"~m^2))
snow_e1
ggsave(filename = "snow_e1.png",plot = snow_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#NiN typer with percentage
snow_e2 <- ggplot(sum_finse_snow_E, aes(x=enhet_id, y=area, fill=ntype_full)) 
snow_e2 <- snow_e2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal dekket av NiN typer", 
       fill="Hovedtypegruppe",
       x="NiN typer",
       y=expression ("Areal i"~m^2)) + 
  geom_text(aes( label = scales::percent(percent), y= percent, digits=2 ),
            angle=90, hjust = -0.5, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
snow_e2
ggsave(filename = "snow_e2.png",plot = snow_e2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#piechart of NiN groups
g <- ggplot(sum_finse_snow, aes(x=kode_hoved, fill=kode_hoved)) + geom_bar() + scale_fill_brewer(palette = "Spectral") 
g + coord_polar #(theta = "y")


# lichen cover ####
lichen_scale <- read.csv2(file = paste0(path,"5000_beskr_1AG-generic.csv"))
names(finse_snow_data)
names(lichen_scale)
colnames(lichen_scale) <- c("lav", "A9_skala")
finse_snow_data_lav <- left_join(finse_snow_data, lichen_scale, by="lav")
sum_finse_snow_lav <- summarize(group_by(finse_snow_data_lav, kode_hoved, lav), area = sum(area))

# showing how much area is covered by the different NIN groups + how much they are covered by lichen
# we also omit L - ferskvannsystemer - because lichen coverage doesn't apply there 
snow_lichen_h1 <- ggplot(finse_snow_data_lav[!finse_snow_data_lav$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
snow_lichen_h1 <-snow_lichen_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  #facet_wrap(~lav)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av hovedtypergrupper fordelt på lavdekning", 
       fill="Lavdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
snow_lichen_h1
ggsave(filename = "snow_lichen_h1.png",plot = snow_lichen_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# showing how much area is covered by the different NIN groups + how much they are covered by lichen
sum_finse_snow_lav <- summarize(group_by(finse_snow_data, kode_hoved, lav), area = sum(area))
sum_finse_snow_lav <- left_join(sum_finse_snow_lav, lichen_scale, by="lav")
snow_lichen_h2 <- ggplot(sum_finse_snow_lav[!sum_finse_snow_lav$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
snow_lichen_h2 <-snow_lichen_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral",direction=-1) +
  #facet_wrap(~lav)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av hovedtypergrupper fordelt på lavdekning", 
       fill="Lavdekning (A9-skala)",
       x="Hovedtypegrupper",
       y= expression ("Areal i"~m^2)) +
  geom_text(aes( label =  round(area, 2), y= area ),
            position = position_stack(vjust = 0.5), 
            check_overlap = TRUE, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
snow_lichen_h2
ggsave(filename = "snow_lichen_h2.png",plot = snow_lichen_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# lichen cover per NiN type
snow_lichen_e1 <- ggplot(finse_snow_data_lav[!finse_snow_data_lav$kode_hoved %in% c("L","T39"),], aes(x=kode_enhet, y=area, fill=A9_skala)) 
snow_lichen_e1 <-snow_lichen_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  facet_wrap(~kode_hoved, scales = "free")+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av NiN typer fordelt på lavdekning", 
       fill="Lavdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
snow_lichen_e1
ggsave(filename = "snow_lichen_e1.png",plot = snow_lichen_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# boulder  cover ####
block_scale <- read.csv2(file = paste0(path,"5000_beskr_1AG-generic.csv"))
names(finse_snow_data)
names(block_scale)
colnames(block_scale) <- c("blokkmark", "A9_skala")
finse_snow_data_blokkmark <- left_join(finse_snow_data, block_scale, by="blokkmark")
sum_finse_snow_blokkmark <- summarize(group_by(finse_snow_data_blokkmark, kode_hoved, blokkmark), area = sum(area))

# showing how much area is covered by the different NIN groups + how much they are covered by block
# we also omit L - ferskvannsystemer - because block coverage doesn't apply there 
snow_block_h1 <- ggplot(finse_snow_data_blokkmark[!finse_snow_data_blokkmark$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
snow_block_h1 <-snow_block_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  #facet_wrap(~blokkmark)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av hovedtypergrupper fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
snow_block_h1
ggsave(filename = "snow_block_h1.png",plot = snow_block_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# showing how much area is covered by the different NIN groups + how much they are covered by block
sum_finse_snow_blokkmark <- summarize(group_by(finse_snow_data, kode_hoved, blokkmark), area = sum(area))
sum_finse_snow_blokkmark <- left_join(sum_finse_snow_blokkmark, block_scale, by="blokkmark")
snow_block_h2 <- ggplot(sum_finse_snow_blokkmark[sum_finse_snow_blokkmark$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
snow_block_h2 <-snow_block_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral",direction=-1) +
  #facet_wrap(~blokkmark)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av hovedtypergrupper fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y= expression ("Areal i"~m^2)) +
  geom_text(aes( label =  round(area, 2), y= area ),
            position = position_stack(vjust = 0.5), 
            check_overlap = TRUE, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
snow_block_h2
ggsave(filename = "snow_block_h2.png",plot = snow_block_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# block cover per NiN type
snow_block_e1 <- ggplot(finse_snow_data_blokkmark[!finse_snow_data_blokkmark$kode_hoved %in% c("L","T39"),], aes(x=kode_enhet, y=area, fill=A9_skala)) 
snow_block_e1 <-snow_block_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  facet_wrap(~kode_hoved, scales = "free")+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av NiN typer fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
snow_block_e1
ggsave(filename = "snow_block_e1.png",plot = snow_block_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#  shrub cover ####
busk_scale <- read.csv2(file = paste0(path,"5000_beskr_1AG-generic.csv"))
names(finse_snow_data)
names(busk_scale)
colnames(busk_scale) <- c("busk", "A9_skala")
finse_snow_data_busk <- left_join(finse_snow_data, busk_scale, by="busk")
sum_finse_snow_busk <- summarize(group_by(finse_snow_data_busk, kode_hoved, busk), area = sum(area))

# showing how much area is covered by the different NIN groups + how much they are covered by busk
# we also omit L - ferskvannsystemer - because busk coverage doesn't apply there 
snow_busk_h1 <- ggplot(finse_snow_data_busk[!finse_snow_data_busk$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
snow_busk_h1 <-snow_busk_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  #facet_wrap(~busk)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av hovedtypergrupper fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
snow_busk_h1
ggsave(filename = "snow_busk_h1.png",plot = snow_busk_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# showing how much area is covered by the different NIN groups + how much they are covered by busk
sum_finse_snow_busk <- summarize(group_by(finse_snow_data, kode_hoved, busk), area = sum(area))
sum_finse_snow_busk <- left_join(sum_finse_snow_busk, busk_scale, by="busk")
snow_busk_h2 <- ggplot(sum_finse_snow_busk[!sum_finse_snow_busk$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
snow_busk_h2 <-snow_busk_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral",direction=-1) +
  #facet_wrap(~busk)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av hovedtypergrupper fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y= expression ("Areal i"~m^2)) +
  geom_text(aes( label =  round(area, 2), y= area ),
            position = position_stack(vjust = 0.5), 
            check_overlap = TRUE, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
snow_busk_h2
ggsave(filename = "snow_busk_h2.png",plot = snow_busk_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# busk cover per NiN type
snow_busk_e1 <- ggplot(finse_snow_data_busk[!finse_snow_data_busk$kode_hoved %in% c("L","T39"),], aes(x=kode_enhet, y=area, fill=A9_skala)) 
snow_busk_e1 <-snow_busk_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  facet_wrap(~kode_hoved, scales = "free")+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE SNOW - Areal av NiN typer fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
snow_busk_e1
ggsave(filename = "snow_busk_e1.png",plot = snow_busk_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#### FINSE FLUX ####
finse_flux <- readOGR(dsn = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Finse - NiN kartlegging/VECTOR_5000", "Finse-FLUX_2017_merged_PH_ANB")
#finse_flux <- readOGR(dsn = "D:/Finse - NiN kartlegging_SNOW_FLUX", "Finse-FLUX_2017_merged_PH_ANB")
plot(finse_flux)
plot(finse_flux$kode_enhet)

names(finse_flux@data)

finse_flux_data <- finse_flux@data[,c(3,5,17,47,48,51)]
names(finse_flux_data) <- c("kode_hoved", "kode_enhet", "busk", "lav", "blokkmark", "area"  )
str(finse_flux_data)
FLUX_kode_enhet_sums <- aggregate(finse_flux_data$area,list(finse_flux_data$kode_enhet),sum)
FLUX_kode_enhet_sums[,3] <- round(FLUX_kode_enhet_sums[,2]/sum(FLUX_kode_enhet_sums[,2])*100, 4)
colnames(FLUX_kode_enhet_sums) <- c("kode_enhet", "area", "percent")
write.csv2(FLUX_kode_enhet_sums, file = paste0(path,"tables/", "FLUX_e.csv"))

FLUX_kode_hoved_sums <- aggregate(finse_flux_data$area,list(finse_flux_data$kode_hoved),sum)
FLUX_kode_hoved_sums[,3] <- round(FLUX_kode_hoved_sums[,2]/sum(FLUX_kode_hoved_sums[,2])*100, 4)
colnames(FLUX_kode_hoved_sums) <- c("kode_hoved", "area", "percent")
write.csv2(FLUX_kode_hoved_sums, file = paste0(path,"tables/", "FLUX_h.csv"))


tapply(finse_flux_data$area,list(finse_flux_data$kode_hoved),FUN=sum)

# add to data a new column termed "id" composed of the rownames of data
finse_flux@data$id <- rownames(finse_flux@data)
proj4string(finse_flux)
# create a data.frame from our spatial object
finse_flux_df <-fortify(finse_flux)

map <- ggplot() +
  geom_path(data = finse_flux_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2)
print(map) 

#ploting graphs
#area vs nin type or main group
ggplot(finse_flux_data, aes(x=kode_enhet, y=area, fill=kode_hoved)) + geom_boxplot() + scale_fill_brewer(palette = "Spectral")

ggplot(finse_flux_data, aes(x=kode_hoved, y=area, fill=kode_hoved)) + geom_boxplot() + scale_fill_brewer(palette = "Spectral")

#group area by
names(finse_flux_data)
sum_finse_flux_H <- summarize(group_by(finse_flux_data, kode_hoved), area = sum(area))
sum_finse_flux_H[,2] <- round(sum_finse_flux_H[,2])
sum_finse_flux_H[,3] <- round(sum_finse_flux_H[,2]/sum(sum_finse_flux_H[,2]), 4)
colnames(sum_finse_flux_H) <- c("kode_hoved", "area", "percent")
# ggplot(sum_finse_flux, aes(x=kode_hoved, y=area, fill=kode_hoved)) + geom_boxplot() + scale_fill_brewer(palette = "Spectral")

# read in csv with proper labels
path="C:/Users/peterhor/Google Drive/2015 - University of Oslo/GEco/2019 - NiN - Finse SNOW FLUX/"
htyper <- read.csv2(file = paste0(path,"rel_kode_hoved_5000.csv"))
library(dplyr)
#join with proper NIN names
colnames(sum_finse_flux_H) <- c("ntype_id", "area", "percent")
sum_finse_flux_H1 <- left_join(sum_finse_flux_H, htyper, by="ntype_id")
sum_finse_flux_H1$ntype_id <- as.factor(sum_finse_flux_H1$ntype_id)
sum_finse_flux_H1 <- as.data.frame(sum_finse_flux_H1)
# hovedtypegrupper ####
# with legend
#specify colors for 13 NiN types
palette_paired_13 <- c('#a6cee3',
                        '#1f78b4',
                        '#b2df8a',
                        '#33a02c',
                        '#fb9a99',
                        '#e31a1c',
                        '#fdbf6f',
                        '#ff7f00',
                        '#cab2d6',
                        '#6a3d9a',
                        '#ffff99',
                        '#b15928',
                        '#999999')

flux_h1 <- ggplot(sum_finse_flux_H1, aes(x=ntype_full, y=area, fill=ntype_full)) 
flux_h1 <-  flux_h1 + geom_bar(position = "stack",stat = "identity") + 
  scale_fill_manual(values = palette_paired_13) +#palette = "Paired") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_discrete(labels = NULL) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title="FINSE FLUX - Areal dekket av hovedtypegrupper", 
       fill="Hovedtypegruppe",
       x="NiN hovedtypegrupper",
       y=expression ("Areal i"~m^2)) +
  geom_text(aes( label = scales::percent(percent),
                 y= percent ),  angle=90, hjust = -0.5, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
flux_h1
ggsave(filename = "flux_h1.png",plot = flux_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#hovedtypegrupper
# 2 without legend
flux_h2 <- ggplot(sum_finse_flux_H1, aes(x=ntype_full, y=area, fill=ntype_full)) 
flux_h2 <- flux_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_manual(values = palette_paired_13) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=50, hjust=1), 
        legend.position = "none") +
  labs(title="FINSE FLUX - Areal dekket av hovedtypegrupper", 
       fill="Hovedtypegruppe",
       x="NiN hovedtypegrupper",
       y=expression ("Areal i"~m^2)) +
  geom_text(aes( label = scales::percent(percent),
                 y= percent ),  vjust = -0.5)
flux_h2
ggsave(filename = "flux_h2.png",plot = flux_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )
# # As percentage
# g <- ggplot(sum_finse_flux_H, aes(x=kode_hoved))
# g + geom_bar(aes(y=percent, fill=kode_hoved), stat = "count") + scale_fill_brewer(palette = "Spectral") +
#   geom_text(aes( label = scales::percent(percent),
#                  y= percent ), stat= "count", vjust = -.5)+
#   labs(title="Areal dekket av hovedtypegrupper", 
#        fill="Hovedtypegruppe",
#        x="NiN hovedtypegrupper",
#        y=expression ("Areal i"~m^2)) 


#group area by
names(finse_flux_data)
sum_finse_flux <- summarize(group_by(finse_flux_data, kode_hoved, kode_enhet), area = sum(area))
# read in csv with proper labels
path="C:/Users/peterhor/Google Drive/2015 - University of Oslo/GEco/2019 - NiN - Finse SNOW FLUX/"
enhet <- read.csv2(file = paste0(path,"rel_kode_enhet_5000.csv"))
sum_finse_flux$area <- round(sum_finse_flux$area)
sum_finse_flux$percent <- round(sum_finse_flux$area/sum(sum_finse_flux$area), 4)
colnames(sum_finse_flux) <- c("kode_hoved", "enhet_id", "area", "percent")

sum_finse_flux_E <- left_join(sum_finse_flux, enhet, by="enhet_id")
sum_finse_flux_E <- left_join(sum_finse_flux_E, htyper, by="ntype_id")
sum_finse_flux_E$enhet_id <- as.factor(sum_finse_flux_E$enhet_id)
sum_finse_flux_E$ntype_id <- as.factor(sum_finse_flux_E$ntype_id)

# enheter - NiN typer ####
flux_e1 <- ggplot(sum_finse_flux_E, aes(x=enhet_id, y=area, fill=ntype_full)) 
flux_e1 <- flux_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_manual(values = palette_paired_13) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal dekket av NiN typer", 
       fill="Hovedtypegruppe",
       x="NiN typer",
       y=expression ("Areal i"~m^2))
flux_e1
ggsave(filename = "flux_e1.png",plot = flux_e1, width = 15, height = 7, 
       device = "png", path = paste0(path, "ggplots/"),dpi = 300 )

#NiN typer with percentage
flux_e2 <- ggplot(sum_finse_flux_E, aes(x=enhet_id, y=area, fill=ntype_full)) 
flux_e2 <- flux_e2 + geom_bar(position = "stack",stat = "identity") + scale_fill_manual(values = palette_paired_13) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal dekket av NiN typer", 
       fill="Hovedtypegruppe",
       x="NiN typer",
       y=expression ("Areal i"~m^2)) + 
  geom_text(aes( label = scales::percent(percent), y= percent, digits=2 ),
            angle=90, hjust = -0.5, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
flux_e2
ggsave(filename = "flux_e2.png",plot = flux_e2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# lichen cover ####
lichen_scale <- read.csv2(file = paste0(path,"5000_beskr_1AG-generic.csv"))
names(finse_flux_data)
names(lichen_scale)
colnames(lichen_scale) <- c("lav", "A9_skala")
finse_flux_data_lav <- left_join(finse_flux_data, lichen_scale, by="lav")
sum_finse_flux_lav <- summarize(group_by(finse_flux_data_lav, kode_hoved, lav), area = sum(area))

# showing how much area is covered by the different NIN groups + how much they are covered by lichen
# we also omit L - ferskvannsystemer - because lichen coverage doesn't apply there 
flux_lichen_h1 <- ggplot(finse_flux_data_lav[!finse_flux_data_lav$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
flux_lichen_h1 <-flux_lichen_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  #facet_wrap(~lav)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal av hovedtypergrupper fordelt på lavdekning", 
       fill="Lavdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
flux_lichen_h1
ggsave(filename = "flux_lichen_h1.png",plot = flux_lichen_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# showing how much area is covered by the different NIN groups + how much they are covered by lichen
sum_finse_flux_lav <- summarize(group_by(finse_flux_data, kode_hoved, lav), area = sum(area))
sum_finse_flux_lav <- left_join(sum_finse_flux_lav, lichen_scale, by="lav")
flux_lichen_h2 <- ggplot(sum_finse_flux_lav[!sum_finse_flux_lav$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
flux_lichen_h2 <-flux_lichen_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral",direction=-1) +
  #facet_wrap(~lav)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal av hovedtypergrupper fordelt på lavdekning", 
       fill="Lavdekning (A9-skala)",
       x="Hovedtypegrupper",
       y= expression ("Areal i"~m^2)) +
  geom_text(aes( label =  round(area, 2), y= area ),
            position = position_stack(vjust = 0.5), 
            check_overlap = TRUE, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
flux_lichen_h2
ggsave(filename = "flux_lichen_h2.png",plot = flux_lichen_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# lichen cover per NiN type
flux_lichen_e1 <- ggplot(finse_flux_data_lav[!finse_flux_data_lav$kode_hoved %in% c("L","T39"),], aes(x=kode_enhet, y=area, fill=A9_skala)) 
flux_lichen_e1 <-flux_lichen_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  facet_wrap(~kode_hoved, scales = "free")+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal av NiN typer fordelt på lavdekning", 
       fill="Lavdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
flux_lichen_e1
ggsave(filename = "flux_lichen_e1.png",plot = flux_lichen_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )


# boulder  cover ####
block_scale <- read.csv2(file = paste0(path,"5000_beskr_1AG-generic.csv"))
names(finse_flux_data)
names(block_scale)
colnames(block_scale) <- c("blokkmark", "A9_skala")
finse_flux_data_blokkmark <- left_join(finse_flux_data, block_scale, by="blokkmark")
sum_finse_flux_blokkmark <- summarize(group_by(finse_flux_data_blokkmark, kode_hoved, blokkmark), area = sum(area))

# showing how much area is covered by the different NIN groups + how much they are covered by block
# we also omit L - ferskvannsystemer - because block coverage doesn't apply there 
flux_block_h1 <- ggplot(finse_flux_data_blokkmark[!finse_flux_data_blokkmark$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
flux_block_h1 <-flux_block_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  #facet_wrap(~blokkmark)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal av hovedtypergrupper fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
flux_block_h1
ggsave(filename = "flux_block_h1.png",plot = flux_block_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# showing how much area is covered by the different NIN groups + how much they are covered by block
sum_finse_flux_blokkmark <- summarize(group_by(finse_flux_data, kode_hoved, blokkmark), area = sum(area))
sum_finse_flux_blokkmark <- left_join(sum_finse_flux_blokkmark, block_scale, by="blokkmark")
flux_block_h2 <- ggplot(sum_finse_flux_blokkmark[!sum_finse_flux_blokkmark$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
flux_block_h2 <-flux_block_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral",direction=-1) +
  #facet_wrap(~blokkmark)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal av hovedtypergrupper fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y= expression ("Areal i"~m^2)) +
  geom_text(aes( label =  round(area, 2), y= area ),
            position = position_stack(vjust = 0.5), 
            check_overlap = TRUE, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
flux_block_h2
ggsave(filename = "flux_block_h2.png",plot = flux_block_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# NiN types and boulder cover
flux_block_e1 <- ggplot(finse_flux_data_blokkmark[!finse_flux_data_blokkmark$kode_hoved %in% c("L","T39"),], aes(x=kode_enhet, y=area, fill=A9_skala)) 
flux_block_e1 <-flux_block_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  facet_wrap(~kode_hoved, scales = "free")+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE FLUX - Areal av NiN typer fordelt på blokkdekning", 
       fill="Blokkdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
flux_block_e1
ggsave(filename = "flux_block_e1.png",plot = flux_block_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

#  shrub cover ####
busk_scale <- read.csv2(file = paste0(path,"5000_beskr_1AG-generic.csv"))
names(finse_flux_data)
names(busk_scale)
colnames(busk_scale) <- c("busk", "A9_skala")
finse_flux_data_busk <- left_join(finse_flux_data, busk_scale, by="busk")
sum_finse_flux_busk <- summarize(group_by(finse_flux_data_busk, kode_hoved, busk), area = sum(area))

# showing how much area is covered by the different NIN groups + how much they are covered by busk
# we also omit L - ferskvannsystemer - because busk coverage doesn't apply there 
flux_busk_h1 <- ggplot(finse_flux_data_busk[!finse_flux_data_busk$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
flux_busk_h1 <-flux_busk_h1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  #facet_wrap(~busk)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE flux - Areal av hovedtypergrupper fordelt på busksjiktdekning", 
       fill="busksjiktdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
flux_busk_h1
ggsave(filename = "flux_busk_h1.png",plot = flux_busk_h1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# showing how much area is covered by the different NIN groups + how much they are covered by busk
sum_finse_flux_busk <- summarize(group_by(finse_flux_data, kode_hoved, busk), area = sum(area))
sum_finse_flux_busk <- left_join(sum_finse_flux_busk, busk_scale, by="busk")
flux_busk_h2 <- ggplot(sum_finse_flux_busk[!sum_finse_flux_busk$kode_hoved %in% c("L","T39"),], aes(x=kode_hoved, y=area, fill=A9_skala)) 
flux_busk_h2 <-flux_busk_h2 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral",direction=-1) +
  #facet_wrap(~busk)+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE flux - Areal av hovedtypergrupper fordelt på busksjiktdekning", 
       fill="Busksjiktdekning (A9-skala)",
       x="Hovedtypegrupper",
       y= expression ("Areal i"~m^2)) +
  geom_text(aes( label =  round(area, 2), y= area ),
            position = position_stack(vjust = 0.5), 
            check_overlap = TRUE, size=3, 
            color=rgb(100,100,100, maxColorValue=255))
flux_busk_h2
ggsave(filename = "flux_busk_h2.png",plot = flux_busk_h2, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )

# busk cover per NiN type
flux_busk_e1 <- ggplot(finse_flux_data_busk[!finse_flux_data_busk$kode_hoved %in% c("L","T39"),], aes(x=kode_enhet, y=area, fill=A9_skala)) 
flux_busk_e1 <-flux_busk_e1 + geom_bar(position = "stack",stat = "identity") + scale_fill_brewer(palette = "Spectral", direction=-1) +
  facet_wrap(~kode_hoved, scales = "free")+
  scale_y_continuous(labels = scales::comma_format()) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5)) + 
  labs(title="FINSE flux - Areal av NiN typer fordelt på busksjiktdekning", 
       fill="Busksjiktdekning (A9-skala)",
       x="Hovedtypegrupper",
       y=expression ("Areal i"~m^2))
flux_busk_e1
ggsave(filename = "flux_busk_e1.png",plot = flux_busk_e1, width = 15, height = 7, device = "png", path = paste0(path, "/ggplots/"),dpi = 300 )
