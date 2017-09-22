#Install package
#install.packages("deldir", dependencies = TRUE)

#Para instalar rgdal
#install.packages("rgdal", repos = "http://cran.us.r-project.org", type = "source")

#library(devtools)
#devtools::install_github("diegovalle/mxmaps")

#library(leaflet)
library(deldir)
library(tidyverse)
library(rgdal)
#library(mxmaps)
library(plotly)
library(latticeExtra)
library(ggforce)
library(plyr)
library(dplyr)

search()

getwd()
setwd('~/Dropbox/JANO/2017/Conabio/Mercados_Mexico/AllData/')
#source("lib/latlong2state.R")
getwd()
dir()

#library(maptools)

#Cargar mapa de Mexico
mapregio <- readOGR("data/destdv250k_2gw/destdv250k_2gw.shp")
shapefile_df <- fortify(mapregio)


head(shapefile_df)
str(shapefile_df)
target <- c("1.1", "0.1", "783.1", "90.1", "799.1", "867.1", "1030.1",
            "1132.1","185.1","1041.1","846.1", "474.1", "152.1", "191.1",
            "898.1","778.1", "952.1", "876.1", "878.1", "865.1", "676.1",
            "805.1", "824.1", "818.1", "16.1", "137.1", "918.1", "945.1",
            "885.1", "819.1", "967.1", "1077.1", "7.1")
shapefile_df <- shapefile_df %>%
  filter(group %in% target) %>%
  mutate(group = revalue(group,c("1.1" = "Sonora"))) %>%
  mutate(group = revalue(group,c("0.1" = "Baja California"))) %>%
  mutate(group = revalue(group,c("783.1" = "Jalisco"))) %>%
  mutate(group = revalue(group,c("90.1" = "Baja California Sur"))) %>%
  mutate(group = revalue(group,c("799.1" = "Veracruz"))) %>%
  mutate(group = revalue(group,c("867.1" = "Puebla"))) %>%
  mutate(group = revalue(group,c("1030.1" = "Oaxaca"))) %>%
  mutate(group = revalue(group,c("1132.1" = "Chiapas"))) %>%
  mutate(group = revalue(group,c("185.1" = "Sinaloa"))) %>%
  mutate(group = revalue(group,c("1041.1" = "Tabasco"))) %>%
  mutate(group = revalue(group,c("846.1" = "Hidalgo"))) %>%
  mutate(group = revalue(group,c("474.1" = "Zacatecas"))) %>%
  mutate(group = revalue(group,c("152.1" = "Tamaulipas"))) %>%
  mutate(group = revalue(group,c("967.1" = "Veracruz"))) %>%
  mutate(group = revalue(group,c("191.1" = "Durango"))) %>%
  mutate(group = revalue(group,c("898.1" = "Distrito Federal"))) %>%
  mutate(group = revalue(group,c("778.1" = "Nayarit"))) %>%
  mutate(group = revalue(group,c("952.1" = "Guerrero"))) %>%
  mutate(group = revalue(group,c("876.1" = "Michoacan"))) %>%
  mutate(group = revalue(group,c("1077.1" = "Tabasco"))) %>%
  mutate(group = revalue(group,c("878.1" = "Mexico"))) %>%
  mutate(group = revalue(group,c("865.1" = "Campeche"))) %>%
  mutate(group = revalue(group,c("7.1" = "Chihuahua"))) %>%
  mutate(group = revalue(group,c("676.1" = "San Luis Potosi"))) %>%
  mutate(group = revalue(group,c("805.1" = "Guanajuato"))) %>%
  mutate(group = revalue(group,c("824.1" = "Quintana Roo"))) %>%
  mutate(group = revalue(group,c("818.1" = "Queretaro"))) %>%
  mutate(group = revalue(group,c("16.1" = "Coahuila"))) %>%
  mutate(group = revalue(group,c("137.1" = "Nuevo Leon"))) %>%
  mutate(group = revalue(group,c("918.1" = "Colima"))) %>%
  mutate(group = revalue(group,c("945.1" = "Morelos"))) %>%
  mutate(group = revalue(group,c("885.1" = "Tlaxcala"))) %>%
  mutate(group = revalue(group,c("819.1" = "Yucatan")))
  #mutate(group = revalue(group,c("" = ""))) %>%
  

Mex <- read.delim("Z_NACIONAL.csv"  , header = T, sep = ",")
dim(Mex)
head(Mex)[1:14]
names(Mex)
names(Mex)[7:8] <- c("lng", "lat")
names(Mex)[7:8]


target1 <- c("Distrito Federal", "Mexico")

Mex2 <- Mex %>%
  select(CVE_EDO, CVE_MUN, CVE_LOC, lng, lat, NOM_MUN, NOM_LOC, NOM_ENT) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Veracruz de Ignacio de la Llave" = "Veracruz"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Coahuila de Zaragoza" = "Coahuila"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Michoacán de Ocampo" = "Michoacan"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("México" = "Mexico"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Querétaro" = "Queretaro"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Nuevo León" = "Nuevo Leon"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("San Luis Potosí" = "San Luis Potosí"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Yucatán" = "Yucatan"))) %>%
  filter(NOM_ENT %in% target1 ) %>%
  #filter(Altitud < 5000) %>%
  #select(Raza_primaria, Longitud, Latitud, Altitud) %>%
  na.omit() %>%
  distinct() %>%
  as.data.frame()
Mex2$CVE_EDO <- sprintf("%02d", Mex2$CVE_EDO)
Mex2$CVE_MUN <- sprintf("%03d", Mex2$CVE_MUN)
Mex2$CVE_LOC <- sprintf("%04d", Mex2$CVE_LOC)

Mex2$CVE_EDO <- as.factor(Mex2$CVE_EDO)
Mex2$CVE_MUN <- as.factor(Mex2$CVE_MUN)
Mex2$CVE_LOC <- as.factor(Mex2$CVE_LOC)

summary(Mex2)

Mex2 <- Mex2 %>%
  mutate(FullCode = paste(CVE_EDO,CVE_MUN,CVE_LOC, sep = ""))

dim(Mex2)
head(Mex2)



dir()

Edo01 <- read.delim("RESLOC2014 - 01 Aguascalientes.csv"  , header = T, sep = ",")
Edo02 <- read.delim("RESLOC2014 - 02 Baja California.csv"  , header = T, sep = ",")
Edo03 <- read.delim("RESLOC2014 - 03 Baja California Sur.csv"  , header = T, sep = ",")
Edo04 <- read.delim("RESLOC2014 - 04 Campeche.csv"  , header = T, sep = ",")
Edo05 <- read.delim("RESLOC2014 - 05 Coahuila.csv"  , header = T, sep = ",")
Edo06 <- read.delim("RESLOC2014 - 06 Colima.csv"  , header = T, sep = ",")
Edo07 <- read.delim("RESLOC2014 - 07 Chiapas.csv"  , header = T, sep = ",")
Edo08 <- read.delim("RESLOC2014 - 08 Chihuahua.csv"  , header = T, sep = ",")
Edo09 <- read.delim("RESLOC2014 - 09 Distrito Federal.csv"  , header = T, sep = ",")
Edo10 <- read.delim("RESLOC2014 - 10 Durango.csv"  , header = T, sep = ",")
Edo11 <- read.delim("RESLOC2014 - 11 Guanajuato.csv"  , header = T, sep = ",")
Edo12 <- read.delim("RESLOC2014 - 12 Guerrero.csv"  , header = T, sep = ",")
Edo13 <- read.delim("RESLOC2014 - 13 Hidalgo.csv"  , header = T, sep = ",")
Edo14 <- read.delim("RESLOC2014 - 14 Jalisco.csv"  , header = T, sep = ",")
Edo15 <- read.delim("RESLOC2014 - 15 Mexico.csv"  , header = T, sep = ",")
Edo16 <- read.delim("RESLOC2014 - 16 Michoacan.csv"  , header = T, sep = ",")
Edo17 <- read.delim("RESLOC2014 - 17 Morelos.csv"  , header = T, sep = ",")
Edo18 <- read.delim("RESLOC2014 - 18 Nayarit.csv"  , header = T, sep = ",")
Edo19 <- read.delim("RESLOC2014 - 19 Nuevo Leon.csv"  , header = T, sep = ",")
Edo20 <- read.delim("RESLOC2014 - 20 Oaxaca.csv"  , header = T, sep = ",")
Edo21 <- read.delim("RESLOC2014 - 21 Puebla.csv"  , header = T, sep = ",")
Edo22 <- read.delim("RESLOC2014 - 22 Queretaro.csv"  , header = T, sep = ",")
Edo23 <- read.delim("RESLOC2014 - 23 Quintana Roo.csv"  , header = T, sep = ",")
Edo24 <- read.delim("RESLOC2014 - 24 San Luis Potosi.csv"  , header = T, sep = ",")
Edo25 <- read.delim("RESLOC2014 - 25 Sinaloa.csv"  , header = T, sep = ",")
Edo26 <- read.delim("RESLOC2014 - 26 Sonora.csv"  , header = T, sep = ",")
Edo27 <- read.delim("RESLOC2014 - 27 Tabasco.csv"  , header = T, sep = ",")
Edo28 <- read.delim("RESLOC2014 - 28 Tamaulipas.csv"  , header = T, sep = ",")
Edo29 <- read.delim("RESLOC2014 - 29 Tlaxcala.csv"  , header = T, sep = ",")
Edo30 <- read.delim("RESLOC2014 - 30 Veracruz.csv"  , header = T, sep = ",")
Edo31 <- read.delim("RESLOC2014 - 31 Yucatan.csv"  , header = T, sep = ",")
Edo32 <- read.delim("RESLOC2014 - 32 Zacatecas.csv"  , header = T, sep = ",")


Edo <- bind_rows(Edo09, Edo15)
summary(Edo)
names(Edo)

summary(Edo$Nombre.de.la.entidad)

head(Edo)
names(Edo)

Edo100 <- Edo %>%
  select(Clave.de.entidad.federativa,
         Clave.de.municipio.o.delegación,
         Clave.de.localidad,
         Nombre.de.la.entidad,
         Disponibilidad.de.tianguis.o.mercado.sobre.ruedas) %>%
  filter(Disponibilidad.de.tianguis.o.mercado.sobre.ruedas == "Dispone" ) %>%
  na.omit() %>%
  distinct()

Edo100  

dim(Edo100)
names(Edo100) <- c("CVE_EDO", "CVE_MUN", "CVE_LOC","Nombre","Tianguis")
names(Edo100)
head(Edo100)
Edo100$CVE_EDO <- sprintf("%02d", Edo100$CVE_EDO)
Edo100$CVE_MUN <- sprintf("%03d", Edo100$CVE_MUN)
Edo100$CVE_LOC <- sprintf("%04d", Edo100$CVE_LOC)

Edo100$CVE_EDO <- as.factor(Edo100$CVE_EDO)
Edo100$CVE_MUN <- as.factor(Edo100$CVE_MUN)
Edo100$CVE_LOC <- as.factor(Edo100$CVE_LOC)


head(Edo100)
Edo100 <- Edo100 %>%
  mutate(FullCode = paste(CVE_EDO,CVE_MUN,CVE_LOC, sep = "")) %>%
  select(Tianguis, FullCode)

head(Mex2)
head(Edo100)

TianguisFF <- inner_join(Edo100,Mex2, by = "FullCode")
dim(TianguisFF)

head(TianguisFF)
names(TianguisFF)
vtess <- deldir(TianguisFF[,6:7])
class(vtess)
summary(vtess)
dim(as.data.frame(vtess$summary$dir.area))

summary(vtess$delsgs)
summary(vtess$dirsgs)
summary(vtess$ind.orig)


# Voronoi

p <- ggplot(TianguisFF, aes(lng, lat))
p + geom_point() + theme_bw()

head(TianguisFF)
ggplot(TianguisFF, aes(lng, lat)) +
  geom_voronoi_tile(aes(fill = vtess$summary$dir.area)) +
  geom_voronoi_segment() 

names(vtess$summary)
FF <- vtess$summary$dir.area
LL <- quantile(vtess$summary$dir.area)

#heat.colors(20) # Or topo.colors(20), or terrain.colors(20)
# or cm.colors(20), or rainbow(20).
#ccc <- sample(topo.colors(20),4)
ccc <- c("#FF0000","#47d160","#47d160","#86b8e8")

FF1 <- FF %>%
  replace(FF >= LL[1] & FF <= LL[2], ccc[1]) %>%
  replace(FF >= LL[2] & FF <= LL[3], ccc[2]) %>%
  replace(FF >= LL[3] & FF <= LL[4], ccc[3]) %>%
  replace(FF >= LL[4] & FF <= LL[5], ccc[4])

FF1


ggplot(TianguisFF, aes(lng, lat)) +
  geom_voronoi_tile(aes(fill = FF1)) +
  geom_voronoi_segment() 


#vtess1 <- tile.list(vtess)
#plot(vtess1)
#plot(vtess1,fillcol = FF1,close = T, number = F, cex = 0.3)


#Now we can make a plot
head(TianguisFF)
summary(TianguisFF)
names(Maiz1)
ggplot(data = TianguisFF, aes(x = lng,y = lat)) +
  geom_point() + 
  theme_bw() +
  #Plot the voronoi lines
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 0.2,
    data = vtess$dirsgs,
    linetype = 1,
    color = "black") 
  #Plot the points
  
#head(Maiz1)
#dim(Maiz1)

#head(vtess$summary)
#dim(vtess$summary)
#dim(Maiz1)
#MaizZZ <- data.frame(Maiz1[,3:4], vtess$summary$dir.area)
#tileplot(z1~x*y, MaizZZ)

head(TianguisFF)

head(shapefile_df)
summary(shapefile_df)

summary(shapefile_df$group)
#shapefile_df1 <- shapefile_df[shapefile_df$group == "1.1",]

shapefile_df1 <- shapefile_df
  
shapefile_df1 <- shapefile_df %>%
  filter(group %in% target1)

shapefile_df1

dim(vtess$dirsgs)
dim(TianguisFF)
map <- ggplot(data = TianguisFF, aes(x = lng,y = lat)) +
  geom_point() + 
  #geom_point(aes(colour = Raza_primaria)) + 
  #geom_point(aes(colour = Altitud)) + 
  theme_bw() +
  #Plot the voronoi lines
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               size = 0.2,
               data = vtess$dirsgs,
               linetype = 1,
               color = "#080530") + 
  geom_path(data = shapefile_df1, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = 0.2) + 
  theme_bw()

print(map)

#ggplotly(map)

head(TianguisFF)
ggplot(TianguisFF, aes(lng, lat)) +
  geom_voronoi_tile(aes(fill = FF1)) +
  geom_voronoi_segment(color = "gray") +
  geom_path(data = shapefile_df1, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = 0.2) + 
  geom_point(size = 0.2) +
  labs(title = "Tianguis ", x = "Longitud", y = "Latitud") +
  scale_fill_discrete(name = "Diferentes\nNiveles",
                      breaks = c(levels(as.factor(ccc))),
                      labels = c("Medio", "Lejos", "Cerca"))



#####################
#####################

names(Edo)
EdoHH <- Edo %>%
  select(Clave.de.entidad.federativa,
         Clave.de.municipio.o.delegación,
         Clave.de.localidad,
         Nombre.de.la.entidad,
         Abasto.de.maíz) %>%
  filter(Abasto.de.maíz == "Dispone" ) %>%
  na.omit() %>%
  distinct()
head(EdoHH)

EdoHH  

dim(EdoHH)
names(EdoHH) <- c("CVE_EDO", "CVE_MUN", "CVE_LOC","Nombre","Tianguis")
names(EdoHH)
head(EdoHH)
EdoHH$CVE_EDO <- sprintf("%02d", EdoHH$CVE_EDO)
EdoHH$CVE_MUN <- sprintf("%03d", EdoHH$CVE_MUN)
EdoHH$CVE_LOC <- sprintf("%04d", EdoHH$CVE_LOC)

EdoHH$CVE_EDO <- as.factor(EdoHH$CVE_EDO)
EdoHH$CVE_MUN <- as.factor(EdoHH$CVE_MUN)
EdoHH$CVE_LOC <- as.factor(EdoHH$CVE_LOC)


head(EdoHH)
EdoHH <- EdoHH %>%
  mutate(FullCode = paste(CVE_EDO,CVE_MUN,CVE_LOC, sep = "")) %>%
  select(Tianguis, FullCode)

head(Mex2)
head(EdoHH)

TianguisHH <- inner_join(EdoHH,Mex2, by = "FullCode")
dim(TianguisHH)


ggplot(TianguisFF, aes(lng, lat)) +
  geom_voronoi_tile(aes(fill = FF1)) +
  geom_voronoi_segment(color = "gray") +
  geom_path(data = shapefile_df1, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = 0.2) + 
  labs(title = "Tianguis ", x = "Longitud", y = "Latitud") +
  scale_fill_discrete(name = "Diferentes\nNiveles",
                      breaks = c(levels(as.factor(ccc))),
                      labels = c("Medio", "Lejos", "Cerca")) +
  geom_point(data = TianguisHH, aes(lng,lat), color = "black", size = 0.5) + 
  geom_point(size = 2, color = "red", shape = 21)
  


#ggplot() +
#  geom_map(data = vor_df, map = vor_df,aes(x = long, y = lat, map_id = id, fill = id), size=0.25) +
#  geom_path(data = vor_df,aes(x = long, y = lat, map = id)) +
#  geom_point(data = dati, aes(x, y),shape = 21, color = "white", fill = "steelblue")





