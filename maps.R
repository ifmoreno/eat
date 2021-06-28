install.packages("sf")
library(tidyverse)
library(sf)
library(RColorBrewer)
library(viridis)

zip_mun <- "https://geoportal.dane.gov.co/descargas/mgn_2020/MGN2020_MPIO_POLITICO.rar"
zip_dep <- "https://geoportal.dane.gov.co/descargas/mgn_2020/MGN2020_DPTO_POLITICO.rar"

municipios <- read_sf("./mapas/MGN_MPIO_POLITICO.shp")

depto_int <- "BOYACÁ"

ver <- municipios %>% 
  filter(DPTO_CNMBR==depto_int) %>% 
  mutate(MPIO_CDPMP= as.numeric(MPIO_CDPMP))
num_depto <- as.numeric(unique(ver$DPTO_CCDGO))

variables <- read.table("datos/fin_pub.txt", 
                        header = T, sep = "|", dec = ",",
                        colClasses = c("numeric","character","numeric","character","character","character","character",
                                       "numeric","character","numeric","numeric","character","character")) %>% 
  filter(Código.Departamento == num_depto,
         Indicador =="% de ingresos que corresponden a transferencias") 

variables_g <- variables %>% 
  filter(Año==2007)
grafica <- ver %>% 
  left_join(variables_g, 
            by = c("MPIO_CDPMP"="Código.Entidad"))

ggplot(data = grafica) +
  geom_sf(aes(fill=Dato.Numérico))+
  #labs(fill=unique(grafica$Indicador))+
  labs(fill="%Transferencias")+
  theme_void()  



install.packages("tmap")
library(tmap)
library(magick)

variables_g <- variables %>% 
  filter(Año==2010)
grafica <- ver %>% 
  left_join(variables_g, 
            by = c("MPIO_CDPMP"="Código.Entidad"))

map_nza <- tm_shape(shp = grafica) +
  tm_borders() +
  tm_fill(col = "Dato.Numérico")

map_nza + tm_style("classic") + tm_layout(title = 2010)

grafico_1 <- image_read("2007.png")
grafico_2 <- image_read("2008.png")
grafico_3 <- image_read("2009.png")
grafico_4 <- image_read("2010.png")

img <- c(grafico_1, grafico_2, grafico_3, grafico_4)

image_append(image_scale(img, "x200"))
my.animation<-image_animate(image_scale(img, "400x400"), fps = 1, dispose = "previous")
image_write(my.animation, "eab-spread.gif")

# ver eat
municipios$MPIO_CDPMP <- as.numeric(municipios$MPIO_CDPMP)
esquemas <- readxl::read_excel("./datos/eats.xlsx")
unique(esquemas$`Tipo EAT`)

para_mapa <- esquemas %>% 
  filter(`Tipo EAT`=="Área Metropolitana") %>% 
  select(3,7,8,9,10,15:18)

grafica <- municipios %>% 
  left_join(para_mapa, by = c("MPIO_CDPMP"="Código DANE de la ET")) %>% 
  mutate(categoria = case_when(
    Categoría=="5"~"Quinta",
    Categoría=="6"~"Sexta", 
    Categoría==""~"Especial",
    Categoría== "4"~"Cuarta",
    Categoría=="1"~"Primera",
    Categoría== "3"~ "Tercera",
    Categoría== "2"~ "Segunda",
    is.na(Categoría) ~ "No Asociado"
  ))
grafica$categoria <- factor(grafica$categoria, levels = c("Especial",
                                                          "Primera",
                                                          "Segunda",
                                                          "Tercera",
                                                          "Cuarta",
                                                          "Quinta",
                                                          "Sexta", 
                                                          "No Asociado"))

grafica$pertenece_eat <- 

ggplot(data = grafica) +
  geom_sf(aes(fill=Categoría))+
  #labs(fill=unique(grafica$Indicador))+
  labs(fill="%Transferencias")+
  theme_void()  

map_nza <- tm_shape(shp = grafica) +
  tm_borders() +
  tm_fill(col = "categoria")

map_nza + tm_style("cobalt") + tm_layout(title = "Categoría Municipal")

view(grafica)


