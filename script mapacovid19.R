# cargar libreria pacman 
require(pacman)

# cargar todas las librerias a usar # NOTA. no totas estas librerias se usaran 
# para este ejemplo
pacman::p_load(raster, rgdal,
               rgeos, stringr, sf,
               tidyverse, RColorBrewer, 
               cowplot, ggpubr, ggspatial, 
               rnaturalearth, rnaturalearthdata,
               viridisLite, viridis)

# establecer tu directorio de trabajo



# obtener datos 

## leer datos covid 20 julio 

# descargar base de datos de covid.

#cargar la base de datos 

cv<- read.csv("200720COVID19MEXICO.csv")

View(cv)


#eliminar casos de personas no muertas
#(se realiza esta elimincion ya que la variable 
#fecha de defuncion las personas 
#que no han muerto tienen este formado)

# filtrar los datos para calcular defunciones
cv1<- filter(cv,FECHA_DEF !="9999-99-99")


#base de defunciones
# tomaremos los datos que de personas que hayan dado +
# al a la prueba

def<-cv1 %>% filter(RESULTADO==1)%>%
  group_by(ENTIDAD_RES)%>%
  summarise(defunciones=n()) 


View(def)





# base de contagios

# iobtener datos de contagios
# tomaremos los datos de la base original
# obtendremos solo los + a la prueba
cont<-cv %>% filter(RESULTADO==1)%>%
  group_by(ENTIDAD_RES)%>%
  summarise(contagios=n())


# obtener datos de la poblacion en mexico

# cargar la base de datos (2) numero de habitantes
#(estimacion)


dat1 <-read.csv("base_municipios_final_datos_01.csv")
dat2 <-read.csv( "base_municipios_final_datos_02.csv")

View(dat1)


#Filtrar datos por anio
# nos quedamos solo con los datos del 2020.

da1 <- dat1[dat1$AÑO%in%2020,]
da2 <- dat2[dat2$AÑO%in%2020,]

# juntamos las 2 bases de datos 
datos <- rbind(da1, da2)

names(datos)


# obtener los el numero de habitantes por estado
# lo agruparemos por Entidad y obtendremos la suma de 
#hombres y mujeres

pob<- datos %>% group_by(CLAVE_ENT)%>%
  summarise(poblacion=sum(POB))

View(pob)

# podemos renombrear las variables 

names(pob) <- c("ENTIDAD_RES", "poblacion")




## juntar bases de datos 




######### datos vectoriales 
mun<-st_read("MÃ©xico_Estados.shp")

mun<- st_transform(mun, crs = 4326)

head(mun)

class(mun)
View(mun)
str(mun)

names(mun)

# separar variable codigo 

mun<- mun %>% separate(CODIGO, c("borrar", "ENTIDAD_RES"),sep=2)

View(mun)
# modificar observaciones para quitarles el cero

mun$ENTIDAD_RES[mun$ENTIDAD_RES=="01"]<-1
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="02"]<-2
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="03"]<-3
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="04"]<-4
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="05"]<-5
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="06"]<-6
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="07"]<-7
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="08"]<-8
mun$ENTIDAD_RES[mun$ENTIDAD_RES=="09"]<-9

View(mun)

# convertir a numerico la variable
mun$ENTIDAD_RES<- as.numeric(mun$ENTIDAD_RES)



head(cont)

# juntar bases de datos (poblacion y contagios)
base<- pob %>% left_join(cont)

View(base)
# juntar bases de datos poblacion, contagios y defunciones
base1<-base %>% left_join(def)


View(base1)

write.csv(base1, "variables.csv")
#calcular variables, tasa de letalidad y tasa de contagio

base2<- base1 %>% mutate(letalidad= defunciones/contagios*100,# letalidad 
                         tasa.cont=contagios/poblacion*100000)# tasa de contagio



View(base2)

names(mun)
names(cont)


# juntar shp con datos 
dts<- mun %>% left_join(base2)

View(dts)

# cargar capa de puntos estados 
puntos<-st_read("estados.puntos.shp")


class(puntos)

str(puntos)
# transformar src
puntos<- st_transform(puntos, crs = 4326)


class(puntos)
names(puntos)
str(puntos)

# renombrear la variable cvegeo por Entidad_RES
puntos<- rename(puntos, ENTIDAD_RES=CVEGEO)


# juntar base de datos con el shape de puntos 
pts<- puntos %>% left_join(base2)

# aparece un error por lo que hay que transformar a numerico
puntos$ENTIDAD_RES<- as.integer(puntos$ENTIDAD_RES)

# juntar shp con dataframe
pts<- puntos %>% left_join(base2)



# hacer mapa 

ggplot()+# funcion para crear grafico
  geom_sf(data = dts, aes(fill=tasa.cont))+# geometria espacial, llenado por tasa.cont
  scale_fill_distiller(palette   = "Reds", direction = 1)+# paleta de colores
  geom_sf(data = pts, aes(size=letalidad), alpha=0.3, col="blue")+# añadir geometria de puntos por tamaño letalidad
  labs(title = "titulo del grafico",# añadir titulo
       caption = "Fuente: www.gob.mx/Datos. abiertos",# añadir caption
       fill="Tasa de contagio \n (100,000 Habs)", size="Datos (%)\n de letalidad",# añadir nombres a etiquetas
       subtitle = "Por: autor")+# añadir subtitulo
  theme_bw()+# tema del grafico
  theme(panel.background = element_rect(fill = "white"),# personalizacion del tema
        plot.background=element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.background = element_rect(fill=NA))+
  annotation_north_arrow(location="tr", which_nort=TRUE,# añadir rosa del norte
                         style = north_arrow_nautical(),# tipo de rosa "nautica"
                         height = unit(2.5, "cm"),# tamaño altura
                         width = unit(2.5, "cm"))+# tamaño anchura
  annotation_scale()# añadir escala

# salvar grafico
ggsave("titulo.png", units = "cm", width = 23,# guardar grafico
       height = 15, dpi = 700)
