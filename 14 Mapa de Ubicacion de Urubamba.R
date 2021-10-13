# Librerias
library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(tmap)
# Cargamos data
alt = getData("alt", country = "Peru", path = tempdir())
Per         <- getData('GADM', country='Peru', level=1) %>%st_as_sf() 
Peru        <- getData('GADM', country='Peru', level=2) %>%st_as_sf() 
Peru_dis    <- getData('GADM', country='Peru', level=3) %>%st_as_sf() 
Cusco       <- subset(Peru, NAME_1 == "Cusco")
Cusco_dis   <- subset(Peru_dis, NAME_1 == "Cusco")
Urubam      <- subset(Cusco_dis, NAME_2 == "Urubamba")
Urub.Paqcha <- subset(Cusco_dis, NAME_3 == "Urubamba")
Cusco_alt     <- crop(alt, Cusco)
Cusco_alt <-Cusco_alt <- mask(Cusco_alt , Cusco_dis)
# Creacion del DEM
slope = terrain(Cusco_alt, opt = "slope") 
aspect = terrain(Cusco_alt, opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

dem1 = raster("Raster/Urubamba/1.tif")
slopee = terrain(dem1, opt = "slope") 
aspecte = terrain(dem1, opt = "aspect")
hill_uru = hillShade(slopee, aspecte, angle = 40, direction = 270)
# Mapeo
MAP=tm_shape(hill,ylim=c(-15.45833,  -11.20833),xlim=c( -74, -70.35)) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE) +
  tm_shape(Cusco_alt) +
  tm_raster(alpha = 0.5, palette = colorRampPalette( c("#33911F","#6BB04C","#9FCF6F","#DFF0A9","#F9DAA3","#EC9662","#D65931","#B91313"))(8),n=12, 
            legend.show = T, title="Elevacion\n(m.s.n.m)", legend.show = FALSE)+
  tm_compass(type="8star", position=c("right", "top"), text.color = "black")+
  tm_layout(title = "", legend.title.size=.7,legend.text.size = 0.6,
            legend.position = c(.033, .005) ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "black",
            title.size = .8,
            legend.title.color = "black",
            legend.text.color = "black",
            fontface="bold",
            legend.stack = 'horizontal',
            bg.color="#F9F9F9",
            legend.bg.color = "#F9F9F9")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 2, position = c(0.65, 0.70))
g1= tmap_grob(MAP)


Peruu=ggplot()+
  geom_sf(data=Per, fill="white", color="black")+
  geom_sf(data = Cusco, fill="black", color="black")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
Peruu.grob <- ggplotGrob(Peruu)

Paqcha=ggplot()+
  geom_sf(data=Cusco, fill="white", color="black")+
  geom_sf(data = Urubam, fill="gray", color="gray", alpha=0.8)+
  geom_sf(data=Urub.Paqcha , fill="red", color="gold")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
Paqcha.grob <- ggplotGrob(Paqcha)

MAPP=tm_shape(hill_uru) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE) +
  tm_shape(dem1) +
  tm_raster(alpha = 0.5, palette = colorRampPalette( c("#33911F","#6BB04C","#9FCF6F","#DFF0A9","#F9DAA3","#EC9662","#D65931","#B91313"))(8),n=12, 
            legend.show = T, title="Elevacion\n(m.s.n.m)")+
  tm_shape(Urubam)+
  tm_borders("black")+
  tm_text("NAME_3",size = .8)+
  tm_shape(Urub.Paqcha)+
  tm_borders("red")+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "black", color.dark = "lightsteelblue4", 
               position = c(.3, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="8star", position=c("left", "top"), text.color = "black")+
  tm_layout(title = "", legend.title.size=.7,legend.text.size = 0.6,
            legend.position = c(.033, .005) ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "black",
            title.size = .8,
            legend.title.color = "black",
            legend.text.color = "black",
            fontface="bold",
            legend.stack = 'horizontal',
            bg.color="#F9F9F9",
            legend.bg.color = "#F9F9F9")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 2, position = c(0.65, 0.05))+
  tm_grid(col = "grey",ticks = T, labels.col = "black")
g3= tmap_grob(MAPP)

# Mapa final
library(cowplot)
im=ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 21), expand = FALSE) +
  draw_plot(g1, width = 6, height = 6,x = 19.5, y = 6)+
  draw_plot(Peruu.grob, width =5, height = 5 ,x = 19, y = 12)+
  draw_plot(Paqcha.grob, width = 5, height = 5 ,x = 19.5, y = 1)+
  draw_plot(g3, width = 20, height = 20,x = 0.05, y = 0.05)+
  theme(panel.background = element_rect(fill = "white"))+
  annotate(geom = "text", x = 22, y = 19, label = "Mapa de Ubicacion \nZona de Paqcha-Dis.Urubamba", 
           family="serif", color = "Black", size = 3.5, face = "bold")
# Exportacion
ggsave(plot = im ,"MAPAS/Urubamba.png",
             units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico




