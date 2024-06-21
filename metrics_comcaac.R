#
#metrics_comcaac.R
#script to plot article metrics for comcaa'c genetic articles

#Libraries --- ---

pacman::p_load('dplyr', 
               'ggplot2', 
               'ggrepel', 
               'RColorBrewer', 
               'viridis')

#Get data --- ---

metrics <- vroom::vroom(file ='/home/tiamat/Desktop/metrics_arts_comcaac.csv') %>% as.data.frame()

metrics <- rbind(metrics, data.frame(articulo = "Ojeda-Granados et al., 2021", 
                                          anio = "2022", numero_de_muestras_obtenidas_analizadas = "24",
                                     metodologia_de_genotipificacion = "Array", 
                                     gen_a_analizar = "-", 
                                          extension_genotipifcacion = "-",citas = "-"))

metrics <- metrics %>% mutate(Sample_Status = ifelse(row_number() <= 18, "With sample", "No sample"))

metrics <- metrics %>% mutate(metodologia_de_genotipificacion = ifelse(is.na(metodologia_de_genotipificacion), "PCR", metodologia_de_genotipificacion))

#Plotting --- ---

#1. Lollipop chart about tech

lollipop.df <- as.data.frame(table(metrics$metodologia_de_genotipificacion))
lollipop.df$percenta <- (lollipop.df$Freq / sum(lollipop.df$Freq)) * 100

lolliplot.p <- ggplot(lollipop.df, aes(x = Freq, y = Var1)) +
  geom_point() +
  geom_segment( mapping = aes( x = 0,
                               xend = Freq,
                               y = Var1,
                               yend = Var1 )) +
 geom_label(label = lollipop.df$Var1) +
labs( x = " ", 
      y = " ") +
  scale_y_discrete(labels = NULL) +
  geom_text(mapping = aes(label = Freq), vjust = 2.2)  +
  theme_classic() 

#Save

ggsave(filename = "comcaac_lollipop.png", 
       plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)

#Plot number of individuals analyzed by technology

metrics_tech <- metrics[1:18,]

# Sumar el número de muestras por tecnología
metrics_tech.sum <- aggregate(as.numeric(numero_de_muestras_obtenidas_analizadas) ~ metodologia_de_genotipificacion,
                                  data = metrics_tech, sum)

metrics_tech.sum$percen <- (sumas_por_tecnologia$total_muestras / total_muestras) * 100

# Renombrar la columna resultante
names(metrics_tech.sum)[2] <- "total_muestras"

# Calcular el porcentaje para cada tecnología
sumas_por_tecnologia$porcentaje <- (sumas_por_tecnologia$total_muestras / total_muestras) * 100

metrics_ref <- metrics[19:33,] 

metrics_by_year_ref <- metrics_ref %>%
  group_by(anio) %>%
  summarise(numero_de_articulos = n())
metrics_by_year_ref$ref <- "Paper re-analyzing data from previous works"

metrics_by_year_tech <- metrics_tech %>%
  group_by(anio) %>%
  summarise(numero_de_articulos = n())
metrics_by_year_tech$ref <- "Paper analyzing new data"

metrics_by_year <- rbind(metrics_by_year_ref, metrics_by_year_tech)
factor(metrics_by_year$ref)
metrics_by_year$anio <- as.numeric(as.character(metrics_by_year$anio))

metrics_by_year.p <- ggplot(metrics_by_year, aes(x = anio, y = numero_de_articulos, group = ref, color = ref)) +
#  geom_segment(aes(xend = anio, yend = 0)) +
  geom_point() +
  geom_line() +
#  geom_text(aes(label = numero_de_articulos), vjust = -0.5, size = 3, label.padding = unit(0.2, "lines")) +
  labs(x = " ", y = element_blank(), title = " ", color = " ") +
  scale_x_continuous(breaks = seq(min(metrics_by_year$anio), max(metrics_by_year$anio), by = 1)) +
  scale_color_brewer(palette = "Set1") +
  theme_classic()+
  theme(legend.position = c(0.2, 0.8))

#############################################

all_years <- seq(min(metrics_by_year$anio), max(metrics_by_year$anio))
all_refs <- unique(metrics_by_year$ref)
complete_data <- expand.grid(anio = all_years, ref = all_refs)

# Unir y reemplazar los NA con 0
metrics_by_year_complete <- merge(complete_data, metrics_by_year, by = c("anio", "ref"), all.x = TRUE)
metrics_by_year_complete[is.na(metrics_by_year_complete$numero_de_articulos), "numero_de_articulos"] <- 0

metrics_by_year_complete.a <- metrics_by_year_complete %>% filter(ref == "Paper analyzing new data")
mean(metrics_by_year_complete.a$numero_de_articulos)
metrics_by_year_complete.b <- metrics_by_year_complete %>% filter(ref == "Paper re-analyzing data from previous works")
mean(metrics_by_year_complete.b$numero_de_articulos)


# Crear el gráfico

metrics_by_year2.p <- ggplot(metrics_by_year_complete, aes(x = factor(anio),
                                                           y = numero_de_articulos, fill = ref)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = element_blank(),
       x = "Year",
       y = "Published peer-review articles",
       fill = element_blank()) +
  scale_fill_manual(values = c("#1f78b4", "#e31a1c")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = c(0.2, 0.8)) 

metrics_by_year2.p

#Save plot 

ggsave(filename = "comcaac_dodge.png", 
       plot = metrics_by_year2.p, width = 10, height = 4, units = "in", dpi = 300)

#Piechart --- ---

piechart.df <- as.data.frame(table(metrics$Sample_Status))

piechart.df <- piechart.df %>%  mutate(Percentage = Freq / sum(Freq) * 100,
       Label = paste0(Var1, " (", round(Percentage, 1), "%)"))

piechart.p <-ggplot(piechart.df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_label(aes(label = Label), 
             position = position_stack(vjust = 0.5),
             size = 4) +
  labs(title = NULL,
       x = NULL, 
       y = NULL) +
  theme_void() +  # Elimina el fondo y los ejes
  theme(legend.position = "none") +  # Elimina la leyenda
  scale_fill_brewer(palette = "Set3") +  
  annotate("text", x = 0, y = 0, label ="Sample number:\n 652", vjust = 0.5, size = 4, color = "black")

#Grid plots

lay <- rbind(c(1,1,1,1),
             c(1,1,1,1),
             c(2,2,3,3),
             c(2,2,3,3),
             c(2, 2, 3, 3))

grid <- gridExtra::grid.arrange(metrics_by_year2.p, piechart.p, layout_matrix = lay)

#Save plot

ggsave(filename = "comcaac_grid.png", 
       plot = grid, width = 15, height = 10, units = "in", dpi = 300)
