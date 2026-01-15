# Clear workspace
rm(list = ls())

# Read stations data
stations <- read.csv("Data/geo_peninsula_zones.csv")

# Change zone names to english
stations$Zona[stations$Zona == 'MS'] <- 'SP'
stations$Zona[stations$Zona == 'MN'] <- 'NP'

# Read Zeus' global data frame
global_df <- readRDS("~/ERA5_Record_Algorithm/Data/global_data/global_df.rds")
global_df$Zone <- stations$Zona[match(global_df$STAID, stations$STAID)]

# Set reference years and filter data frame
year_idx <- c(25:64)
global.filtered <- global_df[which(global_df$t %in% year_idx),]

################################################################################
# Create boxplots
outdir <- 'Results/Exploratory/Geo_boxplots/'

pdf(file = file.path(outdir, "box.G300.pdf"), width = 5, height = 4)

par(mar = c(5, 6, 4, 2))  
boxplot(g300./9.80665 ~ Ix, data = global.filtered,
col = c("blue3", "lightblue"),
names = c("No Record", "Record"),
main = "G300",
xlab = "Ix", ylab = expression(m),
outline = FALSE,
cex.axis = 1.2,   # tamaño de los números en los ejes
cex.lab = 1.4,    # tamaño de "Ix" y del eje Y
cex.names = 1.4,  # tamaño de "No Record", "Record"
cex.main = 1.6)   # tamaño del título)

dev.off()

pdf(file = file.path(outdir, "box.G500.pdf"), width = 5, height = 4)

par(mar = c(5, 6, 4, 2))  
boxplot(g500./9.80665 ~ Ix, data = global.filtered,
col = c("darkgreen", "lightgreen"),
names = c("No Record", "Record"),
main = "G500",
xlab = "Ix", ylab = expression(m),
outline = FALSE,
cex.axis = 1.2,   # tamaño de los números en los ejes
cex.lab = 1.4,    # tamaño de "Ix" y del eje Y
cex.names = 1.4,  # tamaño de "No Record", "Record"
cex.main = 1.6)   # tamaño del título)

dev.off()

pdf(file = file.path(outdir, "box.G700.pdf"), width = 5, height = 4)

par(mar = c(5, 6, 4, 2))  
boxplot(g700./9.80665 ~ Ix, data = global.filtered,
col = c("red", "pink"),
names = c("No Record", "Record"),
main = "G700",
xlab = "Ix", ylab = expression(m),
outline = FALSE,
cex.axis = 1.2,   # tamaño de los números en los ejes
        cex.lab = 1.4,    # tamaño de "Ix" y del eje Y
        cex.names = 1.4,  # tamaño de "No Record", "Record"
        cex.main = 1.6)   # tamaño del título)

dev.off()
################################################################################
# Create boxplots by zones
library(ggplot2)
# Create a vector of labels: zone name, then blank, repeated
zones <- unique(global.filtered$Zone)
x_labels <- rep(zones, each = 2)
x_labels[seq(2, length(x_labels), 2)] <- ""  # blank every second label

G300.z <- ggplot(global.filtered, aes(x = interaction(Ix, Zone), y = g300./9.80665)) +
  geom_boxplot(aes(fill = factor(Ix)), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "G300",
       x = "", y = expression(m)) +
  scale_fill_manual(values = c("blue3", "lightblue"),
                    name = "",
                    labels = c("Not record", "Record")) +
  scale_x_discrete(labels = x_labels) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, size = 12),   # texto eje x
    axis.text.y = element_text(size = 12),                         # texto eje y
    axis.title.x = element_text(size = 14),                        # etiqueta eje x
    axis.title.y = element_text(size = 14),                        # etiqueta eje y
    plot.title = element_text(size = 16, face = "bold"),           # título
    legend.text = element_text(size = 12),                         # texto leyenda
    legend.title = element_text(size = 13)                         # título leyenda (aunque lo ocultaste con name = "")
  )

G500.z <- ggplot(global.filtered, aes(x = interaction(Ix, Zone), y = g500./9.80665)) +
  geom_boxplot(aes(fill = factor(Ix)), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "G500",
       x = "", y = expression(m)) +
  scale_fill_manual(values = c("darkgreen", "lightgreen"),
                    name = "",
                    labels = c("Not record", "Record")) +
  scale_x_discrete(labels = x_labels) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, size = 12),   # texto eje x
    axis.text.y = element_text(size = 12),                         # texto eje y
    axis.title.x = element_text(size = 14),                        # etiqueta eje x
    axis.title.y = element_text(size = 14),                        # etiqueta eje y
    plot.title = element_text(size = 16, face = "bold"),           # título
    legend.text = element_text(size = 12),                         # texto leyenda
    legend.title = element_text(size = 13)                         # título leyenda (aunque lo ocultaste con name = "")
  )

G700.z <- ggplot(global.filtered, aes(x = interaction(Ix, Zone), y = g700./9.80665)) +
  geom_boxplot(aes(fill = factor(Ix)), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "G700",
       x = "", y = expression(m)) +
  scale_fill_manual(values = c("red", "pink"),
                    name = "",
                    labels = c("Not record", "Record")) +
  scale_x_discrete(labels = x_labels) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, size = 12),   # texto eje x
    axis.text.y = element_text(size = 12),                         # texto eje y
    axis.title.x = element_text(size = 14),                        # etiqueta eje x
    axis.title.y = element_text(size = 14),                        # etiqueta eje y
    plot.title = element_text(size = 16, face = "bold"),           # título
    legend.text = element_text(size = 12),                         # texto leyenda
    legend.title = element_text(size = 13)                         # título leyenda (aunque lo ocultaste con name = "")
  )


ggsave(filename = "G300.zone.pdf",
       plot = G300.z,
       device = "pdf",
       path = outdir, 
       width = 6, height = 4)

ggsave(filename = "G500.zone.pdf",
       plot = G500.z,
       device = "pdf",
       path = outdir, 
       width = 6, height = 4)

ggsave(filename = "G700.zone.pdf",
       plot = G700.z,
       device = "pdf",
       path = outdir, 
       width = 6, height = 4)
