# Introduction ----
#### Data Analysis of tree stems around South Africa
# Farhan Ahmed, Bioinformatics Intern at NIGAB, NARC, (ahmedfarhanraja@gmail.com)
# Data Loading ----
trees_bicuar <- read.csv("trees_bicuar.csv")
trees_mlunguya <- read.csv("trees_mlunguya.csv")
#### View data 
head(trees_bicuar)
str(trees_bicuar)
head(trees_mlunguya)
str(trees_mlunguya)
# Data Analysis ----
basal.area <- function(x){
  (pi*(x)^2)/40000
}
basal.area <- function(...){
  (pi*(...)^2)/40000
}
basal.area(trees_bicuar$diam)
basal.area(trees_mlunguya$diam)
trees_bicuar$ba <- basal.area(trees_bicuar$diam)
trees_mlunguya$ba <- basal.area(trees_mlunguya$diam)
trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)
for( i in 1:length(trees) ){
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)
}
trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year)
mean_ba_list <- list()
for( i in 1:length(trees_mlunguya_list) ){
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year, mean_ba)
  mean_ba_list[[i]] <- dat
}
ba.mean.year <- function(dbh, year){
  data.frame(
    mean_ba = mean(basal.area(dbh)),
    year = mean(year)
  )    
}

ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)
for( i in 1:length(trees_mlunguya_list) ){
	mean_ba_list[[i]] <- ba.mean.year(
		trees_mlunguya_list[[i]]$diam,
		trees_mlunguya_list[[i]]$year)
}
# Applying lapply() function
lapply(trees_mlunguya_list, function(x){ba.mean.year(dbh = x$diam, year = x$year)})
bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family)
lapply(bicuar_height_list, mean, na.rm = TRUE)
sapply(bicuar_height_list, mean, na.rm = TRUE)
stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  
  lorey_height <- sum(height_adj * ba, na.rm = TRUE) / sum(ba, na.rm = TRUE)
  
  return(lorey_height)
}
trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)

lapply(trees_bicuar_list, function(x){stick.adj.lorey(height = x$height, method = x$height_method, ba = x$ba)})
diam.summ <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE, 
                     mean(dbh), 
                     NA)
  median_dbh <- ifelse(median == TRUE, 
                       median(dbh), 
                       NA)
  mean_ba <- ifelse(ba == TRUE, 
                    mean(basal.area(dbh)), 
                    NA)
  
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

diam.summ(dbh = trees_bicuar$diam, mean = TRUE, median = FALSE)
# loop to plot multiple graphs----
LPI <- read.csv("LPI_data_loops.csv")
#### Scatter plot
library(ggplot2)
library(dplyr)
vulture <- filter(LPI, Common.Name == "Griffon vulture / Eurasian griffon")
vultureITCR <- filter(vulture, Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              # Changing point size
    geom_smooth(method = lm, aes(fill = Country.list)) +                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position.inside = c(0.9, 0.9)))               # Setting the position for the legend - 0 is left/bottom, 1 is top/right
theme.my.own <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}
(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme.my.own() +                                                    # Adding our new theme!
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))
# Graphs 2 ----
LPI.UK <- filter(LPI, Country.list == "United Kingdom")
house.sparrow <- filter(LPI.UK, Common.Name == "House sparrow")
great.tit <- filter(LPI.UK, Common.Name == "Great tit")
corn.bunting <- filter(LPI.UK, Common.Name == "Corn bunting")
reed.bunting <- filter(LPI.UK, Common.Name == "Reed bunting")
meadow.pipit <- filter(LPI.UK, Common.Name == "Meadow pipit")
(house.sparrow_scatter <- ggplot(house.sparrow, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "House sparrow"))

(great.tit_scatter <- ggplot(great.tit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Great tit"))

(corn.bunting_scatter <- ggplot(corn.bunting, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Corn bunting"))

(meadow.pipit_scatter <- ggplot(meadow.pipit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Meadow pipit"))
# Arranging plots in a grid
library(gridExtra)
panel <- grid.arrange(house.sparrow_scatter, great.tit_scatter, corn.bunting_scatter, meadow.pipit_scatter, ncol = 2)
ggsave(panel, file = "Pop_trend_panel.png", width = 10, height = 8)
dev.off()
# List of Spieces and graphs
Sp_list <- list(house.sparrow, great.tit, corn.bunting, meadow.pipit)
for (i in 1:length(Sp_list)) {                                    
  data <- as.data.frame(Sp_list[i])                              
  sp.name <- unique(data$Common.Name)                           
  plot <- ggplot(data, aes (x = year, y = abundance)) +              
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = sp.name)
  
  ggsave(plot, file = paste(sp.name, ".pdf", sep = ''), scale = 2)  
  
  print(plot)                                                      
}