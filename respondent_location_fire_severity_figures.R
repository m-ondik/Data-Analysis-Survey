#Resources:
#https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument
#https://www.flutterbys.com.au/stats/tut/tut5.4.html
#https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html


library(tidyverse)
library(sf)
library(ggspatial)
# library(png)
# library(grid)
library(magick)

library(ggpmisc)


AUS_STATE_shp <- read_sf("RawData/Maps/Aus_States_shape/STE_2016_AUST.shp")
AUS_STATE_SA4_shp <- read_sf("RawData/Maps/SA4_2021_AUST_SHP_GDA2020/SA4_2021_AUST_GDA2020.shp")
NSW_shp <- AUS_STATE_SA4_shp %>% 
  filter(STE_NAME21 == "New South Wales")

respondent_locations <- read_csv("RawData/Survey_Raw_Data_May 11_2022_07_22.csv") %>% 
  filter(StartDate != "Start Date" & StartDate != '{"ImportId":"startDate","timeZone":"America/Denver"}') %>% 
  filter(Q3 == "Yes") %>% #Direct experience Yes
  select(Q6, Q10_2) %>%  #location/region, % foliage altered
  rename("Region" = Q6, "foliage_altered" = Q10_2) %>% 
  left_join(read_csv("RawData/Respondent_map_data.csv")) %>% #Read in survey respondents locations and fire severity ratings
  rename("long" = Longitude, "lat" = Latitude, "Severity" = Fire_Severity_Rating)
# %>% 
#   mutate(foliage_altered = gsub('[[:punct:] ]+','',foliage_altered))

# respondent_geometry <- respondent_locations %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326)

aus_nsw_plot <- ggplot() +
  geom_sf(data=AUS_STATE_shp)+#plot Australia states shape coordinates
  geom_sf(data = AUS_STATE_shp %>% filter(STE_NAME16 == "New South Wales"), fill = "#424242")+ #Fill in NSW with grey filling
  coord_sf(xlim = c(110, 155), ylim = c(-45, -11), expand = FALSE)+ #Set x and y limits
  theme_void() #Remove all axes and background
#Save Australia map to .PNG
png(file="Figures/survey_respondents_plot_part2.png", width=150, height=150)
aus_nsw_plot
dev.off()

aus_img <- image_read(path="Figures/survey_respondents_plot_part2.png") #Use Magick package to read in a png file
cities<- tribble(~x, ~y, ~city, 151.2093, -33.8688, "Sydney", 149.1310, -35.2802, "Canberra") #creates a rowwise tibble with "~" denoting column names
bushfires <- read_sf("RawData/Maps/201920-bushfire-boundaries/FY2019_2020_Bushfire_Boundaries.shp") #reads in 2019-2020 bushfire boundaries shape file
NSW_bushfires <- bushfires %>% filter(State == "NSW")

# aus_img

# ggplot() +
#   geom_sf(data = NSW_bushfires) #plot NSW 2019-2020 bushfire boundaries coordinates 

respondents_plot <- ggplot() +
  # geom_sf(data=AUS_STATE_shp)+
  geom_sf(data = NSW_shp)+ #plot NSW shape coordinates and the SA4 region outlines
  # geom_sf(data = NSW_bushfires, fill = "#EF9A9A", color = "#E57373")+
  geom_sf(data = NSW_bushfires, fill = "rosybrown", color = "rosybrown")+#alpha("firebrick4", 0.5), color = alpha("firebrick4", 0.5))+#fill = "#FFCDD2", color = "#EF9A9A")+ #plot NSW 2019-2020 bushfire boundaries coordinates and fill in with orange pick with slightly darker orange pink outline
  geom_text(data = tribble(~x, ~y, ~state, 145, -31, "New South Wales"), #Create coordinate location for text "New South Wales" and add it to map at that location
            aes(x, y, label = state), size = 14)+
  geom_point(data=respondent_locations, aes(x = long, y = lat, fill = Severity), shape = 23, size=5)+ #Place diamonds at locations of each respondent filled with the color corresponding to fire severity
  geom_point(data = cities, aes(x, y), shape = 16, size = 3, stroke = 1) + #Place round black filled dots at city locations for Sydney and Canberra
  geom_text(data = tribble(~x, ~y, ~city, 149.1310, -35.2802, "Canberra"), #Create coordinate location for text "Canberra" and add it to map at that location
            aes(x=x-0.05,y = y+0.25,label = city), size = 6)+
  geom_text(data = tribble(~x, ~y, ~city, 151.2093, -33.8688, "Sydney"),#Create coordinate location for text "Sydney" and add it to map at that location
            aes(x = x+0.75, y,label = city), size = 6)+
  scale_fill_gradient2(name = "Observed Severity", #Assign legend title
                       low = "#F9FBE7", mid = "#E65100", high = "black", #Assign colors for gradient legend
                       midpoint = 2, #Assign numeric midpoint (severity rating) for color gradient legend
                       breaks=c(0,1,2,3,4), #Assign breaks for legend labels corresponding to fire severity ratings
                       labels = format(c("Unburnt","Low","Moderate", "High", "Extreme")))+ #Assign text to each legend break
  # ggtitle("Directly Impacted Respondents") +
  xlab("Longitude") + #Name x-axis
  ylab("Latitude") + #Name y-axis
  coord_sf(xlim = c(140, 155), ylim = c(-38.5, -28), expand = FALSE)+ #Set x and y boundaries
  theme_bw()+ #Set theme
  theme(legend.position = c(0.87, 0.25))+ #Set legend position
  annotation_scale(location = "tl", width_hint = 0.3, #Add distance scale to map in top left corner
                   text_cex = 1.5 #Font size of distance text
                   ) +
  annotation_north_arrow(location = "tl", which_north = "true", #Add Arrow pointing north below distance scale in top right corner
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         # height = unit(2, "cm"), width = unit(2, "cm"), #Default is 1.5 x 1.5cm
                         style = north_arrow_fancy_orienteering(text_size = 16))+
  annotation_raster(aus_img, #Add image on top of map (Image denotes location of NSW in Australia)
                    ymin = -Inf, #Give boundaries to size and place image correctly in map
                    ymax = -35.5, 
                    xmin = -Inf, 
                    xmax = 143.5) + 
  annotate("rect", xmin = 140, xmax = 143.7, ymin = -38.5, ymax = -35.5, #Add rectangle around Australia image
           alpha = .25)+ #shade in rectangle very faintly
  theme(axis.title = element_text(size = 20), #Change font sizes
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 15),
        title = element_text(size = 18)
        )   

# respondents_plot

##Save final image
# png(file="Figures/survey_respondents_plot_part1.png", width=700, height=700)
# respondents_plot
# dev.off()

png(file="Figures/respondents_map.png", width=800, height=800)
respondents_plot
dev.off()


respondents_foliage_plot <- ggplot() +
  # geom_sf(data=AUS_STATE_shp)+
  geom_sf(data = NSW_shp)+ #plot NSW shape coordinates and the SA4 region outlines
  # geom_sf(data = NSW_bushfires, fill = "#EF9A9A", color = "#E57373")+
  geom_sf(data = NSW_bushfires, fill = "rosybrown", color = "rosybrown")+#alpha("firebrick4", 0.5), color = alpha("firebrick4", 0.5))+#fill = "#FFCDD2", color = "#EF9A9A")+ #plot NSW 2019-2020 bushfire boundaries coordinates and fill in with orange pick with slightly darker orange pink outline
  geom_text(data = tribble(~x, ~y, ~state, 145, -31, "New South Wales"), #Create coordinate location for text "New South Wales" and add it to map at that location
            aes(x, y, label = state), size = 14)+
  geom_point(data=respondent_locations, aes(x = long, y = lat, fill = Severity), shape = 23, size=5)+ #Place diamonds at locations of each respondent filled with the color corresponding to fire severity
  geom_point(data = cities, aes(x, y), shape = 16, size = 3, stroke = 1) + #Place round black filled dots at city locations for Sydney and Canberra
  geom_text(data = tribble(~x, ~y, ~city, 149.1310, -35.2802, "Canberra"), #Create coordinate location for text "Canberra" and add it to map at that location
            aes(x=x-0.05,y = y+0.25,label = city), size = 6)+
  geom_text(data = tribble(~x, ~y, ~city, 151.2093, -33.8688, "Sydney"),#Create coordinate location for text "Sydney" and add it to map at that location
            aes(x = x+0.75, y,label = city), size = 6)+
  scale_fill_gradient2(name = "Foliage altered (%)", #Assign legend title
                       low = "#F9FBE7", mid = "#E65100", high = "black", #Assign colors for gradient legend
                       midpoint = 2, #Assign numeric midpoint (severity rating) for color gradient legend
                       breaks=c(0,1,2,3,4), #Assign breaks for legend labels corresponding to fire severity ratings
                       labels = format(c("0% (Unburnt)","25% (Moderate)","50% (Moderate)", "75% (Moderate)", "100% (High-Extreme)")))+ #Assign text to each legend break
  # ggtitle("Directly Impacted Respondents") +
  xlab("Longitude") + #Name x-axis
  ylab("Latitude") + #Name y-axis
  coord_sf(xlim = c(140, 155), ylim = c(-38.5, -28), expand = FALSE)+ #Set x and y boundaries
  theme_bw()+ #Set theme
  theme(legend.position = c(0.87, 0.25))+ #Set legend position
  annotation_scale(location = "tl", width_hint = 0.3, #Add distance scale to map in top left corner
                   text_cex = 1.5 #Font size of distance text
  ) +
  annotation_north_arrow(location = "tl", which_north = "true", #Add Arrow pointing north below distance scale in top right corner
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         # height = unit(2, "cm"), width = unit(2, "cm"), #Default is 1.5 x 1.5cm
                         style = north_arrow_fancy_orienteering(text_size = 16))+
  annotation_raster(aus_img, #Add image on top of map (Image denotes location of NSW in Australia)
                    ymin = -Inf, #Give boundaries to size and place image correctly in map
                    ymax = -35.5, 
                    xmin = -Inf, 
                    xmax = 143.5) + 
  annotate("rect", xmin = 140, xmax = 143.7, ymin = -38.5, ymax = -35.5, #Add rectangle around Australia image
           alpha = .25)+ #shade in rectangle very faintly
  theme(axis.title = element_text(size = 20), #Change font sizes
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 15),
        title = element_text(size = 18)
  )   

png(file="Figures/respondents_foliage_map.png", width=800, height=800)
respondents_foliage_plot
dev.off()

# 
# #_________________________________________________________________________________
# library(tidyverse)
# library(maps)
# library(mapdata)
# library(sp)
# library(maptools)
# library(shapefiles)
# 
# aus<-map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
#          ylim=c(-45,-5), mar=c(0,0,0,0))
# 
# 
# aus.sp <- map2SpatialPolygons(aus, IDs = aus$names)
# par(mar=c(0,0,0,0))
# plot(aus.sp, asp=1)
# 
# ggplot(fortify(aus.sp), aes(y=lat, x=long, group=group)) + geom_polygon()
# 
# NSW.shp<-read.shp("RawData/Maps/NSW_shape/MB_2016_NSW.shp")
# 
# NSW.h <- NSW.shp$header
# par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
# plot(0,0, xlim=c(NSW.h$xmin, NSW.h$xmax), ylim=c(NSW.h$ymin, NSW.h$ymax),asp=1,
#      axes=F, ann=F)
# invisible(mapply(function(x) polygon(x$points, col="grey"),
#                  NSW.shp$shp))


# #__________________________________________________________________________________
# library(oz)
# 
# oz(sections = c(4,13, 14, 15))

#___________________________________________________________________________________
#Fire severity correlation plots
library(grid) 
library(RColorBrewer)
library(ggh4x)

fire_severity_data <- read_csv("IntermediateData/Fire Severity Assignment/FESM_fire_severity_by_respondent_locations.csv") %>% 
  left_join(respondent_locations %>% select(Region, Severity))%>% 
  rename("Foliage Altered (%)" = `Foliage altered`,
         "Lightest Ash Color"= `Max Ash color rating`,
         "FESM Fire Severity" = `FESM most likely fire severity rating`,
         "Observed Severity" = Severity)

colnames(fire_severity_data)

fire_severity_data_long <- fire_severity_data %>% 
  select(-`FESM Main Fire severities`, -`FESM Fire severities`, -`FESM most likely fire severity`, -`Ash color`, -`Ash color rating`) %>% 
  pivot_longer(`Tree Kill (%)`: `Observed Severity`, names_to = "Observed_property", values_to = "Observed_value") %>% 
  mutate(Observed_property = factor(Observed_property, c("Tree Kill (%)", "Foliage Altered (%)", "Lightest Ash Color", "Observed Severity"))) 




t_col <- function(color_vector, percent, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  new_color_vector <- c()
  i=1
  for (color in color_vector){
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    new_color_vector[i] <- t.col
    i <- i+1
  }
  return(new_color_vector)
}

brewer.pal(9,"Oranges")
t_col(brewer.pal(9,"Oranges"), 30) %>% print()
brewer.pal(3,"Greys")
t_col("black", 30) %>% print()

color_vector <- c(colorRampPalette(c("#F9FBE7", "#E65100"))(4), colorRampPalette(c("#E65100", "black"))(4)) %>% 
  t_col(30) %>% unique()

g <- color_vector %>%  #30% transparency to range of colors from white to orange to black
  # c("white", "#FF4500B2", "#000000B2") %>%  #30% transparency
  # c("white", "#FFA5007F", "#0000007F") %>% #50% transparency
# c("white", "#E6550D","#636363") %>%
  rev() %>% 
  rasterGrob(width=unit(1,"npc"), height = unit(1,"npc"), 
             interpolate = TRUE) 

g <- rasterGrob(c("#F9FBE7"), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE) 

one <- fire_severity_data %>% 
  ggplot(aes(x = `FESM Fire Severity`, y = `Tree Kill (%)`))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_count()+
  scale_size_continuous(range = c(1, 3),breaks = round)+
  stat_poly_line(color = "black") +
  stat_poly_eq(aes(label = after_stat(rr.label)))+
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  scale_y_continuous(breaks =  seq(-25, 100, by = 25))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text  = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none")
one

two <- fire_severity_data %>% 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_count(aes(x = `FESM Fire Severity`, y = `Foliage Altered (%)`))+
  
three <- fire_severity_data %>% 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_count(aes(x = `FESM Fire Severity`, y = `Lightest Ash Color`))+
  
four <- fire_severity_data %>% 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_count(aes(x = `FESM Fire Severity`, y = `Observed Severity`))+

all_plots <- ggplot(data = fire_severity_data_long, aes(x = `FESM Fire Severity`, y = Observed_value))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_count()+
  scale_size_continuous(range = c(1, 3),breaks = round)+
  stat_poly_line(color = "black") +
  stat_poly_eq(aes(label = after_stat(rr.label)))+
  facet_wrap(~Observed_property, scales = "free",  strip.position = "left", ncol = 4)+
  # theme(strip.placement = "outside",
  #       strip.switch.pad.grid = unit(1, "cm")) +
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.text  = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "none",
        strip.placement = "outside")+
  labs(y = NULL)+
  ggh4x::facetted_pos_scales(y = list(
    Observed_property == "Tree Kill (%)" ~ scale_y_continuous(breaks =  seq(-25, 100, by = 25)),
    Observed_property == "Foliage Altered (%)" ~ scale_y_continuous(breaks =  seq(-25, 100, by = 25)),
    Observed_property == "Lightest Ash Color" ~ scale_y_continuous(breaks = seq(0, 4, by = 1), labels = format(c("NA", "Black", "Grey", "Grey-White", "White"))),
    Observed_property == "Observed Severity"~ scale_y_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))
  ))

all_plots
all_plots %>% 
  ggsave(filename = "Figures/Fire_Severity_Correlation_plots.png", width = 7, height = 2)

#_____________________________________________________________________________________
ggplot()+
  # geom_tile(data = df, aes(x, y, fill = (y/10)+x),alpha = 0.75)+
  # scale_fill_gradient2(low = "#F9FBE7", mid = "#E65100", high = "black", midpoint = 5)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_count(data = fire_severity_data, aes(x = `FESM most likely fire severity rating`, y = `Tree Kill (%)`))+
  scale_size_continuous(range = c(2, 6),breaks = round)+
  stat_poly_line(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Tree Kill (%)`), color = "black") +
  stat_poly_eq(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Tree Kill (%)`,label = after_stat(rr.label)))+
  scale_y_continuous(breaks = seq(0, 100, by = 25))+
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  xlab("FESM Fire Severity")
# grid.draw(g) 

# fire_severity_data %>% 
#   ggplot(aes(`FESM most likely fire severity rating`, `Foliage altered`)) +
#   geom_count()+
#   scale_size_continuous(range = c(2, 4),breaks = round)+
#   stat_poly_line() +
#   stat_poly_eq(aes(label = after_stat(rr.label)))
# 
# 
# fire_severity_data %>% 
#   ggplot(aes(`FESM most likely fire severity rating`, `Max Ash color rating`)) +
#   geom_count()+
#   scale_size_continuous(range = c(2, 6),breaks = round)+
#   stat_poly_line() +
#   stat_poly_eq(aes(label = after_stat(rr.label)))
# 
# fire_severity_data %>% 
#   ggplot(aes(`FESM most likely fire severity rating`, `Respondents fire severity rating`)) +
#   geom_count()+
#   scale_size_continuous(range = c(2, 5),breaks = round)+
#   stat_poly_line() +
#   stat_poly_eq(aes(label = after_stat(rr.label)))
  
ggplot()+
  # geom_tile(data = df, aes(x, y, fill = (y/10)+x),alpha = 0.75)+
  # scale_fill_gradient2(low = "#F9FBE7", mid = "#E65100", high = "black", midpoint = 5)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_count(data = fire_severity_data, aes(x = `FESM most likely fire severity rating`, y = `Foliage altered`))+
  scale_size_continuous(range = c(2, 6),breaks = round)+
  stat_poly_line(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Foliage altered`), color = "black") +
  stat_poly_eq(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Foliage altered`,label = after_stat(rr.label)))+
  scale_y_continuous(breaks = seq(0, 100, by = 25))+
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  ylab("Foliage Altered (%)")+
  xlab("FESM Fire Severity")

ggplot()+
  # geom_tile(data = df, aes(x, y, fill = (y/10)+x),alpha = 0.75)+
  # scale_fill_gradient2(low = "#F9FBE7", mid = "#E65100", high = "black", midpoint = 5)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_count(data = fire_severity_data, aes(x = `FESM most likely fire severity rating`, y = `Max Ash color rating`))+
  scale_size_continuous(range = c(2, 6),breaks = round)+
  stat_poly_line(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Max Ash color rating`), color = "black") +
  stat_poly_eq(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Max Ash color rating`,label = after_stat(rr.label)))+
  scale_y_continuous(breaks = seq(0, 4, by = 1), labels = format(c("NA", "Black", "Grey", "Grey-White", "White")))+
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  ylab("Lightest Ash Observed")+
  xlab("FESM Fire Severity")


ggplot()+
  # geom_tile(data = df, aes(x, y, fill = (y/10)+x),alpha = 0.75)+
  # scale_fill_gradient2(low = "#F9FBE7", mid = "#E65100", high = "black", midpoint = 5)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_count(data = fire_severity_data, aes(x = `FESM most likely fire severity rating`, y = `Respondents fire severity rating`))+
  scale_size_continuous(range = c(2, 6),breaks = round)+
  stat_poly_line(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Respondents fire severity rating`), color = "black") +
  stat_poly_eq(data = fire_severity_data, aes(`FESM most likely fire severity rating`, `Respondents fire severity rating`,label = after_stat(rr.label)))+
  scale_y_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  ylab("Observed Fire Severity")+
  xlab("FESM Fire Severity")

#___________________________________________________________________________________
library(ggplot2) 
library(grid)
library(RColorBrewer)

make_gradient <- function(deg = -45, n = 100, cols) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

g <- make_gradient(cols = c("#F9FBE7", "#E65100", "black"))

color_vector <- c("white", "white") %>% 
  t_col(60) %>% unique()

h <- color_vector %>%  #30% transparency to range of colors from white to orange to black
  # c("white", "#FF4500B2", "#000000B2") %>%  #30% transparency
  # c("white", "#FFA5007F", "#0000007F") %>% #50% transparency
  # c("white", "#E6550D","#636363") %>%
  rev() %>% 
  rasterGrob(width=unit(1,"npc"), height = unit(1,"npc"), 
             interpolate = TRUE) 

ggplot()+
  # geom_tile(data = df, aes(x, y, fill = (y/10)+x),alpha = 0.75)+
  # scale_fill_gradient2(low = "#F9FBE7", mid = "#E65100", high = "black", midpoint = 5)+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  annotation_custom(h, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_count(data = fire_severity_data, aes(x = `FESM Fire Severity`, y = `Foliage Altered (%)`))+
  scale_size_continuous(range = c(1, 2),breaks = round)+
  stat_poly_line(data = fire_severity_data, aes(`FESM Fire Severity`, `Foliage Altered (%)`), color = "black") +
  # stat_poly_eq(data = fire_severity_data, aes(`FESM Fire Severity`, `Foliage Altered (%)`,label = after_stat(rr.label)), label.y = "bottom", label.x = "right")+
  scale_y_continuous(breaks = seq(0, 100, by = 25))+
  scale_x_continuous(breaks = seq(0, 4, by = 1), labels = format(c("Unburnt", "Low", "Moderate", "High", "Extreme")))+
  ylab("Foliage Altered (%)")+
  xlab("FESM Fire Severity")

ggsave(filename = "Figures/Foliage_FESM_Correlation_plot.png", width = 3.5, height = 2.5)
ggsave(filename = "Figures/Foliage_FESM_Correlation_plot_noR2.png", width = 3.5, height = 2.5)

#_______________________________________________________________________________________
library(ordinal)
library(fmsb)

fire_severity_data <- read_csv("IntermediateData/Fire Severity Assignment/FESM_fire_severity_by_respondent_locations.csv") %>% 
  left_join(respondent_locations %>% select(Region, Severity))%>% 
  rename("Foliage Altered (%)" = `Foliage altered`,
         "Lightest Ash Color"= `Max Ash color rating`,
         "FESM Fire Severity" = `FESM most likely fire severity rating`,
         "Observed Severity" = Severity)

fire_severity_data_long <- fire_severity_data %>% 
  select(-`FESM Main Fire severities`, -`FESM Fire severities`, -`FESM most likely fire severity`, -`Ash color`, -`Ash color rating`) %>% 
  pivot_longer(`Tree Kill (%)`: `Observed Severity`, names_to = "Observed_property", values_to = "Observed_value") %>% 
  mutate(Observed_property = factor(Observed_property, c("Tree Kill (%)", "Foliage Altered (%)", "Lightest Ash Color", "Observed Severity"))) 

foliage_data <- fire_severity_data_long %>% 
  filter(Observed_property == "Foliage Altered (%)") %>% 
  mutate(Observed_value_ordinal = as.factor(Observed_value*2)) %>% 
  mutate(FESM_ordinal= as.factor(`FESM Fire Severity`*2)) %>% 
  filter(!is.na(Observed_value_ordinal), !is.na(`FESM_ordinal`))

foliage_clm <- clm(`Observed_value_ordinal`~`FESM_ordinal`, data = foliage_data)
summary(foliage_clm)
NagelkerkeR2(foliage_clm)
