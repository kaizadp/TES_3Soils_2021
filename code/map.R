library(ggplot2)
library(usmap)

test_data <- data.frame(lon = c(-147.487, -123.690, -81.419), 
                        lat = c(65.162,46.308, 28.105),
                        site = c("Alaska", "Washington", "Florida"))
transformed_data <- usmap_transform(test_data)
plot_usmap(color = NA, fill = "grey90", alpha = 1) + 
  geom_point(data = transformed_data, 
             aes(x = lon.1, y = lat.1, color = site), 
             #color = "black",
             size = 7)+
#  annotate("text", label = "Secret River (SR)", x = -1686856, y = 411954.3, size=2, hjust="left")+
#  annotate("text", label = "Caribou Poker Creeks Research Watershed\n(CPCRW)", 
#           x = -1051020, y = -1842646.7, 
#           size=2, hjust="left")+
  scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
  theme(legend.position = "none")+
  NULL
