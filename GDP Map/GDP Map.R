library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends


cn.val <- read.csv("Your Local Address\\1cn_2012_gdp.csv", header = T, sep = ",")

cn.getdata  <- getData("GADM", country = "CHN", level = 1)
cn.usedata <- fortify(cn.getdata)

cn.usedata <- join(cn.usedata, cn.val, by = "id", type = "inner")

distcenters <- ddply(cn.usedata, .(id),summarize, clat = mean(lat), clong = mean(long))
distcenters <- join(cn.val, distcenters, by = "id", type = "inner")

range(cn.usedata$gdp)
brks <- c(70000, 100000, 2000000,4000000, 6000000)

p <- ggplot() +	
  geom_polygon(data = cn.usedata, aes(x = long, y = lat, group = group, fill = gdp), 
               color = "black", size = 0.15) +
  scale_fill_distiller(palette = "Reds", breaks = brks, trans = "reverse") +
  geom_text(data = distcenters, aes(x = clong, y = clat, label = abbr, size = 0.1))+
  theme_nothing(legend = TRUE) +
  labs(title = "GDP of provinces in China in 2015", fill = "in millions")

plot(p)

