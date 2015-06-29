library("shapefiles")
library("rgl")
if(!file.exists("BR_Localidades_2010_v1.dbf")){
  download.file("ftp://geoftp.ibge.gov.br/organizacao_territorial/localidades/Shapefile_SHP/BR_Localidades_2010_v1.dbf", destfile = "BR_Localidades_2010_v1.dbf")
}
dbf <- read.dbf("BR_Localidades_2010_v1.dbf", TRUE)
data <- dbf$dbf
dfLocalidades <- data.frame(data$LONG, data$LAT)

plot(dfLocalidades)
plot3d(
  dbf$dbf$LONG,
  dbf$dbf$LAT,
  dbf$dbf$ALT, 
  col = 1 + abs(floor(dbf$dbf$ALT / 200)), 
  size = 5, 
  xlab = "Longitude", 
  ylab = "Latitude", 
  zlab = "Altitude"
)

plot3d(data$LONG, data$LAT, data$ALT)

wss <- (nrow(dfLocalidades)-1)*sum(apply(dfLocalidades,2,var))
for (i in 2:30)
{
  wss[i] <- sum(kmeans(dfLocalidades, centers = i)$withinss)
}
plot(wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
kmLocalidades <- kmeans(dfLocalidades, centers = 6)
plot(dfLocalidades, col = kmLocalidades$cluster)
points(kmLocalidades$centers, col="yellow", pch = 4)