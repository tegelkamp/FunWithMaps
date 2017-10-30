#R-Script um mit ggmaps und verschiedenen Datenlayern den 
#optimalen Wohnort zu finden

#Notwendige Pakete installieren
#install.packages("ggmap")
#install.packages("placement")
#install.packages("googleway")

#Karte laden und erste Fahrtzeit ausprobieren
require(ggmap)
require(googleway)

my.apikey<-'AIzaSyAGt0UMnSW956R_GvWijJysOBPrk71GGUk'

#Adressen festlegen
adress.work<-c("Ebhardtstraße 6, 30159 Hannover, Germany"
              ,"Venloer Str. 14, 50672 Köln, Germany")

adress.home<-c("Bistritzer Str. 92, 50858 Köln, Germany"
              , "Brandenburger Str. 13, 50668 Köln, Germany"
              , "Großer Wall 1, 33378 Rheda-Wiedenbrück"
              , "Bothmerstr. 1, 30519 Hannover"
              , "Harkortstr. 17-13, 59423 Unna"
              , "Meerbrede 17, 32107 Bad Salzuflen"
              , "Moorhoffstraße 2-10, 30419 Hannover")


adress.station<-list(c("50.938158,6.845206"),
                     c("Bahnhof Köln Lövenich"),
                     c("Köln Hauptbahnhof, Germany"))

rm(my.routes) 
my.routes <- expand.grid(from = adress.home
                         , to = adress.work
                         , stringsAsFactors = FALSE)

#Fahrtzeit mit Auto und Verkehr
my.routes$morning<-
    sapply(1:length(my.routes$from),
          function(x) {google_distance(origins=my.routes$from[x]
                ,destinations=my.routes$to[x]
                ,mode='driving'
                ,departure_time = as.POSIXct("2017-11-13 07:00:00"
                                             , format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")
                ,key = my.apikey
                ,traffic_model = "pessimistic"
                ,simplify = TRUE)$rows$elements[[1]][3]$duration_in_traffic$value
          })

my.routes$evening<-
  sapply(1:length(my.routes$from),
         function(x) {google_distance(origins=my.routes$to[x]
                                      ,destinations=my.routes$from[x]
                                      ,mode='driving'
                                      ,departure_time = as.POSIXct("2017-11-13 18:00:00"
                                                                   , format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")
                                      ,key = my.apikey
                                      ,traffic_model = "pessimistic"
                                      ,simplify = TRUE)$rows$elements[[1]][3]$duration_in_traffic$value
         })

#Taegliche Fahrtzeiten errechnen
my.routes$by.car.minutes<-round((my.routes$morning
                                 +my.routes$evening)/60,0)

#Taegliche Fahrtzeiten je Wohnort aggregieren
driving.times<-aggregate(my.routes$by.car.minutes
               , by=list(from=my.routes$from), FUN=sum)

adress.home.lat.long<-sapply(1:length(driving.times$from)
                   , function(x) {google_geocode(driving.times$from[x]
                                                 , key=my.apikey)$results$geometry$location})

driving.times$lat<-unlist(adress.home.lat.long['lat',])
driving.times$lng<-unlist(adress.home.lat.long['lng',])

#Grenzen fuer die Darstellung in der Karte ermitteln
map.bounds<-sapply(1:length(adress.work)
                   , function(x) {google_geocode(adress.work[x]
                                    , key=my.apikey)$results$geometry$location})


#Kartenmittelpunkt berechnen
my.map <- get_map(location = c(lon = mean(unlist(map.bounds['lng',1:2]))
                            , lat = mean(unlist(map.bounds['lat',1:2])))
               , zoom = 8,
               source = "google")

ggmap(my.map) + 
  geom_point(aes(x=lng,y=lat, colour=x),
             data=driving.times) + 
  scale_color_gradient(low = "green", high="red")



################################################################################
################################################################################
################################################################################
#Routen anzeigen (aber eigentlich nicht notwendig....)
my.directions<-route(from = adress.work[1], to = adress.work[2])

ggmap(my.map) +
  geom_leg(
    aes(
      x = startLon, xend = endLon,
      y = startLat, yend = endLat
    ),
    data = my.directions, color = "red"
  )


qmap(driving.times$from[1])

#Laufzeit zum Bahnhof
mapdist(from=adress.home
        , to=adress.station
        , mode=c("walking"))

#Fahrtzeit zum Bahnhof (+10 Minuten Parkplatz und Laufen)


drive_time(adress.home
           , adress.work
           , privkey = my.apikey
           , travel_mode = "TRANSIT")

#Ermittle die geringste Fahrtzeit (ueber welchen Bahnhof,
#mit welchen Verkehrsmittelmix)

qmap(adress.work)
