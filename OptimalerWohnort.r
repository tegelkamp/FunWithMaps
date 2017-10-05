#R-Script um mit ggmaps und verschiedenen Datenlayern den 
#optimalen Wohnort zu finden

#Notwendige Pakete installieren
#install.packages("ggmap")
#install.packages("placement")
install.packages("googleway")

#Karte laden und erste Fahrtzeit ausprobieren
require(ggmap)
require(googleway)

my.apikey<-'AIzaSyAGt0UMnSW956R_GvWijJysOBPrk71GGUk'

#Adressen festlegen
adress.work<-list(c("Ebhardtstraße 6, 30159 Hannover, Germany")
                 , c("Venloer Str. 14, 50672 Köln, Germany"))

adress.home<-list(c("Bistritzer Str. 92, 50858 Köln, Germany")
                 ,c("Brandenburger Str. 13, 50668 Köln, Germany"))


adress.station<-list(c("50.938158,6.845206"),
                     c("Bahnhof Köln Lövenich"),
                     c("Köln Hauptbahnhof, Germany"))

#Fahrtzeit mit Auto und Verkehr
tmp<-google_distance(origins=adress.home[1]
                ,destinations=adress.work[1]
                ,mode='driving'
                ,departure_time = as.POSIXct("2017-11-13 07:00:00"
                                             , format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                ,key = my.apikey
                ,traffic_model = "pessimistic"
                ,simplify = TRUE)$rows$elements[[1]][3]$duration_in_traffic$value


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
