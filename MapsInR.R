library(tidyverse)

# -------------------------------------------------------------------------- ###
# Download Data----
# -------------------------------------------------------------------------- ###
# Get USGS Data
# https://earthquake.usgs.gov/fdsnws/event/1/#format-geojson
usgs_url <- "https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=2020-01-01&endtime=2020-10-31&minmagnitude=4"
usgs_dt <- read_csv(usgs_url)

# Let's rename some columns
usgs_dt <- usgs_dt %>% 
  rename(Latitude = latitude,
         Longitude = longitude,
         Depth = depth,
         Magnitude = mag)

# Get AFAD Data
# https://deprem.afad.gov.tr/sondepremler
afad_url <- "https://deprem.afad.gov.tr/latestCatalogsList"
afad_dt <- afad_url %>% 
  httr::POST(encode = "json") %>% 
  .[["content"]] %>% 
  rawToChar() %>% 
  jsonlite::fromJSON() %>% 
  as_tibble()

# Some Cleaning
afad_dt <- afad_dt %>% 
  mutate(time = lubridate::dmy_hms(time),
         Latitude = as.numeric(lat),
         Longitude = as.numeric(lon),
         Depth = as.numeric(depth),
         Magnitude = as.numeric(m))


# -------------------------------------------------------------------------- ###
# Explanatory Analysis----
# -------------------------------------------------------------------------- ###
# Which are the largest earthquakes in our data sets?
usgs_dt %>% 
  filter(Magnitude == max(Magnitude)) %>% 
  select(time:Magnitude, place)

afad_dt %>% 
  filter(Magnitude == max(Magnitude)) %>% 
  select(time, Latitude, Longitude, Magnitude, place:other)


# Select only numeric data and get summary statistics
library(summarytools)
st_options(lang = "tr")

usgs_dt %>% 
  select_if(~ is.numeric(.)) %>% 
  summarytools::descr()

afad_dt %>% 
  select_if(~ is.numeric(.)) %>% 
  summarytools::descr()


# -------------------------------------------------------------------------- ###
# Some Plots----
# -------------------------------------------------------------------------- ###
myboxplot <- function(data, var){
  var <- enquo(var)
  p <- data %>% 
    ggplot() +
    geom_boxplot(aes(!!var)) +
    hrbrthemes::theme_ipsum_rc()
  
  p
    
}

p1 <- myboxplot(usgs_dt, Latitude) 
p2 <- myboxplot(usgs_dt, Longitude) 
p3 <- myboxplot(usgs_dt, Depth) 
p4 <- myboxplot(usgs_dt, Magnitude) 

# For other ways to do this,
# SEE: https://bit.ly/2HMwWM2
gridExtra::grid.arrange(p1, p2, p3, p4)


p1 <- myboxplot(afad_dt, Latitude) 
p2 <- myboxplot(afad_dt, Longitude) 
p3 <- myboxplot(afad_dt, Depth) 
p4 <- myboxplot(afad_dt, Magnitude) 

# For other ways to do this,
# SEE: https://bit.ly/2HMwWM2
gridExtra::grid.arrange(p1, p2, p3, p4)

# Correlation Plot
library(ggcorrplot)

usgs_dt %>% 
  select(Latitude, Longitude, Depth, Magnitude) %>% 
  cor() %>% 
  round(3) %>% 
  ggcorrplot(., 
             hc.order = TRUE, 
             type = "lower", 
             outline.color = "white", 
             ggtheme = hrbrthemes::theme_ipsum_rc(),
             lab = TRUE)

afad_dt %>% 
  select(Latitude, Longitude, Depth, Magnitude) %>% 
  cor() %>% 
  round(3) %>% 
  ggcorrplot(., 
             hc.order = TRUE, 
             type = "lower", 
             outline.color = "white", 
             ggtheme = hrbrthemes::theme_ipsum_rc(),
             lab = TRUE)

# Depth vs Magnitude
usgs_dt %>% 
  ggplot(aes(Depth, Magnitude)) +
  geom_point(alpha = 0.25)

afad_dt %>% 
  ggplot(aes(Depth, Magnitude)) +
  geom_point(alpha = 0.25)


# Let's summarize and plot some categorical variables
afad_dt %>% 
  count(country)

afad_dt %>% 
  group_by(country) %>% 
  select_if(~ is.numeric(.)) %>% 
  summarytools::descr()
  
afad_dt %>% 
  select(country, Latitude, Longitude, Depth, Magnitude) %>% 
  ggplot(aes(country, fill = country)) +
  geom_bar(stat = "count") +
  hrbrthemes::theme_ipsum_rc() +
  labs(x = "Country",
       y = "Count",
       fill = "")


afad_dt %>% 
  select(country, Latitude, Longitude, Depth, Magnitude) %>% 
  ggplot(aes(Magnitude, country)) +
  geom_boxplot() +
  hrbrthemes::theme_ipsum_rc() +
  labs(y = "",
       x = "Magnitude")

afad_dt %>% 
  select(country, Latitude, Longitude, Depth, Magnitude) %>% 
  mutate(country = fct_reorder(country, Magnitude)) %>% 
  ggplot(aes(Magnitude, country)) +
  geom_boxplot() +
  hrbrthemes::theme_ipsum_rc() +
  labs(y = "",
       x = "Magnitude")

afad_dt %>% 
  select(country, Latitude, Longitude, Depth, Magnitude) %>% 
  ggplot(aes(Magnitude, Depth)) +
  geom_point(alpha = 0.5) +
  hrbrthemes::theme_ipsum_rc() +
  labs(y = "Depth",
       x = "Magnitude") +
  facet_wrap(~country, scales = "free_y")


# This creates warnings but ignore them.
# This is just an example
usgs_dt %>% 
  separate(place, c("placeone", "country"), sep = ", ") %>% 
  count(country) %>% 
  arrange(desc(n))

countries <- c("Indonesia", "Japan", "Philippines", "Papua New Guinea", "Alaska", "Chile", "Tonga", "Greece", "Vanuatu", "Russia")

usgs_dt %>% 
  separate(place, c("placeone", "country"), sep = ", ") %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(Magnitude, Depth)) +
  geom_point(alpha = 0.5) +
  hrbrthemes::theme_ipsum_rc() +
  labs(y = "Depth",
       x = "Magnitude") +
  facet_wrap(~country, scales = "free_y")


# -------------------------------------------------------------------------- ###
# ggplot2----
# -------------------------------------------------------------------------- ###
# SEE: https://ggplot2-book.org/maps.html
library(viridis)
world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill="lightgray", 
               colour = "white") +
  geom_point(data = usgs_dt, 
             aes(Longitude, Latitude, colour = Magnitude, size = Magnitude)) +
  scale_color_viridis() +
  ggthemes::theme_map() +
  theme(legend.position = "right")


# Turkey Region
afad_dt %>% 
  select(Latitude, Longitude) %>% 
  summary()

lon_min <- 24
lon_max <- 44
lat_min <- 34.5
lat_max <- 41.5

ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill="lightgray", 
               colour = "white") +
  geom_point(data = afad_dt, 
             aes(Longitude, Latitude, colour = Magnitude, size = Magnitude)) +
  coord_map(xlim = c(lon_min, lon_max), 
            ylim = c(lat_min, lat_max)) +
  scale_color_viridis() +
  ggthemes::theme_map() +
  theme(legend.position = "right")


# Izmir
# You can use https://boundingbox.klokantech.com/

izmir_lon_min <- 25.7157
izmir_lon_max <- 27.9667
izmir_lat_min <- 37.2073
izmir_lat_max <- 38.8198

ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill="lightgray", 
               colour = "white") +
  geom_point(data = afad_dt, 
             aes(Longitude, Latitude, colour = Magnitude, size = Magnitude)) +
  coord_map(xlim = c(izmir_lon_min, izmir_lon_max), 
            ylim = c(izmir_lat_min, izmir_lat_max)) +
  scale_color_viridis() +
  ggthemes::theme_map() +
  theme(legend.position = "right")

# -------------------------------------------------------------------------- ###
# tmap----
# -------------------------------------------------------------------------- ###
# SEE: https://github.com/mtennekes/tmap
library(tmap)
library(sf)

# Define CRS
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

df <- st_as_sf(x = usgs_dt, 
               coords = c("Longitude", "Latitude"), 
               crs = projcrs)

tm_shape(spData::world) + 
  tm_style("classic") + 
  tm_fill(col = "white") + 
  tm_borders() + 
  tm_layout(main.title = "Earthquakes in 2020") + 
  tm_compass(type = "8star", position = c("right", "bottom")) + 
  tm_scale_bar(breaks = c(0, 2500, 5000), text.size = 1, position = c("left", "bottom")) +
  tm_shape(df) + 
  tm_dots(size = 0.1, col = "Magnitude", palette = "-plasma")


# -------------------------------------------------------------------------- ###
# Leaflet----
# -------------------------------------------------------------------------- ###
# SEE: https://rstudio.github.io/leaflet/
library(leaflet)

pal <- leaflet::colorFactor(viridis_pal(option = "B")(20), domain = afad_dt$Magnitude)

afad_dt %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude,
                   radius = ~ Magnitude * 3,
                   color = ~ pal(Magnitude),
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   popup = (paste("Time: ", afad_dt$time, "<br>",
                                  "Magnitude: ", afad_dt$Magnitude, "<br>",
                                  "Depth: ", afad_dt$Depth)))



# -------------------------------------------------------------------------- ###
# TurkeyMap----
# -------------------------------------------------------------------------- ###
# Harita verisini okutalim.
tr_province <- sptools::gadm("Turkey", "sf", 1, save = FALSE, intlib = FALSE)

# -------------------------------------------------------------------------- ###
# Buyukbas hayvanlarin haritasini cizelim----
# -------------------------------------------------------------------------- ###
# Veriyi oku ve duzenle
dt <- readxl::read_xls("./raw-data/bb_hayvan_sayisi.xls", skip = 3)

dt <- dt %>% 
  select(-c(2, 3, 7)) %>% 
  rename("year" = "...1") %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = -year, 
               names_to = "city", 
               values_to = "cattle") %>% 
  mutate(cattle = str_remove_all(cattle, "\\."),
         cattle = as.numeric(cattle))

# Harita verisinde sehir isimlerini kucuk harf yapalim
tr_province$NAME_1 <- tolower(tr_province$NAME_1)

# not in fonksiyonu yaratalim. %in% fonksiyonuna benzer.
'%!in%' <- function(x,y)!('%in%'(x,y))

# TUIK verisinde olup harita verisinde olmayan isimleri bulalim
dt$city[dt$city %!in% tr_province$NAME_1]

# Harita verisinde olup TUIK verisinde olmayan isimleri bulalim
tr_province$NAME_1[tr_province$NAME_1 %!in% dt$city]


# Sehir isimleri eslesmiyor. Bunlari duzeltelim. Bunun daha kisa bir yolu olabilir.
tr_province <- tr_province %>% 
  mutate(NAME_1 = str_replace_all(NAME_1,
                                  c("afyon" = "afyonkarahisar", 
                                    "bartın" = "bartin", 
                                    "bingöl" = "bingol", 
                                    "çanakkale" = "canakkale", 
                                    "çankiri" = "cankiri", 
                                    "çorum" = "corum", 
                                    "düzce" = "duzce", 
                                    "elazığ" = "elazig", 
                                    "gümüshane" = "gumushane", 
                                    "iğdır" = "igdir", 
                                    "k. maras" = "kahramanmaras", 
                                    "karabük" = "karabuk", 
                                    "kinkkale" = "kirikkale", 
                                    "kütahya" = "kutahya", 
                                    "zinguldak" = "zonguldak")))

# Tekrar kontrol edelim
# TUIK verisinde olup harita verisinde olmayan isimleri bulalim
dt$city[dt$city %!in% tr_province$NAME_1]

# Harita verisinde olup TUIK verisinde olmayan isimleri bulalim
tr_province$NAME_1[tr_province$NAME_1 %!in% dt$city]


# Eslesmeyen yok. Veri setlerini birlestirelim
map_dt <- left_join(tr_province, dt, by = c("NAME_1" = "city"))


# Haritayi cizelim
# Haritanin boyutu biraz buyuk. Zaman alabilir. Sabrederseniz images klasorundeki
# haritayi elde edersiniz.
ggplot(data = map_dt) +
  geom_sf(aes(fill = cattle)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Number of Cattles\n")

# Kaydedelim
ggsave("./images/tr_map.png")


# -------------------------------------------------------------------------- ###
# BONUS: WordCloud----
# -------------------------------------------------------------------------- ###
library(rvest)     # To download text
library(tm)        # Text mining package
library(wordcloud) # For word clouds

# Let's find a text and download it.
url <- "https://www.gutenberg.org/cache/epub/19033/pg19033.txt"

dt <- read_html(url)

dt_text <- dt %>% html_text()

dt_text


# Create vector source and then convert to corpus
docs <- Corpus(VectorSource(dt_text))

# Detailed info about corpus
inspect(docs)

# Create a content transformer.
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Transform the corpus 
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stop words as a character vector
docs <- tm_map(docs, removeWords, c("gutenbergtm", "gutenberg", "project", "ebook", "license", "agreement", "trademark", "terms"))

# Remove punctuation
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Construct a term-document matrix 
dtm <- TermDocumentMatrix(docs)
dtm

# Convert to matrix
dtm <- as.matrix(dtm)
head(dtm)

dtm <- sort(rowSums(dtm), decreasing = TRUE)
head(dtm)

# Convert to data frame
dt <- data.frame(word = names(dtm), freq = dtm)
head(dt)

# Create the word cloud
wordcloud(words = dt$word,
          freq = dt$freq,
          min.freq = 10,
          max.words = 300,
          random.order = FALSE,
          rot.per = 0.35,
          colors = c("#a1395b", "#243b61"))

