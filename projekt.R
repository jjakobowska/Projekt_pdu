# Ustawienie katalogu roboczego
setwd("C:\\Users\\olgas\\OneDrive\\Pulpit\\projekt")

# Instalacja i załadowanie pakietów
library(flightsbr)
library(quantmod)
library(dplyr)
library(lubridate)

# Parametry 
rok <- 2023
miesiace <- 1:12

# 1. Zapisanie ramek danych z odpowiednich bibliotek

### a. Loty pasażerskie
loty_rok <- lapply(miesiace, function(m) {
  kod_miesiaca <- as.numeric(sprintf("%d%02d", rok, m))
  cat("Loty:", kod_miesiaca, "\n")
  df <- read_flights(date = kod_miesiaca)
  df$month <- m
  df$year <- rok
  df
}) %>% bind_rows()

write.csv(loty_rok, sprintf("flights_%d.csv", rok), row.names = FALSE)

#### b. Ceny biletów krajowych
bilety_dom <- lapply(miesiace, function(m) {
  kod_miesiaca <- as.numeric(sprintf("%d%02d", rok, m))
  cat("Ceny krajowe:", kod_miesiaca, "\n")
  df <- read_airfares(date = kod_miesiaca, domestic = TRUE)
  df$month <- m
  df$year <- rok
  df
}) %>% bind_rows()

write.csv(bilety_dom, sprintf("airfares_domestic_%d.csv", rok), row.names = FALSE)

### c. Ceny biletów międzynarodowych
airfares_int_2023 <- lapply(miesiace, function(m) {
  kod_miesiaca <- as.numeric(sprintf("%d%02d", rok, m))
  df <- read_airfares(date = kod_miesiaca, domestic = FALSE)
  df$month <- m
  df$year  <- rok
  df
}) %>% bind_rows()

write.csv(airfares_int_2023, file = sprintf("airfares_international_%d.csv", rok),row.names = FALSE)

### d. Ruch lotniczy
ruch_rok <- lapply(miesiace, function(m) {
  kod_miesiaca <- as.numeric(sprintf("%d%02d", rok, m))
  cat("Ruch:", kod_miesiaca, "\n")
  df <- read_airport_movements(date = kod_miesiaca)
  df$month <- m
  df$year <- rok
  df
}) %>% bind_rows()

write.csv(ruch_rok, sprintf("airport_movements_%d.csv", rok), row.names = FALSE)

### e. Samoloty 
cat("Samoloty zarejestrowane w", rok, "\n")
aircrafts <- read_aircrafts(date = rok)
aircrafts$year <- rok

write.csv(aircrafts, sprintf("aircrafts_%d.csv", rok), row.names = FALSE)

### f. Lotniska 
cat("Lotniska (wszystkie)\n")
airports <- read_airports(type = "all")

write.csv(airports, "airports_all.csv", row.names = FALSE)

### g. Kursy walutowe
start_date <- as.Date(sprintf("%d-01-01", rok))
end_date <- as.Date(sprintf("%d-12-31", rok))

getSymbols("BRL=X", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

kurs_brl_usd <- `BRL=X` %>%
  as.data.frame() %>%
  mutate(date = as.Date(rownames(.))) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(mean_rate = mean(`BRL=X.Close`, na.rm = TRUE), .groups = "drop")

write.csv(kurs_brl_usd, sprintf("exchange_rate_brl_usd_%d.csv", rok), row.names = FALSE)


# 2. Wczytanie ramek danych z plików

flights_2023 <- read.csv("flights_2023.csv")
airfares_domestic_2023 <- read.csv("airfares_domestic_2023.csv")
airfares_international_2023 <- read.csv("airfares_international_2023.csv")
airport_movements_2023 <- read.csv("airport_movements_2023.csv")
aircrafts <- read.csv("aircrafts_2023.csv")
airports_all <- read.csv("airports_all.csv")
exchange_rate_brl_usd_2023 <- read.csv("exchange_rate_brl_usd_2023.csv")
airports_international <- read.csv("icao_cities_full.csv") 

# ostatnia ramka została stworzona własnoręcznie przez nas na potrzeby projektu (zawiera kody ICAO i nazwy miast, których potrzebowałyśmy do analizy )


# 3.Stworzenie ramki danych potrzebnej do analizy dla miesięcy 5-7 (rozbicie wynika z błędnego nazewnictwa kolumn w oryginalnych ramkach danych):
#   Ramka zawiera kolumny: year, month, airline, origin, origin_city, destination, destination_city, flight_type, fare_brl, fare_usd, mean_rate.

# Wybór kolumn z ramki danych airfares_domestic_2023, zmiana nazw i dodanie kolumny flight_type
airfares_domestic_1 <- airfares_domestic_2023 %>% 
  select(year, month, airline = empresa, origin = origem, destination = destino, fare_brl = tarifa) %>%
  mutate(
    fare_brl    = as.numeric(gsub(",", ".", fare_brl)),
    flight_type = "DOMESTIC"
  )

# Wybór kolumn z ramki danych airfares_international_2023, zmiana nazw i dodanie kolumny flight_type
airfares_international_1 <- airfares_international_2023 %>% 
  select(year, month, airline = icao_empresa_aerea, origin = icao_aerodromo_origem,
         destination = icao_aerodromo_destino, fare_brl = tarifa_nominal) %>%
  mutate(
    fare_brl    = as.numeric(gsub(",", ".", fare_brl)),
    flight_type = "INTERNATIONAL"
  )

airfares_all <- bind_rows(airfares_domestic_1, airfares_international_1)

# Dodanie danych o kursach walut
airfares_all <- airfares_all %>%
  left_join(exchange_rate_brl_usd_2023, by = c("year", "month")) %>%
  mutate(fare_usd = fare_brl / mean_rate) 

# Stworzenie ramki danych zawierającej kody ICAO i nazwy miast
icao_city <- airports_all %>%
  select(ICAO = codigo_oaci, City = municipio)
combined_icao_city <- bind_rows(icao_city, airports_international)

# Dodanie informacji o miastach
airfares_all <- airfares_all %>%
  left_join(combined_icao_city, by = c("origin" = "ICAO")) %>%
  rename(origin_city = City) %>%
  left_join(combined_icao_city, by = c("destination" = "ICAO")) %>%
  rename(destination_city = City)

# Wybrana kolejność kolumn
airfares_all <- airfares_all %>%
  na.omit() %>%
  filter(flight_type == "INTERNATIONAL") %>%
  select(
    year, month, airline, origin, origin_city,
    destination, destination_city, flight_type,
    fare_brl, fare_usd, mean_rate
  )

# Zapisanie ramki danych do pliku
write.csv(airfares_all, "airfares_all_57.csv", row.names = FALSE)


# 3.Stworzenie ramki danych potrzebnej do analizy dla miesięcy 1-4 i 8-12 (rozbicie wynika z błędnego nazewnictwa kolumn w oryginalnych ramkach danych):
#   Ramka zawiera kolumny: year, month, airline, origin, origin_city, destination, destination_city, flight_type, fare_brl, fare_usd, mean_rate.

# Wybór kolumn z ramki danych airfares_domestic_2023, zmiana nazw i dodanie kolumny flight_type
airfares_domestic_1 <- airfares_domestic_2023 %>% 
  select(year, month, airline = empresa, origin = origem, destination = destino, fare_brl = tarifa) %>%
  mutate(
    fare_brl    = as.numeric(gsub(",", ".", fare_brl)),
    flight_type = "DOMESTIC"
  )

# Wybór kolumn z ramki danych airfares_international_2023, zmiana nazw i dodanie kolumny flight_type
airfares_international_1 <- airfares_international_2023 %>% 
  select(year, month, airline = empresa, origin = origem,
         destination = destino, fare_brl = tarifa) %>%
  mutate(
    fare_brl    = as.numeric(gsub(",", ".", fare_brl)),
    flight_type = "INTERNATIONAL"
  )

airfares_all <- bind_rows(airfares_domestic_1, airfares_international_1)

# Dodanie danych o kursach walut
airfares_all <- airfares_all %>%
  left_join(exchange_rate_brl_usd_2023, by = c("year", "month")) %>%
  mutate(fare_usd = fare_brl / mean_rate) 

# Stworzenie ramki danych zawierającej kody ICAO i nazwy miast
icao_city <- airports_all %>%
  select(ICAO = codigo_oaci, City = municipio)
combined_icao_city <- bind_rows(icao_city, airports_international)

# Dodanie informacji o miastach
airfares_all <- airfares_all %>%
  left_join(combined_icao_city, by = c("origin" = "ICAO")) %>%
  rename(origin_city = City) %>%
  left_join(combined_icao_city, by = c("destination" = "ICAO")) %>%
  rename(destination_city = City)

# Wybrana kolejność kolumn
airfares_all <- airfares_all %>%
  na.omit() %>%
  filter(flight_type == "INTERNATIONAL") %>%
  select(
    year, month, airline, origin, origin_city,
    destination, destination_city, flight_type,
    fare_brl, fare_usd, mean_rate
  )

# Zapisanie ramki danych do pliku
write.csv(airfares_all, "airfares_all_14_812.csv", row.names = FALSE)


### 4. Wczytanie stworzonych ramek danych
airfares_all_57 <- read.csv("airfares_all_57.csv")
airfares_all_14_812 <- read.csv("airfares_all_14_812.csv")


### 5. Połączenie i zapisanie do pliku wynikowej ramki danych
airfares_combined <- bind_rows(
  airfares_all_14_812,
  airfares_all_57
) %>%
  arrange(month)

write.csv(airfares_combined,"airfares_combined.csv", row.names = FALSE)


### 6. Komentarze
###    Nadmiarowe dane wynikają ze zmieniających się koncepcji projektu w czasie
















