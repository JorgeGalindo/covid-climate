#This is just

library(tidyverse)
library(dplyr)


rm(list = ls())


#1. Fetch epidmeiological data! Downloaded from official sources.
#Colombia: https://www.datos.gov.co/Salud-y-Protecci-n-Social/Estado-de-Casos-de-Coronavirus-COVID-19-en-Colombi/6c4c-msrp
#Mexico: https://www.gob.mx/salud/documentos/datos-abiertos-152127

CO_df <- read.csv("data/CO.csv") %>%
  dplyr::mutate(
    country="Colombia",
    city=Ciudad.de.ubicación,
    date_detect=as.Date(paste0(as.character(Fecha.diagnostico),"-2020"), format ="%d-%m-%Y"),
    death=case_when(
      atención=="Fallecido" ~ 1,
      atención!="Fallecido" ~ 0
                   )
          ) %>%
  dplyr::select(country,city, date_detect, death)

MX_df <- read.csv("data/MX.csv") %>%
  dplyr::mutate(
    country="México",
    entidad=case_when(
      ENTIDAD_RES<10 ~ paste0("0",as.character(ENTIDAD_RES)),
      ENTIDAD_RES>9 ~ as.character(ENTIDAD_RES)
              ),
    municipio=case_when(
      MUNICIPIO_RES>=100 ~ as.character(MUNICIPIO_RES),
      between(MUNICIPIO_RES,10,100) ~ paste0("0",as.character(MUNICIPIO_RES)),
      MUNICIPIO_RES<10 ~ paste0("00",as.character(MUNICIPIO_RES))
           ),
    city_num=paste0(entidad, municipio),
    date_detect=as.Date(as.character(FECHA_INGRESO), format ="%Y-%m-%d"),
    death=case_when(
      FECHA_DEF != "9999-99-99" ~ 1,
      FECHA_DEF == "9999-99-99" ~ 0
    )
  ) %>%
  dplyr::filter(RESULTADO==1) %>%
  dplyr::select(country, city_num, date_detect, death)

##MX data has no municipal IDs so we have to add them. I downloaded mine from INEGI's official page

MXmun <- read.csv("data/municipiosmx.csv", sep=";", colClasses = c("character","character","character")) %>%
  dplyr::mutate(
    city_num=paste0(CLAVE_ENTIDAD, CLAVE_MUNICIPIO),
    city=as.character(MUNICIPIO)
  ) %>%
  dplyr::select(city,city_num)

MX_df <- left_join(MX_df, MXmun) %>%
  dplyr::select(country,city, date_detect, death)

##Unify (and put everything as lowercase so matching can be done with weather data afterwards!)
individual_df <- data.frame(rbind(CO_df, MX_df)) %>%
  dplyr::mutate(city=tolower(city)) %>%
  dplyr::select(-death)

##Convert into a standard acum cases-deaths

###Complete all dates since the 1st of January
dates_v <- as.character(seq(as.Date("2020-01-01"), as.Date("2020-04-25"), by="days"))

dummy_dates_v <- as.character(seq(as.Date("2020-01-01"), as.Date("2020-04-25"), by="days"))

dummy_dates_df <- data.frame(dummy_dates_v) %>%
  mutate(
    cases=as.integer(0),
    city="bogota",
    country="Colombia",
    date_detect=as.Date(dummy_dates_v, format = "%Y-%m-%d")
  ) %>%
  dplyr::select(city,date_detect,cases)

###Store country-city pair to be able to join it again later on

countrycity_df <- individual_df %>%  
  dplyr::group_by(country, city) %>%
  dplyr::summarize(cases=n()) %>% 
  dplyr::select(country, city, cases)

###Collapse basic df

basic_df <- individual_df %>%  
  dplyr::group_by(city, date_detect) %>%
  dplyr::summarize(cases=n()) 

basic_df <- data.frame(basic_df) 

###Add all dates
  
basic_df <- rbind(basic_df,dummy_dates_df)

wide_df <- basic_df %>% 
  pivot_wider(names_from = date_detect, values_from = cases)

basic_df <- wide_df %>% 
     pivot_longer(dates_v, names_to = "date_detect", values_to = "cases") %>%
     replace_na(list(cases=0))

acum_df <- basic_df %>%
  group_by(city) %>%
    mutate(acum_cases=cumsum(cases))

basic_df <- left_join(basic_df,acum_df)

co_cities_df <- countrycity_df %>%
  filter(country=="Colombia")

co_v <- co_cities_df$city

basic_df <- basic_df %>% 
  mutate(country=case_when(
          !(city  %in% co_v) ~ "MX",
          city %in% co_v ~ "CO"
              )
        ) %>% 
  mutate(cityid=paste0(country, city))

#2. Now, weather time!

##First, obtain temperatures 
##I download the data from OpenWeatherMaps. You can easily get an API key here: https://openweathermap.org
##In case you don't want or you cannot sign up to OWM you can use my data -- jump to "Filter and join"

library(owmr)
Sys.setenv(OWM_API_KEY = "_______") #Set your own API key

cities_df <- owm_cities %>%
  mutate(cityid=paste0(countryCode, tolower(nm))) %>%
  filter(cityid %in% basic_df$cityid)

df_cities <- cities_df %>%
  tibble::add_column(g = rep(1:20, 20)[1:nrow(cities_df)])

groups <- dplyr::group_split(df_cities, g)

res <- lapply(groups, function(g) get_current_for_group(g$id) %>% owmr_as_tibble) %>%
  dplyr::bind_rows()

df_cities <- df_cities %>% left_join(res, by = "id")

df_cities <- df_cities[!duplicated(df_cities$cityid), ]

write.csv(df_cities, "data/weather_recent.csv") # Just in case you want to store diff days of data


##Filter and join
df_cities <-read.csv("data/weather_recent.csv") # If you did not download any OWM data, here it will just use mine :)

casesandclimate_df <- basic_df %>% 
  dplyr::left_join(df_cities, by = "cityid")

casesandclimate_df <- casesandclimate_df %>% 
  dplyr::select(country, city, date_detect, cases, acum_cases, temp, humidity) %>%
  mutate(temp=temp-273.15) %>%
  filter(temp > 0) 

###Align by case count to graph it out
casesandclimate_df <- casesandclimate_df %>% 
  filter(city != "bogota") %>%
  filter(acum_cases>=5) %>%
  group_by(city) %>%
  mutate(days_since_5case = row_number()) %>%
  filter(days_since_5case == 1 | date_detect=="2020-04-17") %>%
  mutate(theline=case_when(
    date_detect == "2020-04-17" ~  "Today",
    date_detect != "2020-04-17" ~ "fifthcase"
        )
  ) %>%
  mutate(temp_facet=case_when(
      temp>33 ~ "1. About to die",
      temp<34 & temp>30 ~ "2. Very hot",
      temp<31 & temp>28 ~ "3. Hot",
      temp<29 & temp>25 ~ "4. Hottish",
      temp<26 & temp>20 ~ "5. Temperate",
      temp<21 ~ "6. Coldish"
      )
    ) 

###Calculate the slope
###This is just a rough mathematical approximation of how fast does the epidemic run in each municipality
###Ideally, we would want to estimate reproductive numbers, but that would be extremely effort-consuming for 300+ cases
###So we just see how steep the curve of accumulated confirmed cases gets up to now

casesandclimate_slope <-   casesandclimate_df %>%
  pivot_wider(names_from = "theline", values_from = "acum_cases") %>%
  mutate(today_case=lead(Today)) %>%
  mutate(days_since_5case=lead(days_since_5case)) %>%
  mutate(fifthcase_num=fifthcase) %>%
  dplyr::select(-Today,-fifthcase) %>%
  filter(fifthcase_num>=5) %>%
  mutate(slope=(today_case-fifthcase_num)/days_since_5case)
  


#3. And let's graph! I provide two scatterplots and a more experimental alternative -- see below

##First, scatterplots

humidity_scatter <- ggplot(data=casesandclimate_slope) +
  geom_point(aes(x=humidity, y=slope, size=cases, color=humidity, alpha=0.8))+ 
  geom_smooth(aes(x=humidity, y=slope, color=..x..), method=loess, se = FALSE, formula = y ~ x, size=1.5, alpha= 0.9) + 
  scale_color_viridis_c()+
  scale_y_continuous(breaks = c(0,5,10,15),limits=c(0,15)) +
  scale_x_continuous(breaks = c(15,35,55,75), limits = c(15,85)) +
  labs(title="Relación entre humedad \n y velocidad de la pandemia", subtitle="Cada burbuja representa un municipio en Colombia o México, \n y su tamaño equivale al número de casos de COVID-19 detectados", caption="@jorgegalindo, con datos de OpenWeatherMap (humedad el 30 de abril) y ECDC - Comisión Europea.", y="Inclinación de la curva de contagio hasta el 30 de abril (mayor=más acelerada)", x = "Humedad a 30 de abril (%)") +
  theme(panel.background = element_rect(fill="#ffffff"),
        axis.ticks = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill="#ffffff"),
        panel.grid.major = element_line(color = "#bbbbbb", size = 0.25, linetype = "dashed"),
        axis.title.x = element_text(family="Helvetica",face="italic", size=rel(1.1), vjust=-1, color="#777777"),
        axis.title.y = element_text(family="Helvetica",face="italic", size=rel(1.1), vjust=1, color="#777777"),
        axis.text = element_text(family="Helvetica", size=rel(1.1)),
        plot.title = element_text(family="Helvetica", face="bold", size=rel(2), hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size=rel(1.5), hjust = 0.5),
        plot.caption = element_text(family="Helvetica", size=rel(1), hjust = 0.5, color="#333333", margin = margin(30, 0, 0, 0)),
        plot.margin = margin(1, 1, 1, 1, "cm")
  )


temp_scatter <- ggplot(data=casesandclimate_slope) +
  geom_point(aes(x=temp, y=slope, size=cases, color=temp, alpha=0.8))+ 
  geom_smooth(aes(x=temp, y=slope, color=..x..), method=lm, se = FALSE, formula = y ~ x, size=1.5, alpha= 0.9) + 
  scale_color_viridis_c()+
  scale_y_continuous(breaks = c(0,5,10,15),limits=c(0,15)) +
  scale_x_continuous(breaks = c(10,20,30,40), limits = c(10,45)) +
  labs(title="Relación entre temperatura \n y velocidad de la pandemia", subtitle="Cada burbuja representa un municipio en Colombia o México, \n y su tamaño equivale al número de casos de COVID-19 detectados", caption="@jorgegalindo, con datos de OpenWeatherMap (temperatura el 30 de abril) y ECDC - Comisión Europea.", y="Inclinación de la curva de contagio hasta el 30 de abril (mayor=más acelerada)", x = "Temperatura a 30 de abril (c)") +
  theme(panel.background = element_rect(fill="#ffffff"),
        axis.ticks = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill="#ffffff"),
        panel.grid.major = element_line(color = "#bbbbbb", size = 0.25, linetype = "dashed"),
        axis.title.x = element_text(family="Helvetica",face="italic", size=rel(1.1), vjust=-1, color="#777777"),
        axis.title.y = element_text(family="Helvetica",face="italic", size=rel(1.1), vjust=1, color="#777777"),
        axis.text = element_text(family="Helvetica", size=rel(1.1)),
        plot.title = element_text(family="Helvetica", face="bold", size=rel(2), hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size=rel(1.5), hjust = 0.5),
        plot.caption = element_text(family="Helvetica", size=rel(1), hjust = 0.5, color="#333333", margin = margin(30, 0, 0, 0)),
        plot.margin = margin(1, 1, 1, 1, "cm")
  )

##Now, the experiment. I just draw the slopes divided by temp levels in different plots, and color them according to temp. 
##This is a half-cooked plotting idea so far but I believe it's got potential :)

temp_books <- ggplot(data = casesandclimate_df) + 
  geom_line(aes(x = theline, y = acum_cases, group = city, colour = temp), size=0.8) +
  facet_wrap(vars(temp_facet), ncol=2) +
  scale_y_log10()+
  scale_x_discrete(expand = c(0, 0))+
  scale_color_viridis(discrete = FALSE, option = "magma")+
  theme(panel.background = element_rect(fill="#ffffff"),
        axis.ticks = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill="#ffffff"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#dddddd", size=.25),
        panel.spacing = unit(4, "lines"),
        axis.text = element_blank(),
        axis.title = element_blank()
  )

