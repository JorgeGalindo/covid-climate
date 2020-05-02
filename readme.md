# COVID's relationship with climate conditions in Colombia and Mexico

This is just an ongoing personal R project aimed to see if there is any discernible relation at the municipal level for climate conditions (temperature and humidity) and the spread of COVID-19

Today (ie. last update) = 2 May 2020

## What you need

As of today, to run this you just need R and a couple basic libraries (Tidyverse, lubridate).

## What's in

You'll see one script and one folder: data. It contains what it seems to contain.


**The data folder** is where action comes from and goes to. **CO.csv** and **MX,csv** contain individual-level datasets for COVID cases in Colombia and Mexico. **municipiosmx.csv** is just a list of official statistical codes for Mexico municipalities. All the data comes from official sources (INEGI for the codes, [Secretaría de Salud](https://www.gob.mx/salud/documentos/datos-abiertos-152127) for Mexico's case dataset, and [Ministerio de Salud](https://www.datos.gov.co/Salud-y-Protecci-n-Social/Estado-de-Casos-de-Coronavirus-COVID-19-en-Colombi/6c4c-msrp) for Colombia's). Download corresponds to today (2 May 2020). 

**script.R** simply where the action happens. It takes municipal data and merges it with real-time weather data. Where does it come from? I downloaded it from OpenWeatherMap project with my own API (you can obtain yours by signing up!). In case you don't want or cannot sign up, I also left in the folder some weather data with its corresponding date of download: see data folder, files with the name structure **weather_______.csv** where ________ corresponds to the download date.


## What will be in (hopefully)


If I find the time, I might include some way to estimate real-time reproductive numbers for each municipality. I might also add a dataset containing temperatures and humidity levels for several days.


## Author

Jorge Galindo - [@JorgeGalindo](https://twitter.com/jorgegalindo)

To download and deal with OWM data, I use [the super-useful owmr package maintained by Stefan Kuethe](https://cran.r-project.org/web/packages/owmr/owmr.pdf). Stefan was kind enough to respond to an inquiry I made to him by providing a fundamental chunk of code to properly download OWM data. So we can consider him a necessary contributor to this little project.

## License

Free to use however you see fit! But I wish you read some specialized literature before getting too excited about trends, etc. I'm no epidemiologist myself, that's why I know you'll need it in case you are neither. I hope you can provide something useful to the public debate. I'm sure you will :)