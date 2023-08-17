install.packages('rjson', repos = 'http://cran.us.r-project.org')
library('jsonlite')

credentials <- list()
secretsJson <- read_json('.env.json')
print(secretsJson)
# Loop would technically make sense but other than 5 of them, 
# they all have differences in arguments passed

# member_prod
host <- secretsJson$member_prod$host
dbname <- secretsJson$member_prod$dbname
user <-  secretsJson$member_prod$user
pwd <-  secretsJson$member_prod$pwd
type <- secretsJson$member_prod$type
credentials$member_prod <- data.frame(host, dbname, user, pwd, type, stringsAsFactors = F)

# unity_prod
user <- secretsJson$unity_prod$user
pwd <- secretsJson$unity_prod$pwd
host <- secretsJson$unity_prod$host
dbname <- secretsJson$unity_prod$dbname
type <- secretsJson$unity_prod$type
credentials$unity_prod <- data.frame(host, dbname, user, pwd, type, stringsAsFactors = F)

# mint_prod
host <- secretsJson$mint_prod$host
dbname <- secretsJson$mint_prod$dbname
user <- secretsJson$mint_prod$user
pwd <- secretsJson$mint_prod$pwd
type <- secretsJson$mint_prod$type
credentials$mint_prod <- data.frame(host, dbname, user, pwd, type, stringsAsFactors = F)

# memberapi_prod
host <- secretsJson$memberapi_prod$host
dbname <- secretsJson$memberapi_prod$dbname
user <-  secretsJson$memberapi_prod$user
pwd <-  secretsJson$memberapi_prod$pwd
type <- secretsJson$memberapi_prod$type
credentials$memberapi_prod <- data.frame(host, dbname, user, pwd, type, stringsAsFactors = F)

# hubspot_prod
host <- secretsJson$hubspot_prod$host
user <- secretsJson$hubspot_prod$user
pwd <- secretsJson$hubspot_prod$pwd
dbname <- secretsJson$hubspot_prod$dbname
type <- secretsJson$hubspot_prod$type
credentials$hubspot_prod <- data.frame(host, dbname, user, pwd, type, stringsAsFactors = F)

# redshift_prod
host <- secretsJson$redshift_prod$host
port <- secretsJson$redshift_prod$port
dbname <- secretsJson$redshift_prod$dbname
user <- secretsJson$redshift_prod$user
pwd <- secretsJson$redshift_prod$pwd
drv <- secretsJson$redshift_prod$drv
type <- secretsJson$redshift_prod$type
credentials$redshift_prod <- data.frame(host, dbname, user, pwd, port, drv, type, stringsAsFactors = F)

# hubspot_api
APIkey <- secretsJson$hubspot_api$APIkey
UserID <- secretsJson$hubspot_api$UserID
baseurl <- secretsJson$hubspot_api$baseurl
documentationUrl <- secretsJson$hubspot_api$documentationUrl
examplecall <- secretsJson$hubspot_api$examplecall
type <- secretsJson$hubspot_api$type
credentials$hubspot_api <- data.frame(APIkey, UserID, baseurl, documentationUrl, examplecall, type, stringsAsFactors = F)

# stripe_api
baseurl <- secretsJson$stripe_api$baseurl
authenticate <- secretsJson$stripe_api$authenticate
examplecall <- secretsJson$stripe_api$examplecall
type <- secretsJson$stripe_api$type
documentationUrl <- secretsJson$stripe_api$documentationUrl
credentials$stripe_api <- data.frame(authenticate, baseurl, documentationUrl, examplecall, type, stringsAsFactors = F)

# googleAnalytics_package
package <- secretsJson$googleAnalytics_package$package
profileId <- secretsJson$googleAnalytics_package$profileId
examplecall <- secretsJson$googleAnalytics_package$examplecall
documentationUrl <- secretsJson$googleAnalytics_package$documentationUrl
type <- secretsJson$googleAnalytics_package$type
credentials$googleAnalytics_package <- data.frame(package, profileId, documentationUrl, examplecall, type, stringsAsFactors = F)

credentials$createConnection <- function(cName){
  
  if(credentials[cName][[1]]$type == "MySQL"){
   
    mydb <-  dbConnect(MySQL(), user=credentials[cName][[1]]$user, password = credentials[cName][[1]]$pwd,
          dbname= credentials[cName][[1]]$dbname, host= credentials[cName][[1]]$host );
  }
  if(credentials[cName][[1]]$type == "PostgreSQL"){
    drv <- dbDriver("PostgreSQL");
    mydb <-  dbConnect(drv,host=credentials[cName][[1]]$host, port=credentials[cName][[1]]$port,
               dbname=credentials[cName][[1]]$dbname, user=credentials[cName][[1]]$user,
               password=credentials[cName][[1]]$pwd)
     
  }
  print("Please remember to close your connections")
  return(mydb)
}


credentials$googlemapAPI <- data.frame(APIkey = secretsJson$googlemapAPI$APIkey, 
                                       example = secretsJson$googlemapAPI$example,
                                       stringsAsFactors = F)

credentials$darkskyAPI <- data.frame(APIkey = secretsJson$darkskyAPI$APIkey, stringsAsFactors = F)

getGeoData_googleAPI <- function(location){
  googlemapAPI <- secretsJson$googlemapAPI$APIkey
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=", googlemapAPI))
  raw_data_2 <- fromJSON(geo_data)
  return(raw_data_2)
}


Sys.setenv(DARKSKY_API_KEY = secretsJson$darkskyAPI$APIkey)

get_weather <- function(city, state_code, local_start_time, time_zone){
  library("darksky")
  city_geo <-  try(geocode(paste0(city, ", ", state_code), source = "dsk"))
  f <- get_forecast_for(city_geo$lat, city_geo$lon, str_replace(local_start_time, ".000", ""))
  #print(f$hourly)
  start_t <-   floor_date(as.POSIXct(str_replace(local_start_time, "T", " ")), unit = "hours")
  if(!'precipType' %in% names(f)){ ##May not exist for historical records
    f$hourly$precipType <- NA
  }
  f <-  f$hourly[f$hourly$time >= start_t &   f$hourly$time <= start_t + 60*60*2  , c("time", "temperature", "summary", 
                                                                                      "precipProbability", "precipIntensity", "precipType" )]
  f$local_time <-  format(with_tz(f$time, tz=paste0("US/", ifelse(time_zone=="East", "Eastern", a$time_zone))), usetz = F)
  
  
  f$local_time <- paste0(ifelse(hour(f$local_time)>12, hour(f$local_time)-12, hour(f$local_time)), ":", format(as.POSIXct(f$local_time), usetz=F, format= "%M"), ifelse(hour(f$local_time)>11, "pm", "am") )
  
  #f[,c("local_time", "summary", "precipProbability", "precipIntensity", "precipType" ) ]
  
  names(f) <- c("time", "summary", "temp", "precip", "Intensity", "Type", "hour" ) 
  
  return(f[,c("hour", "summary", "temp", "precip", "Intensity", "Type" )  ])
}

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

killall_Dbconnections <- function(list){
  
  for(o in 1:length(list)){
    if(class(eval(parse(text=list[o])))[1] %in% c("MySQLConnection", "PostgreSQLConnection")) {
      print(paste0("Found ", o))
      #break
      dbDisconnect(eval(parse(text=list[o])))
      #rm(list=list[o])
    }
  }
  return("All Dbs are Closed")
}


applyPlayonChannels <- function(ga_data){
  
  state_associations_referrals <- c('ahsaa.com',
                                    'asaa.org',
                                    'aiaonline.org',
                                    'ahsaa.org',
                                    'cifstate.org',
                                    'cifccs.org',
                                    'www2.chsaa.org',
                                    'chsaa.org',
                                    'casciac.org',
                                    'fhsaa.org',
                                    'm.fhsaa.org',
                                    'ghsa.net',
                                    'event.ghsa.net',
                                    'idhsaa.org',
                                    'ihsa.org',
                                    'ihsaa.org',
                                    'iahsaa.org',
                                    'kshssa.org',
                                    'khsaa.org',
                                    'khsaa.tv',
                                    'lhsaa.org',
                                    'cdn.lhsaa.org',
                                    'mpa.cc',
                                    'mpssaa.org',
                                    'miaa.net',
                                    'miaa.ezstream.com',
                                    'miaa.ezstream.net',
                                    'mpssaa.org',
                                    'mhsaa.com',
                                    'mshsl.org',
                                    'misshsaa.com',
                                    'mshsaa.org',
                                    'mhsa.org',
                                    'nsaahome.org',
                                    'niaa.com',
                                    'nhiaa.org',
                                    'njsiaa.org',
                                    'nmact.org',
                                    'nysphsaa.org',
                                    'psal.org',
                                    'nchsaa.org',
                                    'ndhsaa.com',
                                    'ndhsaanow.com',
                                    'ohsaa.org',
                                    'm.ossaarankings.com',
                                    'ossaa.com',
                                    'ossaa.net',
                                    'osaa.org',
                                    'piaa.org',
                                    'riil.org',
                                    'schsl.org',
                                    'sdhsaa.com',
                                    'tssaasports.com',
                                    'tssaa.org',
                                    'brackets.tssaa.org',
                                    'tmsaa.tssaa.org',
                                    'uiltexas.org',
                                    'uil.utexas.edu',
                                    'tapps.biz',
                                    'uhsaa.org',
                                    'vhsl.org',
                                    'visaa.org',
                                    'wiaa.com',
                                    'wiaawi.org',
                                    'wvssac.org',
                                    'whsaa.org',
                                    'azpreps365.com',
                                    'chsaanow.com',
                                    'ciacsports.com')
  
  
  ga_data$POChannel <- NA
  
  ga_data$POChannel[grepl('^(display|cpm)$', ga_data$medium)] <- "Display"
  ga_data$POChannel[grepl('^(cpc|ppc)$', ga_data$medium)  &  
                      grepl('^(National - NFHS Brand|Foster - National - NFHS Brand|Bing - National - NFHS Brand)$', ga_data$campaign) & is.na(ga_data$POChannel)] <- "PSBrand"
  ga_data$POChannel[grepl('^(cpc|ppc)$', ga_data$medium)  & is.na(ga_data$POChannel)] <- "PSNonBrand"
  ga_data$POChannel[(ga_data$hasSocialSourceReferral=="Yes" | grepl("^(social|social-network|social-media|sm|social network|social media)$", ga_data$medium)) &
                      grepl("Paid", ga_data$campaign) & is.na(ga_data$POChannel) ] <- "Paid Social"
  ga_data$POChannel[(ga_data$hasSocialSourceReferral=="Yes" | grepl("^(social|social-network|social-media|sm|social network|social media)$", ga_data$medium)) &
                      is.na(ga_data$POChannel)] <- "Social Network"
  ga_data$POChannel[ grepl("^(cpc|ppc|cpv|cpa|cpp|content-text|affiliate)$", ga_data$medium) &
                       is.na(ga_data$POChannel)] <- "Other Advertising"
  
  ga_data$POChannel[ ga_data$medium=="referral" & ga_data$source %in% state_associations_referrals &
                       is.na(ga_data$POChannel)] <- "Referral - State Associations"
  
  ga_data$POChannel[ ga_data$medium=="referral" & grepl("maxpreps.com", ga_data$source) &
                       is.na(ga_data$POChannel)] <- "Referral - MaxPreps"
  
  ga_data$POChannel[ ga_data$medium=="referral" & grepl("cube", ga_data$source) &
                       is.na(ga_data$POChannel)] <- "Referral - Cube"
  
  ga_data$POChannel[ ga_data$medium=="referral"  &
                       is.na(ga_data$POChannel)] <- "Referral"
  
  ga_data$POChannel[ ga_data$medium=="email"  &
                       is.na(ga_data$POChannel)] <- "Email"
  
  ga_data$POChannel[ ga_data$medium=="organic" &
                       is.na(ga_data$POChannel)] <- "Organic Search"
  
  ga_data$POChannel[ ga_data$source=="(direct)"  & (ga_data$medium=="(not set)" | ga_data$medium=="(none)") &
                       is.na(ga_data$POChannel)] <- "Direct"
  
  ga_data$POChannel[ is.na(ga_data$POChannel)] <- "Other"
  
  return(ga_data)
}

rm(APIkey, authenticate, baseurl, dbname, documentationUrl, drv, examplecall, host, package, port, profileId, pwd, user, UserID)

