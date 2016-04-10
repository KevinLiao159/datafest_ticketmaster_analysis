library(dplyr)
library(lubridate)
library(ggmap)
library(caret)
library(data.table)
library(reshape2)
library(rpart)
library(rattle)
library(party)

buyTicketDuringPresale <- function(predate, onsaledate, buydate) {
  
  if (is.null(predate)) {
    return(FALSE)
  }
  
  return(buydate >= predate & buydate <= onsaledate)
  
}




getLongLatDist <- function(df) {
  print("work1")
  df$fin_mkt_nm <- toupper(df$fin_mkt_nm)
  if (length(df$venue_city[!grepl(pattern="\\>$", df$venue_city)])!=0) {
    df$venue_city[!grepl(pattern="\\>$", df$venue_city)] <- "QUEBEC"
  }
  print("work2")
  distance <- function (lon1, lat1, lon2, lat2){
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- lon1 * rad
    b1 <- lat2 * rad
    b2 <- lon2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
  }
  
  tmp <- df %>% dplyr::select(venue_city) %>% dplyr::group_by(venue_city) %>% dplyr::summarise(n = n())
  tmp1 <- df %>% dplyr::select(fin_mkt_nm) %>% dplyr::group_by(fin_mkt_nm) %>% dplyr::summarise(n = n())
  tmp <- as.data.frame(tmp) %>% dplyr::select(venue_city)
  tmp1 <- as.data.frame(tmp1) %>% dplyr::select(fin_mkt_nm)
  
  long_lat <- as.data.frame(sapply(tmp$venue_city, geocode))
  long_lat1 <- as.data.frame(sapply(tmp1$fin_mkt_nm, geocode))
  
  a <- as.data.table(t(long_lat))
  b <- cbind(a, tmp)
  names(b) <- c("lon_ven_city", "lat_ven_city", "venue_city")
  df <- dplyr::left_join(df,b)
  
  c <- as.data.table(t(long_lat1))
  d <- cbind(c, tmp1)
  names(d) <- c("lon_fin", "lat_fin", "fin_mkt_nm")
  df <- as.data.table(dplyr::left_join(df,d))
  
  
  df$distance <- apply(df %>% dplyr::select(lon_fin, lat_fin, lon_ven_city, lat_ven_city), 1, function(x){
    distance(x$lon_fin, x$lat_fin, x$lon_ven_city, x$lat_ven_city)
  })
  
  return(df)
}
