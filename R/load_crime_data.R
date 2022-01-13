
#' Load Chicago Crime Dataset and Subset by Time/Date and Crime Type
#'
#' @param chicago_data_path String specifying the path of the Chicago crime dataset CSV as available from https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2
#' @param start_time A start datetime of the form "yyyy-mm-dd hh:mm:ss" where "hh" is 24 hour format, local time with daylight savings.
#' @param end_time An end datetime of the same form as `start_time`. Note that if only a date is provided, time will be set to 00:00:00, meaning the day specified in `end_time` will not be included.
#' @param crime Either a string or a list of strings each specifying a crime type as stored in the `Primary.Type` column of the Chicago crime dataset. The possible crimes are DECEPTIVE PRACTICE, THEFT,
#' CRIMINAL DAMAGE, ASSAULT, BATTERY, OTHER OFFENSE, NARCOTICS, WEAPONS VIOLATION, MOTOR VEHICLE THEFT, CRIMINAL TRESPASS, BURGLARY, INTERFERENCE WITH PUBLIC OFFICER, ARSON, ROBBERY, CRIM SEXUAL ASSAULT,
#' CRIMINAL SEXUAL ASSAULT, PROSTITUTION, PUBLIC PEACE VIOLATION, OFFENSE INVOLVING CHILDREN, LIQUOR LAW VIOLATION, CONCEALED CARRY LICENSE VIOLATION, SEX OFFENSE, STALKING, GAMBLING, HOMICIDE, OBSCENITY,
#' INTIMIDATION, KIDNAPPING, HUMAN TRAFFICKING, NON-CRIMINAL, OTHER NARCOTIC VIOLATION, PUBLIC INDECENCY.
#'
#' @return A dataframe containing the crime data between the specified times / dates and of the specified types(s), where the datetimes have been converted to POSIXct format. The data is cleaned of any entries where location data is missing or set as a default value.
#' @export
#'
#' @importFrom dplyr "%>%"
load_crime_data = function(chicago_data_path, start_time = NULL, end_time=NULL, crime=NULL){
  chicago_data = as.data.frame(utils::read.csv(chicago_data_path)) %>% dplyr::mutate(Date = lubridate::mdy_hms(Date, tz="America/Chicago"))
  chicago_data = chicago_data %>% tidyr::drop_na(Primary.Type,Longitude,Latitude) #Remove NA values in the important columns

  if(! is.null(start_time)){
    chicago_data = chicago_data %>% dplyr::filter(Date >= lubridate::as_datetime(start_time, tz="America/Chicago"))
  }
  if(! is.null(end_time)){
    chicago_data = chicago_data %>% dplyr::filter(Date <= lubridate::as_datetime(end_time, tz="America/Chicago"))
  }

  if(! is.null(crime)){
    chicago_data = chicago_data %>% dplyr::filter(Primary.Type %in% crime)
  }

  chicago_data = chicago_data %>% dplyr::filter(X.Coordinate != 0 & Y.Coordinate != 0) #Remove entries where location has been set to a default

  return(chicago_data)
}
