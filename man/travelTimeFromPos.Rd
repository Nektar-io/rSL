\name{travelTimeFromPos}
\alias{travelTimeFromPos}
\title{Get travel time from current position}
\usage{
travelTimeFromPos(WGS, destinations = c(9001), Time = "12:00",
  Date = strftime(today(), "\%d.\%m.\%Y"), verbose = FALSE)
}
\arguments{
  \item{coords}{A vector of WGS coordinates on [northing,
  easting] format for the original point of travel}

  \item{destinations}{The destination station IDs we want
  to calculate travel times to. Default is 9001, which is
  T-Centralen. Other examples include 9192 (Slussen) and
  9117 (Odenplan).}

  \item{time}{A character string on the "HH:MM" format.}

  \item{date}{A character string on the "DD.MM.YYYY"
  format.}
}
\description{
Get travel time (including walking distance) in minutes
from current position to central places in Stockholm
}

