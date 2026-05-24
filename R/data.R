#' A test dataset for the mars package.
#'
#' A dataset of size N=100 with n=10 explanatory variables,
#' and a response variable that depends on only the first two
#' explanatory variables.
#'
#' @format A data frame with 100 rows and 11 variables:
#' \describe{
#'   \item{y}{response variable}
#'   \item{x1}{explanatory variable}
#'   \item{x2}{explanatory variable}
#'   \item{x3}{explanatory variable}
#'   \item{x4}{explanatory variable}
#'   \item{x5}{explanatory variable}
#'   \item{x6}{explanatory variable}
#'   \item{x7}{explanatory variable}
#'   \item{x8}{explanatory variable}
#'   \item{x9}{explanatory variable}
#'   \item{x10}{explanatory variable}
#' }
"marstestdata"

#' @title Spotify song data
#' @description A dataset of 350 songs with 15 variables, including popularity, duration, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, and time signature.
#' @format A data frame with 350 rows and 15 variables:
#' \describe{
#'   \item{X}{index}
#'   \item{popularity}{track popularity score 0-100}
#'   \item{duration_ms}{track duration in milliseconds}
#'   \item{danceability}{danceability score 0-1}
#'   \item{energy}{energy level 0-1}
#'   \item{key}{musical key}
#'   \item{loudness}{loudness in dB}
#'   \item{mode}{major or minor mode}
#'   \item{speechiness}{speechiness score 0-1}
#'   \item{acousticness}{acousticness score 0-1}
#'   \item{instrumentalness}{instrumentalness score 0-1}
#'   \item{liveness}{liveness score 0-1}
#'   \item{valence}{musical positivity score 0-1}
#'   \item{tempo}{beats per minute}
#'   \item{time_signature}{time signature}
#'}
#' @source \url{https://www.kaggle.com/datasets/maharshipandya/spotify-tracks-dataset}
"spotify"


#' @title Airbnb Dataset
#' @description A dataset of 337 Airbnb listings in New York City, with 9 variables including latitude, longitude, room type, price, minimum nights, number of reviews, reviews per month, calculated host listings count, and availability in the next 365 days.
#' @format A data frame with 337 rows and 9 variables:
#' \describe{
#'   \item{latitude}{latitude coordinate}
#'   \item{longitude}{longitude coordinate}
#'   \item{room_type}{type of room listed}
#'   \item{price}{nightly price in USD}
#'   \item{minimum_nights}{minimum nights required}
#'   \item{number_of_reviews}{number of reviews}
#'   \item{reviews_per_month}{average reviews per month}
#'   \item{calculated_host_listings_count}{number of listings by host}
#'   \item{availability_365}{days available per year}
#'}
#'
#' @source \url{https://www.kaggle.com/datasets/dgomonov/new-york-city-airbnb-open-data/}
"airbnb"

#' @title Diabetes Dataset
#' @description A dataset of 767 patients with diabetes, with 9 variables including number of pregnancies, plasma glucose concentration, diastolic blood pressure, skin thickness, insulin level, body mass index, diabetes pedigree function, age, and diabetes outcome.
#' @format A data frame with 767 rows and 9 variables:
#' \describe{
#'   \item{X6}{number of pregnancies}
#'   \item{X148}{plasma glucose concentration}
#'   \item{X72}{diastolic blood pressure}
#'   \item{X35}{triceps skin fold thickness}
#'   \item{X0}{2-hour serum insulin}
#'   \item{X33.6}{body mass index}
#'   \item{X0.627}{diabetes pedigree function}
#'   \item{X50}{age in years}
#'   \item{X1}{diabetes diagnosis (0=no, 1=yes)}
#' }
#' @source \url{https://machinelearningmastery.com}
"diabetes"
