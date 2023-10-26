library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(httr)
library(here)

### historic scraping via github https://github.com/ebmdatalab/euctr-tracker-data/

oauth_endpoints("github")

EU_trackR <- oauth_app(appname = "EU_trials_trackR",
                       key = Sys.getenv("GITHUB_CLIENT_ID"),
                       secret = Sys.getenv("GITHUB_CLIENT_SECRET"))

github_token <- oauth2.0_token(oauth_endpoints("github"), EU_trackR)


gtoken <- config(token = github_token)


url <- "https://github.com/ebmdatalab/euctr-tracker-data/commits/master/all_sponsors.json"

history <- read_html(url)
dates <- history |>
  html_elements("h2") |>
  html_text()  |>
  str_remove_all("Commits on ")

dates <- lubridate::mdy(dates) |>
  na.omit()

SHAs <- history |>
  html_elements("clipboard-copy") |>
  html_attr("value") |>
  na.omit()



trials <- tibble(retrieval_date = dates, SHAs) |>
  mutate(url = paste0("https://github.com/ebmdatalab/euctr-tracker-data/raw/",
                      SHAs,
                      "/all_sponsors.json"))


euctr_names <- c(
  "rwth-aachen-university",
  "charite-universitatsmedizin-berlin",
  "ruhr-university-bochum",
  "university-of-bonn",
  "dresden-university-of-technology",
  "university-duisburg-essen",
  "heinrich-heine-university-dusseldorf",
  "university-erlangen-nuremberg",
  "goethe-university",
  "university-of-freiburg",
  "university-of-giessen",
  "university-of-gottingen",
  "medical-university-greifswald",
  "martin-luther-university-halle-wittenberg",
  "university-of-hamburg",
  "hannover-medical-school",
  "heidelberg-university-hospital",
  "saarland-university",
  "friedrich-schiller-university-jena",
  "university-of-cologne",
  "leipzig-university",
  "university-of-munich-ludwig-maximilians",
  "otto-von-guericke-university-magdeburg",
  "johannes-gutenberg-university-of-mainz",
  "central-institute-of-mental-health-in-mannheim", # dubious that this is the right institution!
  "philipps-university-marburg",
  "university-of-munster",
  "university-of-regensburg",
  "universitat-rostock",
  "schleswig-holstein-university-hospital",
  "technical-university-of-munich",
  "university-hospital-tubingen",
  "university-of-ulm",
  "university-of-wittenherdecke",
  "julius-maximilian-university-of-wurzburg"
)

institutions <- c(
  "Aachen",
  "Berlin",
  "Bochum",
  "Bonn",
  "Dresden",
  "Duisburg-Essen",
  "Düsseldorf",
  "Erlangen",
  "Frankfurt",
  "Freiburg",
  "Giessen",
  "Göttingen",
  "Greifswald",
  "Halle-Wittenberg",
  "Hamburg",
  "Hannover",
  "Heidelberg",
  "Homburg",
  "Jena",
  "Köln",
  "Leipzig",
  "LMU-München",
  "Magdeburg",
  "Mainz",
  "Mannheim",
  "Marburg",
  "Münster",
  "Regensburg",
  "Rostock",
  "Schleswig-Holstein",
  "TU-München",
  "Tübingen",
  "Ulm",
  "Witten/Herdecke",
  "Würzburg"
  )

institutions <- tibble(UMC = institutions, euctr_name = euctr_names)


var_names <- c("euctr_name", "total_unreported", "total_reported", "total_due",
               "not_yet_due_trials", "inconsistent_trials", "total_trials")

retrieve_euct_data <- function(url, institutions) {

  req <- httr::GET(url, gtoken)
  httr::stop_for_status(req)

  # Extract content from a request
  json1 <- httr::content(req) |>  stringr::str_replace_all("NaN", "0")

  # Convert to a data.frame
  gitDF <- jsonlite::fromJSON(json1, flatten = TRUE) |>
    dplyr::filter(slug %in% institutions) |>
    dplyr::rename(euctr_name = slug) |>
    dplyr::select(dplyr::all_of(var_names))

  Sys.sleep(2) #pause to let connection work
  closeAllConnections()
  gc()

  gitDF
}

institutions_trials <- trials |>
  mutate(trial_data = map(url, \(x) retrieve_euct_data(x, euctr_names), .progress = TRUE))


scraped_trials <- institutions_trials |>
  unnest(trial_data) |>
  select(retrieval_date, all_of(var_names))

scraped_trials <- scraped_trials |>
  left_join(institutions, by = "euctr_name") |>
  select(UMC, everything()) |>
  arrange(retrieval_date)


previous_data <- read_csv(here("data", "EU_trialstracker.csv"))

new_trials <- scraped_trials |>
  # add_row(UMC = "My Uni", retrieval_date = as.Date("2024-01-01"), euctr_name = "cool_uni", total_unreported = 0,
  #         total_reported = 10, total_due = 10, not_yet_due_trials = 2, inconsistent_trials = 0, total_trials = 12) |>
  anti_join(previous_data)

write_excel_csv(new_trials, here("data", "EU_trialstracker.csv"), append = TRUE)

# proportion of summary results reporting within a year are total_reported / total_due


#TODO: automate this on a server
