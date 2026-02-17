## Progetto Data Mining, Perrelli Adalgisa

# Librerie e funzioni -----------------------------------------------------

library(skimr)
library(ggplot2)
library(dplyr)
library(visdat)
library(sf)
library(ggspatial)
library(tidygeocoder)
library(geosphere)
library(viridis)
library(stringr)
library(tidyr)
library(purrr)
library(pls)
library(leaps)
library(broom)
library(glmnet)
library(lars)
library(corrplot)
library(caret)
library(mgcv)

righe_con_na <- function(ds, variabile) {
  ds[is.na(ds[[variabile]]), ]
}

tab_contingenza <- function(dati, target_col, pred_col) {
  if (is.character(target_col)) {
    target_name <- target_col
    target_data <- dati[[target_col]]
  } else {
    target_name <- deparse(substitute(target_col))
    target_data <- target_col
  }
  
  if (is.character(pred_col)) {
    pred_name <- pred_col
    pred_data <- dati[[pred_col]]
  } else {
    pred_name <- deparse(substitute(pred_col))
    pred_data <- pred_col
  }
  
  complete_cases <- complete.cases(pred_data, target_data)
  x <- pred_data[complete_cases]  # predictor
  y <- target_data[complete_cases]  # to predict
  
  tab_contingenza <- table(x, y)
  cat("contingency table:\n")
  print(tab_contingenza)
  cat("\n")
  
  levels_x <- rownames(tab_contingenza)
  levels_y <- colnames(tab_contingenza)
  
  best_pred <- character(length(levels_x))
  names(best_pred) <- levels_x
  
  mat_prob <- matrix(0,
                     nrow = nrow(tab_contingenza), 
                     ncol = ncol(tab_contingenza))
  rownames(mat_prob) <- levels_x
  colnames(mat_prob) <- levels_y
  
  for (i in seq_along(levels_x)) {
    curr_lev <- levels_x[i]
    freq <- tab_contingenza[curr_lev, ]
    tot_freq <- sum(freq)
    
    if (tot_freq > 0) {
      mat_prob[i, ] <- as.numeric(freq) / tot_freq
      piu_probabile <- names(which.max(freq))
      best_pred[curr_lev] <- piu_probabile
    }
  }
  
  cat("conditional probabilities P(y|x):\n")
  print(round(mat_prob, 3))
  cat("\n")
  
  fun_previsione <- function(new_data, prob_restituita = FALSE) {
    if (is.character(pred_col)) {
      new_x <- new_data[[pred_col]]
    } else {
      new_x <- new_data[[pred_name]]
    }
    
    if (prob_restituita) {
      risultato <- mat_prob[as.character(new_x), , drop = FALSE]
    } else {
      risultato <- best_pred[as.character(new_x)]
    }
    
    return(risultato)
  }
  
  pred_training <- fun_previsione(dati[complete_cases, ])
  acc <- mean(pred_training == y, na.rm = TRUE)
  cat("accuracy:", round(acc, 3), "\n\n")
  
  return(list(
    predici = fun_previsione,
    matrice_probabilita = mat_prob,
    predizioni_migliori = best_pred,
    accuratezza = acc,
    tabella_contingenza = tab_contingenza
  ))
}

MAE <- function(y, y_fit){
  mean(abs(y - y_fit))
}

predict_regsubsets_matrix <- function(object, X_new, id) {
  s    <- summary(object)
  vars <- names(which(s$which[id, ]))
  has_int <- "(Intercept)" %in% vars
  vars <- setdiff(vars, "(Intercept)")
  
  beta <- coef(object, id = id)
  X_sub <- X_new[, vars, drop = FALSE]
  
  y_hat <- as.numeric(X_sub %*% beta[vars])
  if (has_int) y_hat <- y_hat + beta["(Intercept)"]
  y_hat
}
# Caricamento del dataset -------------------------------------------------

train <- read.csv("training.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
train$dataset_type <- "train"
test$dataset_type <- "test"

test$selling_price <- NA
ID_final <- test$ID

ds_intero <- rbind(train, test)
str(ds_intero)
skim(ds_intero)
freq_missing <- apply(ds_intero, 2, function(x) sum(is.na(x)))
freq_missing[freq_missing > 0]

ds_intero <- subset(ds_intero, select = -c(ID))
names(ds_intero)
# Imputazione dei dati mancanti -------------------------------------------

## availability ----
table(ds_intero$availability, useNA = "always")
ds_intero$availability <- ifelse(is.na(ds_intero$availability), 0, 1)

## rooms_number ----
ds_intero$rooms_number[ds_intero$rooms_number == "5+"] <- "6"
ds_intero$rooms_number <- as.numeric(ds_intero$rooms_number)

## bathrooms_number ----
ds_intero$bathrooms_number[ds_intero$bathrooms_number == "3+"] <- "4"
ds_intero$bathrooms_number <- as.numeric(ds_intero$bathrooms_number)

ds_intero$bathrooms_number[ds_intero$bathrooms_number > ds_intero$rooms_number & 
                           !is.na(ds_intero$bathrooms_number) & 
                           !is.na(ds_intero$rooms_number)] <- NA

mod_bath <- tab_contingenza(ds_intero, "bathrooms_number", "rooms_number")

na_indices <- is.na(ds_intero$bathrooms_number)
if(sum(na_indices) > 0) {
  ds_intero$bathrooms_number[na_indices] <- mod_bath$predici(ds_intero[na_indices, ])
}

ds_intero$bathrooms_number <- as.numeric(ds_intero$bathrooms_number)

## square_meters ----
boxplot(ds_intero$square_meters)
summary(ds_intero$square_meters)
table(ds_intero$square_meters, useNA = "always")
area_small <- ds_intero[!is.na(ds_intero$square_meters) & ds_intero$square_meters <= 12, ] # unrealistic square meters
ds_intero$square_meters[!is.na(ds_intero$square_meters) & ds_intero$square_meters <= 12] <- NA

tot_rooms <- ds_intero$bathrooms_number + ds_intero$rooms_number
sqmt_per_room <- ds_intero$square_meters/tot_rooms
boxplot(sqmt_per_room)
summary(sqmt_per_room)

area_big <- ds_intero[ds_intero$square_meters >= 600 & !is.na(ds_intero$square_meters), ]
# 800 square meters for 1 bathroom + 1 room & 800 square meters in duomo
ds_intero$square_meters[!is.na(ds_intero$square_meters) & ds_intero$square_meters == 800] <- NA
summary(ds_intero$square_meters)

mod_sm <- lm(log(square_meters) ~ rooms_number + bathrooms_number, data = ds_intero)
summary(mod_sm)

missing_indices <- is.na(ds_intero$square_meters)
pred_sm <- exp(predict(mod_sm, newdata = ds_intero[missing_indices, ]))
summary(pred_sm)
summary(ds_intero$square_meters)
ds_intero$square_meters[missing_indices] <- pred_sm

## lift ----
table(ds_intero$lift, useNA = "always")
righe_con_na(ds_intero, "lift")

period_of_construction <- unique(quantile(ds_intero$year_of_construction, probs = seq(0, 1, 0.2), na.rm = TRUE))
etichette <- paste0("Quantile ", seq_along(period_of_construction[-1]))
ds_intero <- ds_intero %>%
  mutate(period_of_con = cut(year_of_construction, breaks = period_of_construction, include.lowest = TRUE, labels = etichette))

lift_num_floors <- ds_intero %>%
  filter(!is.na(lift)) %>%
  group_by(total_floors_in_building, lift, period_of_con) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(total_floors_in_building, period_of_con)

lift_plot_data <- lift_num_floors %>%
  group_by(total_floors_in_building, period_of_con) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()
ggplot(lift_plot_data, aes(x = as.factor(total_floors_in_building), y = percent, fill = lift)) +
  geom_col(position = "fill") +
  facet_wrap(~ period_of_con) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Presenza di ascensore per numero piani e periodo di costruzione",
    x = "Numero di piani",
    y = "Percentuale",
    fill = "Ascensore"
  ) +
  theme_minimal()
ds_intero <- ds_intero %>%
  mutate(
    lift = case_when(
      is.na(lift) & is.na(period_of_con) & is.na(total_floors_in_building) ~ NA_character_,
      is.na(lift) & (period_of_con %in% c("Quantile 1", "Quantile 2", "Quantile 3") & total_floors_in_building %in% 1:3) ~ "no",
      is.na(lift) & (period_of_con == "Quantile 4" & total_floors_in_building %in% 1:2) ~ "no",
      is.na(lift) & (period_of_con == "Quantile 5" & total_floors_in_building == 1) ~ "no",
      is.na(lift) ~ "yes",
      TRUE ~ lift
    )
  )

ds_intero$period_of_con <- NULL

## conditions ----
table(ds_intero$conditions, useNA = "always")

righe_con_na(ds_intero, "conditions")

# imputare NA in base a "other_features":
names(ds_intero) <- trimws(names(ds_intero)) # rimuove spazi iniziali/finali
names(ds_intero) <- gsub("\\s+", " ", names(ds_intero)) # spazi multipli → 1 spazio

# è stata osservata la presenza di variabili dummy avanti lo stesso nome:
dups <- names(ds_intero)[duplicated(names(ds_intero))]
for (nm in unique(dups)) {
  same_cols_idx <- which(names(ds_intero) == nm)
  if (length(same_cols_idx) > 1) {
    ds_intero[[nm]] <- apply(ds_intero[, same_cols_idx], 1, function(x) if (any(x == 1, na.rm = TRUE)) 1 else 0)
    keep <- same_cols_idx[1]
    drop <- same_cols_idx[-1]
    ds_intero <- ds_intero[, -drop]
  }
}
any(duplicated(names(ds_intero))) # duplicati rimossi

is_new <- (
  (ds_intero$`window frames in triple glass / pvc` == 1 |
     ds_intero$`window frames in triple glass / pvcdouble exposure` == 1 |
     ds_intero$`optic fiber` == 1) &
    (ds_intero$`alarm system` == 1 |
       ds_intero$`video entryphone` == 1 |
       ds_intero$`electric gate` == 1)
)

is_excellent <- (
  (ds_intero$`security door` == 1 | ds_intero$`alarm system` == 1) &
    (ds_intero$`window frames in double glass / wood` == 1 |
       ds_intero$`window frames in triple glass / pvc` == 1 |
       ds_intero$`hydromassage` == 1 |
       ds_intero$`fireplace` == 1) &
    (ds_intero$`furnished` == 1 | ds_intero$`kitchen` == 1)
)

is_good <- (
  (ds_intero$`balcony` == 1 | ds_intero$`cellar` == 1) &
    (ds_intero$`window frames in glass / wood` == 1 |
       ds_intero$`window frames in double glass / metal` == 1) &
    (ds_intero$`partially furnished` == 1 | ds_intero$`single tv system` == 1)
)

to_be_refurbished <- (
  (ds_intero$`optic fiber` == 0 &
     ds_intero$`security door` == 0 &
     ds_intero$`alarm system` == 0) &
    (ds_intero$`window frames in glass / metal` == 1 |
       ds_intero$`window frames in glass / wood` == 1) &
    (ds_intero$`furnished` == 0 & ds_intero$`partially furnished` == 0)
)

ds_intero$conditions[is.na(ds_intero$conditions) & is_new] <- "new / under construction"
ds_intero$conditions[is.na(ds_intero$conditions) & is_excellent] <- "excellent / refurbished"
ds_intero$conditions[is.na(ds_intero$conditions) & is_good] <- "good condition / liveable"
ds_intero$conditions[is.na(ds_intero$conditions) & to_be_refurbished] <- "to be refurbished"

# sono rimasti 70 valori mancanti; per l'imputazione di questi ultimi considero l'anno di costruzione:
cond_by_year <- ds_intero %>%
  filter(!is.na(conditions)) %>%
  group_by(year_of_construction) %>%
  summarise(cond_moda = names(sort(table(conditions), decreasing = TRUE))[1])

ds_intero <- ds_intero %>%
  left_join(cond_by_year, by = "year_of_construction") %>%
  mutate(conditions = ifelse(is.na(conditions), cond_moda, conditions)) %>%
  dplyr::select(-cond_moda)

## other_features ----
features_list <- strsplit(as.character(ds_intero$other_features), " \\| ")
feature_freq  <- table(unlist(features_list), useNA = "always")
feature_freq

ds_intero$partially_furnished <- as.numeric(grepl(
  "only kitchen furnished|partially furnished",
  ds_intero$other_features, ignore.case = TRUE))

ds_intero$fully_furnished <- as.numeric(
  grepl("furnished", ds_intero$other_features, ignore.case = TRUE) &
    !ds_intero$partially_furnished)

table(ds_intero$fully_furnished, ds_intero$partially_furnished)

# features per lo score
ds_intero$balconies <- as.numeric(grepl("balcony|balconies",
                                        ds_intero$other_features, ignore.case = TRUE))
ds_intero$garden <- as.numeric(grepl("private garden|shared garden|private and shared garden",
                                     ds_intero$other_features, ignore.case = TRUE))
ds_intero$cellar <- as.numeric(grepl("cellar", ds_intero$other_features, ignore.case = TRUE))
ds_intero$alarm <- as.numeric(grepl("alarm system", ds_intero$other_features, ignore.case = TRUE))
ds_intero$tv <- as.numeric(grepl("tv system", ds_intero$other_features, ignore.case = TRUE))
ds_intero$closet <- as.numeric(grepl("closet", ds_intero$other_features, ignore.case = TRUE))
ds_intero$electric_gate <- as.numeric(grepl("electric gate", ds_intero$other_features, ignore.case = TRUE))
ds_intero$concierge_disabled_access <- as.numeric(grepl("concierge|disabled access",
                                                        ds_intero$other_features, ignore.case = TRUE))
ds_intero$optic_fiber <- as.numeric(grepl("optic fiber", ds_intero$other_features, ignore.case = TRUE))
ds_intero$security_reception <- as.numeric(grepl("security door|reception",
                                                 ds_intero$other_features, ignore.case = TRUE))
ds_intero$terrace <- as.numeric(grepl("terrace", ds_intero$other_features, ignore.case = TRUE))
ds_intero$video_entryphone <- as.numeric(grepl("video entryphone",
                                               ds_intero$other_features, ignore.case = TRUE))
ds_intero$tavern <- as.numeric(grepl("tavern", ds_intero$other_features, ignore.case = TRUE))
ds_intero$pool <- as.numeric(grepl("pool", ds_intero$other_features, ignore.case = TRUE))
ds_intero$tennis <- as.numeric(grepl("tennis court", ds_intero$other_features, ignore.case = TRUE))
ds_intero$fireplace <- as.numeric(grepl("fireplace", ds_intero$other_features, ignore.case = TRUE))
ds_intero$hydromassage <- as.numeric(grepl("hydromassage",
                                           ds_intero$other_features, ignore.case = TRUE))

mod_weights <- lm(selling_price ~ balconies + garden + cellar + alarm + tv + closet +
                  electric_gate + concierge_disabled_access + optic_fiber + security_reception + 
                  terrace + video_entryphone + fireplace + tavern + tennis + pool + hydromassage,
                  data = ds_intero)
summary(mod_weights)

ds_intero$features_score <- with(ds_intero,
                                 75265 * balconies +
                                 (-90816) * garden +
                                 59580 * cellar +
                                 219560 * alarm +
                                 (-69577) * tv +
                                 83436 * closet +
                                 (-72231) * electric_gate +
                                 160841 * concierge_disabled_access +
                                 (-33826) * optic_fiber +
                                 44256 * security_reception +
                                 183318 * terrace +
                                 32505 * video_entryphone +
                                 344243 * fireplace +
                                 120280 * tavern +
                                 (-217872) * tennis +
                                 191683 * pool +
                                 178178 * hydromassage)

# normalizzazione
ds_intero$features_score <- (ds_intero$features_score - min(ds_intero$features_score, na.rm = TRUE)) /
                             (max(ds_intero$features_score, na.rm = TRUE) - min(ds_intero$features_score, na.rm = TRUE))

hist(ds_intero$features_score, 
     col = "#80cdc1",
     main = "Histogram of features_score",
     xlab = "score")

# exposure
ds_intero$exposure <- as.numeric(grepl("exposure",
                                       ds_intero$other_features, ignore.case = TRUE))

fun_exposure_score <- function(data) {
  data$exposure_score <- 1
  internal <- grepl("internal exposure", data$other_features, ignore.case = TRUE)
  external <- grepl("external exposure", data$other_features, ignore.case = TRUE)
  south <- grepl("exposure.*south", data$other_features, ignore.case = TRUE)
  east <- grepl("exposure.*east", data$other_features, ignore.case = TRUE)
  west <- grepl("exposure.*west", data$other_features, ignore.case = TRUE)
  north <- grepl("exposure.*north", data$other_features, ignore.case = TRUE)
  double <- grepl("double exposure", data$other_features, ignore.case = TRUE)
  
  exposure_category <- rep("none", nrow(data))
  exposure_category[internal] <- "internal"
  exposure_category[external] <- "external"
  exposure_category[north] <- "north"
  exposure_category[west] <- "west"
  exposure_category[east] <- "east"
  exposure_category[south] <- "south"
  exposure_category[double] <- "double"
  
  scores <- c(
    "none" = 1.0,
    "internal" = 1.2,
    "external" = 1.8,
    "north" = 2.0,
    "west" = 2.5,
    "east" = 3.0,
    "south" = 4.0,
    "double" = 4.5
  )
  
  data$exposure_score <- scores[exposure_category]
  data$exposure_score
}

ds_intero$exposure_score <- fun_exposure_score(ds_intero)
table(ds_intero$exposure_score)

# windows
ds_intero$double_glass <- as.numeric(grepl("double glass",
                                           ds_intero$other_features, ignore.case = TRUE))
ds_intero$triple_glass <- as.numeric(grepl("triple glass",
                                           ds_intero$other_features, ignore.case = TRUE))
ds_intero$window_pvc   <- as.numeric(grepl("pvc",
                                           ds_intero$other_features, ignore.case = TRUE))
ds_intero$window_metal <- as.numeric(grepl("metal",
                                           ds_intero$other_features, ignore.case = TRUE))
ds_intero$window_wood  <- as.numeric(grepl("wood",
                                           ds_intero$other_features, ignore.case = TRUE))

sum(!ds_intero$window_metal & !ds_intero$window_pvc & !ds_intero$window_wood)
sum(!ds_intero$double_glass & !ds_intero$triple_glass)

ds_intero <- subset(ds_intero, select = -c(other_features))

## total_floors_in_building ----
table(ds_intero$total_floors_in_building, useNA = "always")

ds_intero <- ds_intero %>%
  mutate(total_floors_in_building = str_replace(total_floors_in_building, "1 floor", "1"))
ds_intero$total_floors_in_building <- as.integer(ds_intero$total_floors_in_building)

hist(ds_intero$total_floors_in_building)

righe_con_na(ds_intero, "total_floors_in_building")

righe_da_analizzare <- subset(ds_intero, lift == "no" & is.na(total_floors_in_building))

ds_intero$total_floors_in_building[
  is.na(ds_intero$total_floors_in_building) &
    ds_intero$lift == "no" &
    ds_intero$condominium_fees == "No condominium fees"
] <- 1

ds_intero$total_floors_in_building[
  is.na(ds_intero$total_floors_in_building) &
    ds_intero$lift == "no" &
    ds_intero$condominium_fees != "No condominium fees"
] <- 2

ds_intero$total_floors_in_building[
  is.na(ds_intero$total_floors_in_building) &
    ds_intero$floor %in% c("ground floor", "mezzanine", "semi-basement")
] <- 2

ds_intero$total_floors_in_building[is.na(ds_intero$total_floors_in_building)] <- as.integer(ds_intero$floor[is.na(ds_intero$total_floors_in_building)])

## car_parking ----
table(ds_intero$car_parking)

peso_garage <- 2
peso_shared <- 1

ds_intero$parking_score <- sapply(ds_intero$car_parking, function(x) {
  if (x == "no" | is.na(x)) {
    return(0)
  } else {
    n_garage <- sum(as.numeric(unlist(regmatches(x, gregexpr("[0-9]+(?= in garage/box)", x, perl=TRUE)))))
    n_shared <- sum(as.numeric(unlist(regmatches(x, gregexpr("[0-9]+(?= in shared parking)", x, perl=TRUE)))))
    if (grepl("in garage/box", x) & n_garage == 0) n_garage <- 1
    if (grepl("in shared parking", x) & n_shared == 0) n_shared <- 1
    
    score <- n_garage * peso_garage + n_shared * peso_shared
    return(score)
  }
})

summary(ds_intero$parking_score)

ds_intero$car_parking <- NULL

## condominium_fees ----
table(ds_intero$condominium_fees, useNA = "always")

ds_intero <- ds_intero %>%
  mutate(condominium_fees = str_replace(condominium_fees, "No condominium fees", "0"))
ds_intero <- ds_intero %>%
  mutate(condominium_fees = str_replace(condominium_fees, "NaN  ", "NA"))
ds_intero$condominium_fees <- as.integer(ds_intero$condominium_fees)

summary(ds_intero$condominium_fees)

righe_da_analizzare <- righe_con_na(ds_intero, "condominium_fees")

ds_intero$condominium_fees[ds_intero$condominium_fees %in% c(250000, 200000, 110000)] <- ds_intero$condominium_fees[ds_intero$condominium_fees %in% c(250000, 200000, 110000)] / 1000

ds_intero$condominium_fees[is.na(ds_intero$condominium_fees) & ds_intero$total_floors_in_building == 1] <- 0

ds_intero <- ds_intero %>%
  group_by(zone) %>%
  mutate(
    condominium_fees = ifelse(
      is.na(condominium_fees),
      median(condominium_fees, na.rm = TRUE),
      condominium_fees
    )
  ) %>%
  ungroup()

## year_of_construction ----
summary(ds_intero$year_of_construction)

righe_con_na(ds_intero, "year_of_construction")

current_year <- 2025
ds_intero$building_age <- current_year - ds_intero$year_of_construction
ds_intero <- ds_intero %>%
  group_by(zone, energy_efficiency_class) %>%
  mutate(
    building_age = ifelse(
      is.na(building_age),
      median(building_age, na.rm = TRUE),
      building_age
    )
  ) %>%
  ungroup()
ds_intero <- ds_intero %>%
  group_by(zone) %>%
  mutate(
    building_age = ifelse(
      is.na(building_age),
      median(building_age, na.rm = TRUE),
      building_age
    )
  ) %>%
  ungroup()
ds_intero$building_age[is.na(ds_intero$building_age)] <-
  median(ds_intero$building_age, na.rm = TRUE)
ds_intero$year_of_construction <- NULL

## zone ----
table(ds_intero$zone, useNA = "ifany")
ds_intero[which(is.na(ds_intero$zone)), ]

ds_intero %>%
  filter(!is.na(zone)) %>%
  filter (
    square_meters >= 90 & square_meters <= 110, 
    selling_price >= 450000 & selling_price <= 550000,
    conditions == "excellent / refurbished",
  ) %>%
  count(zone, sort = TRUE)

ds_intero$zone[is.na(ds_intero$zone)] <- "città studi"

loc.poly<-st_read("A090101_ComuneMilano.shp",quiet=TRUE) 
ggplot(data = st_boundary(loc.poly)) + 
  geom_sf()
loc.poly

unique_zones <- sort(unique(ds_intero$zone))

# spatial grouping for exploratory analysis
ds_intero <- ds_intero %>%
  mutate(zone_omi = case_when(
    zone == "affori" ~ "BOVISASCA, AFFORI, P. ROSSI , COMASINA",
    zone == "amendola - buonarroti" ~ "SEMPIONE, PAGANO, WASHINGTON",
    zone == "arco della pace" ~ "PARCO SEMPIONE, ARCO DELLA PACE, CORSO MAGENTA",
    zone == "arena" ~ "PARCO SEMPIONE, ARCO DELLA PACE, CORSO MAGENTA",
    zone == "argonne - corsica" ~ "PIOLA, ARGONNE, CORSICA",
    zone == "ascanio sforza" ~ "SOLARI, P.TA GENOVA, ASCANIO SFORZA",
    zone == "baggio" ~ "BAGGIO, Q. ROMANO, MUGGIANO",
    zone == "bande nere" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "barona" ~ "BARONA, FAMAGOSTA, FAENZA",
    zone == "bicocca" ~ "SARCA, BICOCCA",
    zone == "bignami - ponale" ~ "NIGUARDA, BIGNAMI, PARCO NORD",
    zone == "bisceglie" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "bocconi" ~ "TABACCHI, SARFATTI, CREMA",
    zone == "bologna - sulmona" ~ "TITO LIVIO, TERTULLIANO, LONGANESI",
    zone == "borgogna - largo augusto" ~ "VENEZIA, PORTA VITTORIA, PORTA ROMANA",
    zone == "bovisa" ~ "BOVISA, BAUSAN, IMBONATI",
    zone == "bovisasca" ~ "BOVISASCA, AFFORI, P. ROSSI , COMASINA",
    zone == "brera" ~ "CENTRO STORICO - BRERA",
    zone == "bruzzano" ~ "BOVISASCA, AFFORI, P. ROSSI , COMASINA",
    zone == "buenos aires" ~ "PISANI, BUENOS AIRES, REGINA GIOVANNA",
    zone == "ca' granda" ~ "NIGUARDA, BIGNAMI, PARCO NORD",
    zone == "cadore" ~ "LIBIA, ,XXII MARZO, INDIPENDENZA",
    zone == "cadorna - castello" ~ "CENTRO STORICO -SANT`AMBROGIO, CADORNA, VIA DANTE",
    zone == "cantalupa - san paolo" ~ "BARONA, FAMAGOSTA, FAENZA",
    zone == "carrobbio" ~ "CENTRO STORICO -UNIVERSITA STATALE, SAN LORENZO",
    zone == "cascina dei pomi" ~ "GALLARATESE, LAMPUGNANO, P. TRENNO, BONOLA",
    zone == "cascina gobba" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "cascina merlata - musocco" ~ "MUSOCCO, CERTOSA, EXPO, C.NA MERLATA",
    zone == "casoretto" ~ "PIOLA, ARGONNE, CORSICA",
    zone == "cenisio" ~ "CENISIO, FARINI, SARPI",
    zone == "centrale" ~ "STAZIONE CENTRALE VIALE STELVIO",
    zone == "cermenate - abbiategrasso" ~ "BARONA, FAMAGOSTA, FAENZA",
    zone == "certosa" ~ "MUSOCCO, CERTOSA, EXPO, C.NA MERLATA",
    zone == "chiesa rossa" ~ "MAROCCHETTI, VIGENTINO, CHIESA ROSSA",
    zone == "cimiano" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "città studi" ~ "PIOLA, ARGONNE, CORSICA",
    zone == "city life" ~ "CITY LIFE",
    zone == "comasina" ~ "BOVISASCA, AFFORI, P. ROSSI , COMASINA",
    zone == "corso genova" ~ "PORTA TICINESE, PORTA GENOVA, VIA SAN VITTORE",
    zone == "corso san gottardo" ~ "PORTA TICINESE, PORTA GENOVA, VIA SAN VITTORE",
    zone == "corso magenta" ~ "PARCO SEMPIONE, ARCO DELLA PACE, CORSO MAGENTA",
    zone == "corvetto" ~ "ORTLES, SPADOLINI, BAZZI",
    zone == "crescenzago" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "de angeli" ~ "SEMPIONE, PAGANO, WASHINGTON",
    zone == "dergano" ~ "BOVISA, BAUSAN, IMBONATI",
    zone == "dezza" ~ "SEGESTA, ARETUSA, VESPRI SICILIANI",
    zone == "duomo" ~ "CENTRO STORICO -DUOMO, SANBABILA, MONTENAPOLEONE, MISSORI, CAIROLI",
    zone == "famagosta" ~ "BARONA, FAMAGOSTA, FAENZA",
    zone == "farini" ~ "CENISIO, FARINI, SARPI",
    zone == "figino" ~ "BAGGIO, Q. ROMANO, MUGGIANO",
    zone == "frua" ~ "SEMPIONE, PAGANO, WASHINGTON",
    zone == "gallaratese" ~ "GALLARATESE, LAMPUGNANO, P. TRENNO, BONOLA",
    zone == "gambara" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "garibaldi - corso como" ~ "PORTA NUOVA",
    zone == "ghisolfa - mac mahon" ~ "CENISIO, FARINI, SARPI",
    zone == "giambellino" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "gorla" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "gratosoglio" ~ "MISSAGLIA, GRATOSOGLIO",
    zone == "greco - segnano" ~ "SARCA, BICOCCA",
    zone == "guastalla" ~ "CENTRO STORICO -UNIVERSITA STATALE, SAN LORENZO",
    zone == "indipendenza" ~ "LIBIA, ,XXII MARZO, INDIPENDENZA",
    zone == "inganni" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "isola" ~ "PORTA NUOVA",
    zone == "istria" ~ "NIGUARDA, BIGNAMI, PARCO NORD",
    zone == "lambrate" ~ "LAMBRATE, RUBATTINO, ROMBON",
    zone == "lanza" ~ "CENTRO STORICO - BRERA",
    zone == "lodi - brenta" ~ "PORTA VIGENTINA, PORTA ROMANA",
    zone == "lorenteggio" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "largo caioroli 2" ~ "CENTRO STORICO -DUOMO, SANBABILA, MONTENAPOLEONE, MISSORI, CAIROLI",
    zone == "maggiolina" ~ "MAGGIOLINA, PARCO TROTTER, LEONCAVALLO",
    zone == "martini - insubria" ~ "LIBIA, ,XXII MARZO, INDIPENDENZA",
    zone == "melchiorre gioia" ~ "STAZIONE CENTRALE VIALE STELVIO",
    zone == "missori" ~ "CENTRO STORICO -DUOMO, SANBABILA, MONTENAPOLEONE, MISSORI, CAIROLI",
    zone == "molise - cuoco" ~ "TITO LIVIO, TERTULLIANO, LONGANESI",
    zone == "monte rosa - lotto" ~ "SEGESTA, ARETUSA, VESPRI SICILIANI",
    zone == "monte stella" ~ "IPPODROMO, CAPRILLI, MONTE STELLA",
    zone == "montenero" ~ "PORTA VIGENTINA, PORTA ROMANA",
    zone == "morgagni" ~ "PISANI, BUENOS AIRES, REGINA GIOVANNA",
    zone == "moscova" ~ "TURATI, MOSCOVA, CORSO VENEZIA",
    zone == "muggiano" ~ "BAGGIO, Q. ROMANO, MUGGIANO",
    zone == "navigli - darsena" ~ "SOLARI, P.TA GENOVA, ASCANIO SFORZA",
    zone == "niguarda" ~ "NIGUARDA, BIGNAMI, PARCO NORD",
    zone == "ortica" ~ "FORLANINI, MECENATE, ORTOMERCATO, SANTA GIULIA",
    zone == "pagano" ~ "SEMPIONE, PAGANO, WASHINGTON",
    zone == "palestro" ~ "TURATI, MOSCOVA, CORSO VENEZIA",
    zone == "paolo sarpi" ~ "CENISIO, FARINI, SARPI",
    zone == "parco lambro" ~ "PARCO LAMBRO, FELTRE, UDINE",
    zone == "parco trotter" ~ "MAGGIOLINA, PARCO TROTTER, LEONCAVALLO",
    zone == "pasteur" ~ "PISANI, BUENOS AIRES, REGINA GIOVANNA",
    zone == "pezzotti - meda" ~ "TABACCHI, SARFATTI, CREMA",
    zone == "piave - tricolore" ~ "VENEZIA, PORTA VITTORIA, PORTA ROMANA",
    zone == "piazza napoli" ~ "SOLARI, P.TA GENOVA, ASCANIO SFORZA",
    zone == "piazzale siena" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "plebisciti - susa" ~ "VENEZIA, PORTA VITTORIA, PORTA ROMANA",
    zone == "ponte lambro" ~ "RONCHETTO, CHIARAVALLE, RIPAMONTI",
    zone == "ponte nuovo" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "porta nuova" ~ "PORTA NUOVA",
    zone == "porta romana - medaglie d'oro" ~ "PORTA VIGENTINA, PORTA ROMANA",
    zone == "porta venezia" ~ "TURATI, MOSCOVA, CORSO VENEZIA",
    zone == "porta vittoria" ~ "VENEZIA, PORTA VITTORIA, PORTA ROMANA",
    zone == "portello - parco vittoria" ~ "MUSOCCO, CERTOSA, EXPO, C.NA MERLATA",
    zone == "prato centenaro" ~ "NIGUARDA, BIGNAMI, PARCO NORD",
    zone == "precotto" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "primaticcio" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "qt8" ~ "IPPODROMO, CAPRILLI, MONTE STELLA",
    zone == "quadrilatero della moda" ~ "CENTRO STORICO -DUOMO, SANBABILA, MONTENAPOLEONE, MISSORI, CAIROLI",
    zone == "quadronno - crocetta" ~ "PORTA VIGENTINA, PORTA ROMANA",
    zone == "quartiere adriano" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "quartiere feltre" ~ "PARCO LAMBRO, FELTRE, UDINE",
    zone == "quartiere forlanini" ~ "FORLANINI, MECENATE, ORTOMERCATO, SANTA GIULIA",
    zone == "quartiere olmi" ~ "BAGGIO, Q. ROMANO, MUGGIANO",
    zone == "quarto cagnino" ~ "GALLARATESE, LAMPUGNANO, P. TRENNO, BONOLA",
    zone == "quarto oggiaro" ~ "QUARTO OGGIARO, SACCO",
    zone == "quinto romano" ~ "BAGGIO, Q. ROMANO, MUGGIANO",
    zone == "quintosole - chiaravalle" ~ "RONCHETTO, CHIARAVALLE, RIPAMONTI",
    zone == "repubblica" ~ "STAZIONE CENTRALE VIALE STELVIO",
    zone == "ripamonti" ~ "ORTLES, SPADOLINI, BAZZI",
    zone == "rogoredo" ~ "ORTLES, SPADOLINI, BAZZI",
    zone == "roserio" ~ "QUARTO OGGIARO, SACCO",
    zone == "rovereto" ~ "MAGGIOLINA, PARCO TROTTER, LEONCAVALLO",
    zone == "rubattino" ~ "LAMBRATE, RUBATTINO, ROMBON",
    zone == "san babila" ~ "CENTRO STORICO -DUOMO, SANBABILA, MONTENAPOLEONE, MISSORI, CAIROLI",
    zone == "san carlo" ~ "LORENTEGGIO, INGANNI, BISCEGLIE, SAN CARLO B.",
    zone == "san siro" ~ "IPPODROMO, CAPRILLI, MONTE STELLA",
    zone == "san vittore" ~ "PORTA TICINESE, PORTA GENOVA, VIA SAN VITTORE",
    zone == "sant'ambrogio" ~ "CENTRO STORICO -SANT`AMBROGIO, CADORNA, VIA DANTE",
    zone == "santa giulia" ~ "FORLANINI, MECENATE, ORTOMERCATO, SANTA GIULIA",
    zone == "scala - manzoni" ~ "CENTRO STORICO -DUOMO, SANBABILA, MONTENAPOLEONE, MISSORI, CAIROLI",
    zone == "sempione" ~ "PARCO SEMPIONE, ARCO DELLA PACE, CORSO MAGENTA",
    zone == "solari" ~ "SOLARI, P.TA GENOVA, ASCANIO SFORZA",
    zone == "ticinese" ~ "CENTRO STORICO -UNIVERSITA STATALE, SAN LORENZO",
    zone == "tre castelli - faenza" ~ "BARONA, FAMAGOSTA, FAENZA",
    zone == "trenno" ~ "GALLARATESE, LAMPUGNANO, P. TRENNO, BONOLA",
    zone == "tripoli - soderini" ~ "BOVISASCA, AFFORI, P. ROSSI , COMASINA",
    zone == "turati" ~ "TURATI, MOSCOVA, CORSO VENEZIA",
    zone == "turro" ~ "LAMBRATE, RUBATTINO, ROMBON",
    zone == "udine" ~ "PARCO LAMBRO, FELTRE, UDINE",
    zone == "vercelli - wagner" ~ "SEMPIONE, PAGANO, WASHINGTON",
    zone == "via calizzano" ~ "GALLARATESE, LAMPUGNANO, P. TRENNO, BONOLA",
    zone == "via canelli" ~ "RONCHETTO, CHIARAVALLE, RIPAMONTI",
    zone == "via fra' cristoforo" ~ "BARONA, FAMAGOSTA, FAENZA",
    zone == "vialba" ~ "BOVISASCA, AFFORI, P. ROSSI , COMASINA",
    zone == "via marignano, 3" ~ "FORLANINI, MECENATE, ORTOMERCATO, SANTA GIULIA", 
    zone == "viale ungheria - mecenate" ~ "FORLANINI, MECENATE, ORTOMERCATO, SANTA GIULIA",
    zone == "vigentino - fatima" ~ "MAROCCHETTI, VIGENTINO, CHIESA ROSSA",
    zone == "villa san giovanni" ~ "MONZA, CRESCENZAGO, GORLA, QUARTIERE ADRIANO",
    zone == "vincenzo monti" ~ "CITY LIFE",
    zone == "washington" ~ "SEMPIONE, PAGANO, WASHINGTON",
    zone == "zara" ~ "SARCA, BICOCCA",
    #TRUE ~ NA_character_
  ))

table(ds_intero$zone_omi, useNA = "ifany")
length(table(ds_intero$zone_omi, useNA = "ifany"))

# to visualize the average selling price across zones
train$zone_omi <- ds_intero$zone_omi[1:8000]
test$zone_omi <- ds_intero$zone_omi[8001:nrow(ds_intero)]

avg_price<- train %>%
  group_by(zone_omi) %>%
  summarise(avg_price = mean(selling_price, na.rm = TRUE))

omi_poly <- st_read("_expl_f205-2024_1.geojson", quiet = TRUE)
omi_poly_2 <- st_zm(omi_poly)
colnames(omi_poly)
head(omi_poly)
data.sp=sp::merge(omi_poly_2, avg_price, by.y = "zone_omi", by.x ="Zona_Descr",all.x=T) 

map0 <- ggplot(data = data.sp) +
  geom_sf(aes(fill = avg_price, geometry = geometry), color = "white", size = 0.2) +
  scale_fill_viridis(name = "Avg. Price (€)", na.value = "gray", option = "plasma",
                     direction = -1) +
  labs(
    title = "Average Selling Price by Zone",
  ) +
  annotation_north_arrow(which_north = "true", location = "tl", style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )
map0

# grouping smaller zones for variable usage
ds_intero <- ds_intero %>%
  mutate(zone = case_when(
    zone == "corso magenta" ~ "sant'ambrogio",
    zone == "largo caioroli 2" ~ "duomo", 
    zone == "via marignano, 3" ~ "porta venezia",
    zone == "via fra' cristoforo" ~ "ticinese",
    zone == "cascina gobba" ~ "cimiano",
    zone == "parco lambro" ~ "lambrate", 
    zone == "quadrilatero della moda" ~ "brera",
    zone == "via calizzano" ~ "san siro",
    zone == "via canelli" ~ "niguarda",
    zone == "scala - manzoni" ~ "duomo",
    zone == "qt8" ~ "gallaratese",
    zone == "molise - cuoco" ~ "cuoco",
    zone == "bologna - sulmona" ~ "sulmona",
    zone == "san vittore" ~ "san vittore carcere",
    zone == "ponte nuovo" ~ "via padova, zona ponte nuovo",
    zone == "vercelli - wagner" ~ "piazza wagner",
    zone == "cermenate - abbiategrasso" ~ "piazza abbiategrasso",
    TRUE ~ zone
  ))

### distance from duomo ###
duomo_coords <- data.frame(
  name = "Duomo di Milano",
  lon = 9.191383,
  lat = 45.464211
)

zone_tab <- table(ds_intero$zone)
names_vec <- names(zone_tab)
names_vec <- paste(names_vec, "Milano, Italia")
coords <- geo(names_vec, method = "arcgis")
coords <- as.data.frame(coords)
colnames(coords)[1] = 'zone'
coords$zone <- names_vec
summary(coords)
str(coords)

coords_clean <- coords %>%
  mutate(zone_clean = str_remove(zone, " Milano, Italia"))
coords_clean <- subset(coords_clean, select = -c(zone))
ds_intero <- ds_intero %>%
  left_join(coords_clean, by = c("zone" = "zone_clean"))

ds_intero$dist_duomo <- sapply(1:nrow(ds_intero), function(i) { # in meters
  distHaversine(
    c(ds_intero$long[i], ds_intero$lat[i]),
    c(duomo_coords$lon, duomo_coords$lat)
  )
})

print(unique(ds_intero[, c("zone", "dist_duomo")]))
cor(ds_intero$dist_duomo, ds_intero$selling_price, use = "complete")

### distance from the closest metro stop ###
MM<-st_read("tpl_metrofermate.shp",quiet=TRUE); st_crs(MM) 
data.sp_metro <- st_as_sf(ds_intero, coords = c("long", "lat"), crs = 4326, remove = FALSE)
data.sp_metro <- st_transform(data.sp_metro, crs = 32632)
MM <- st_transform(MM, crs = 32632)
distance <- st_distance(data.sp_metro, MM)
idx_min <- apply(distance, 1, which.min)
dist_min <- apply(distance, 1, min)
data.sp_metro <- data.sp_metro %>%
  mutate(
    distance_metro = as.numeric(dist_min),
  )
ds_intero <- st_drop_geometry(data.sp_metro)

print(unique(ds_intero[, c("zone", "distance_metro")]))

## floor ----
table(ds_intero$floor)

ds_intero$floor <- ifelse(ds_intero$floor == "semi-basement", -1,
                          ifelse(ds_intero$floor %in% c("ground floor", "mezzanine"), 0, ds_intero$floor))
ds_intero$floor <- as.integer(ds_intero$floor)
hist(ds_intero$floor)

## heating_centralized ----
table(ds_intero$heating_centralized, useNA = "always")

righe_con_na(ds_intero, "heating_centralized")

eta_soglia <- 35      # ~ edifici post-1990
piani_soglia <- 3
fee_soglia <- median(ds_intero$condominium_fees, na.rm = TRUE)
idx_na <- is.na(ds_intero$heating_centralized)

ds_intero$heating_centralized[idx_na] <- ifelse(
  ds_intero$building_age[idx_na] <= eta_soglia &
    ds_intero$total_floors_in_building[idx_na] > piani_soglia &
    ds_intero$condominium_fees[idx_na] > fee_soglia,
  "central",
  "independent"
)

## energy_efficiency_class ----
table(ds_intero$energy_efficiency_class, useNA = "always")

ds_intero$energy_efficiency_class <- trimws(ds_intero$energy_efficiency_class)
ds_intero$energy_efficiency_class[
  ds_intero$energy_efficiency_class %in% c("", ",")
] <- NA
table(ds_intero$energy_efficiency_class, useNA = "always")
ds_intero$energy_efficiency_grouped <- NA_character_

# HIGH: a, b
ds_intero$energy_efficiency_grouped[
  ds_intero$energy_efficiency_class %in% c("a", "b")
] <- "high"

# MEDIUM: c, d
ds_intero$energy_efficiency_grouped[
  ds_intero$energy_efficiency_class %in% c("c", "d")
] <- "medium"

# LOW: e, f, g
ds_intero$energy_efficiency_grouped[
  ds_intero$energy_efficiency_class %in% c("e", "f", "g")
] <- "low"

mode_fun <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA)
  names(which.max(table(ux)))
}

ds_intero <- ds_intero %>%
  group_by(zone, building_age) %>%
  mutate(
    energy_efficiency_grouped = ifelse(
      is.na(energy_efficiency_grouped),
      mode_fun(energy_efficiency_grouped),
      energy_efficiency_grouped
    )
  ) %>%
  ungroup()

global_mode <- mode_fun(ds_intero$energy_efficiency_grouped)

ds_intero$energy_efficiency_grouped[
  is.na(ds_intero$energy_efficiency_grouped)
] <- global_mode
ds_intero$energy_efficiency_grouped <- factor(
  ds_intero$energy_efficiency_grouped,
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

# Ultime operazioni sul dataset -------------------------------------------

ds_intero <- ds_intero %>%
  dplyr::select(
    -energy_efficiency_class,
    -zone_omi,
    -tavern,
    -pool,
    -tennis,
    -fireplace,
    -hydromassage
  )
for (var in c("lift", "availability", "conditions", "heating_centralized", "zone")){
  ds_intero[[var]] <- factor(ds_intero[[var]])
}
ds_intero$energy_efficiency_grouped <- as.numeric(ds_intero$energy_efficiency_grouped)

freq_missing <- apply(ds_intero, 2, function(x) sum(is.na(x)))
freq_missing[freq_missing > 0]
colnames(ds_intero)[caret::nearZeroVar(ds_intero)]
skim(ds_intero)
summary(ds_intero$selling_price[1:8000])

ds_train <- subset(ds_intero, dataset_type == "train")
test <- subset(ds_intero, dataset_type == "test")
str(ds_train)
str(test)

set.seed(123)
id_train <- sort(sample(1:nrow(ds_train), size = floor(0.75 * nrow(ds_train)), replace = FALSE))
id_valid <- setdiff(1:nrow(ds_train), id_train)
train <- ds_train[id_train, ]
validation <- ds_train[id_valid, ]

# Cambio della variabile risposta:
train$lpsm <- log(train$selling_price/train$square_meters)
train$selling_price <- NULL
validation$lpsm <- log(validation$selling_price/validation$square_meters)
actual_price <- validation$selling_price
validation$selling_price <- NULL

train$dataset_type <- NULL
validation$dataset_type <- NULL

numerical_data <- train[, sapply(train, is.numeric)]
cor_matrix <- cor(numerical_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

par(mfrow = c(1, 1))
plot(train$square_meters, train$lpsm,
     xlab = "square_meters", ylab = "Sale Price", pch = 16, cex = 0.8)
plot(train$floor, train$lpsm,
     xlab = "floor", ylab = "Sale Price", pch = 16, cex = 0.8)
plot(train$condominium_fees, train$lpsm,
     xlab = "condominium_fees", ylab = "Sale Price", pch = 16, cex = 0.8)
plot(train$rooms_number, train$lpsm,
     xlab = "rooms_number", ylab = "Sale Price", pch = 16, cex = 0.8)
boxplot(lpsm ~ lift, data = train)
boxplot(lpsm ~ conditions, data = train)
boxplot(lpsm ~ heating_centralized, data = train)

# Modelli -----------------------------------------------------------------

## Valore mediano ----
y_hat_median <- rep(median(train$lpsm), nrow(validation))
predicted_price <- exp(y_hat_median) * validation$square_meters
round(MAE(actual_price, predicted_price), 4) # 170608

## Primo modello ----
m1 <- lm(lpsm ~ lift + floor + condominium_fees + bathrooms_number + rooms_number + dist_duomo, data = train)
summary(m1)

y_hat_m1 <- predict(m1, newdata = validation)
summary(y_hat_m1)
predicted_price <- exp(y_hat_m1) * validation$square_meters
round(MAE(actual_price, predicted_price), 4) # 121621.8

## Modello completo ----
m_full <- lm(lpsm ~ ., data = train)
summary(m_full)

y_hat_m_full <- predict(m_full, newdata = validation)
summary(y_hat_m_full)
predicted_price <- exp(y_hat_m_full) * validation$square_meters
round(MAE(actual_price, predicted_price), 4) # 81446.97

## Backward regression ----
X_train <- model.matrix(lpsm ~ ., data = train)[, -1]
p_max <- dim(X_train)[2]
# Individuo e rimuovo le colonne risultate collineari:
cols_ldep <- caret::findLinearCombos(X_train)$linearCombos
cols_ldep
to_drop <- unique(unlist(cols_ldep))
X_nondep <- X_train[, -to_drop, drop = FALSE]
p_max_br <- ncol(X_nondep)

m_backward <- regsubsets(x = X_nondep, y = train$lpsm,
                         method = "backward",
                         nbest = 1, nvmax = p_max_br)
sum_backward <- summary(m_backward)

which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates

X_valid <- model.matrix(lpsm ~ ., data = validation)[, -1]
X_valid <- X_valid[, colnames(X_nondep), drop = FALSE]  # stesse colonne del train

resid_back     <- matrix(0, nrow(validation), p_max_br + 1)
resid_log_back <- matrix(0, nrow(validation), p_max_br + 1)

# modello nullo
mod_null_lpsm <- lm(lpsm ~ 1, data = train)
y_hat0_lpsm   <- predict(mod_null_lpsm, newdata = validation)
y_hat0_price  <- exp(y_hat0_lpsm) * validation$square_meters

resid_back[, 1]     <- actual_price - y_hat0_price
resid_log_back[, 1] <- log(actual_price) - log(y_hat0_price)

for (j in 2:(p_max_br + 1)) {
  k <- j - 1
  y_hat_lpsm  <- predict_regsubsets_matrix(m_backward, X_valid, id = k)
  y_hat_price <- exp(y_hat_lpsm) * validation$square_meters
  
  resid_back[, j]     <- actual_price - y_hat_price
  resid_log_back[, j] <- log(actual_price) - log(y_hat_price)
}

data_cv <- data.frame(
  p    = 0:p_max_br,
  MAE  = apply(resid_back, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_back^2, 2, mean)
)

p_back_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_back_optimal # 23

par(mfrow = c(1, 2))
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6,
     ylab = "MAE (validation)", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MAE(actual_price, y_hat_price), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6,
     ylab = "MSLE", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")

y_hat_back_lpsm  <- predict_regsubsets_matrix(m_backward, X_valid,
                                              id = p_back_optimal)
y_hat_back_price <- exp(y_hat_back_lpsm) * validation$square_meters
MAE(actual_price, y_hat_back_price) # 139975.1

## PCR ----
m_pcr <- pcr(lpsm ~ ., data = train, center = TRUE, scale = TRUE, validation = "CV")
summary(m_pcr)
m_pcr$ncomp
msep_obj <- MSEP(m_pcr)
plot(msep_obj, legendpos = "topright")
ncomp_opt <- which.min(msep_obj$val[1,1,])
print(paste("Numero ottimale di componenti:", ncomp_opt))
MAE(exp(predict(m_pcr, newdata = validation, ncomp = 175))*validation$square_meters, actual_price) # 81446.96

## Ridge regression ----
X_shrinkage <- model.matrix(lpsm ~ ., data = train)[, -1]
y_shrinkage <- train$lpsm
lambda_ridge_grid <- exp(seq(-6, 6, length = 100))
m_ridge <- glmnet(X_shrinkage, y_shrinkage, alpha = 0, lambda = lambda_ridge_grid)

resid_ridge <- matrix(0, nrow(validation), length(lambda_ridge_grid))
resid_log_ridge <- matrix(0, nrow(validation), length(lambda_ridge_grid))

y_hat_ridge <- predict(m_ridge, newx = model.matrix(lpsm ~ ., data = validation)[, -1], s = lambda_ridge_grid)
for (j in 1:length(lambda_ridge_grid)) {
  resid_ridge[, j] <- validation$lpsm - y_hat_ridge[, j]
  resid_log_ridge[, j] <- log(validation$lpsm) - log(y_hat_ridge[, j])
}

data_cv <- data.frame(
  lambda = lambda_ridge_grid,
  MAE = apply(resid_ridge, 2, function(x) mean(abs(x)))
)

lambda_ridge_optimal <- lambda_ridge_grid[which.min(data_cv$MAE)]
lambda_ridge_optimal # [1] 0.006536829

plot(data_cv$lambda, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(lambda))
abline(v = log(lambda_ridge_optimal), lty = "dashed")

y_hat_ridge <- predict(m_ridge, newx = model.matrix(lpsm ~ ., data = validation)[, -1], s = lambda_ridge_optimal)
y_hat_ridge_trans <- exp(y_hat_ridge) * validation$square_meters
MAE(actual_price, y_hat_ridge_trans) # 81328.43

## Cross-validation for ridge regression
ridge_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid) # esegue cross-validation
par(mfrow = c(1, 1))
plot(ridge_cv) # risultati simili a quelli ottenuti dalla procedura manuale vista sopra

ridge_cv$lambda.min
ridge_cv$lambda.1se
# i risultati coincidono all'incirca con quelli ottenuti manualmente sopra

# MSLE for lambda.min and lambda.1se
ridge_cv$cvm[ridge_cv$index]

## Lasso regression ----
X_shrinkage <- model.matrix(lpsm ~ ., data = train)[, -1]
y_shrinkage <- train$lpsm
lambda_lasso_grid <- exp(seq(-6, 6, length = 100))

m_lasso <- glmnet(X_shrinkage, y_shrinkage, alpha = 1, lambda = lambda_ridge_grid)

resid_lasso <- matrix(0, nrow(validation), length(lambda_lasso_grid))
resid_log_lasso <- matrix(0, nrow(validation), length(lambda_lasso_grid))

y_hat_lasso <- predict(m_lasso, newx = model.matrix(lpsm ~ ., data = validation)[, -1], s = lambda_lasso_grid)
for (j in 1:length(lambda_lasso_grid)) {
  resid_lasso[, j] <- validation$lpsm - y_hat_lasso[, j]
  resid_log_lasso[, j] <- log(validation$lpsm) - log(y_hat_lasso[, j])
}

data_cv <- data.frame(
  lambda = lambda_lasso_grid,
  MAE = apply(resid_lasso, 2, function(x) mean(abs(x)))
)

lambda_lasso_optimal <- lambda_lasso_grid[which.min(data_cv$MAE)]
lambda_lasso_optimal # [1] 0.002478752

plot(data_cv$lambda, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(lambda))
abline(v = log(lambda_lasso_optimal), lty = "dashed")

y_hat_lasso <- predict(m_lasso, newx = model.matrix(lpsm ~ ., data = validation)[, -1], s = lambda_lasso_optimal)
y_hat_lasso_trans <- exp(y_hat_lasso) * validation$square_meters
MAE(actual_price, y_hat_lasso_trans) # 82112.92

## LARS ----
m_lar <- lars(X_shrinkage, y_shrinkage, type = "lar")
m_lar

plot(m_lar$df, m_lar$Cp, type = "b", xlab = "Degrees of freedom", ylab = "Cp of Mallow")
abline(v = which.min(m_lar$Cp)) # 156
# linea molto piatta da 150 degrees of freedom

y_hat_lar <- predict(m_lar, newx = model.matrix(lpsm ~ ., data = validation)[, -1], 
                     s = which.min(m_lar$Cp))$fit
y_hat_lar_trans <- exp(y_hat_lar)*validation$square_meters
MAE(actual_price, y_hat_lar_trans) # 81341.84

## Elastic-net ----
# penalità 0.5
lambda_en_grid <- exp(seq(-10, 0, length = 100))
m_en <- glmnet(X_shrinkage, y_shrinkage, alpha = 0.5, lambda = lambda_en_grid)

resid_en <- matrix(0, nrow(validation), length(lambda_en_grid))
resid_log_en <- matrix(0, nrow(validation), length(lambda_en_grid))
y_hat_en <- predict(m_en, newx = model.matrix(lpsm ~ ., data = validation)[, -1], s = lambda_en_grid)

for (j in 1:length(lambda_en_grid)) {
  resid_en[, j] <- validation$lpsm - y_hat_en[, j]
  resid_log_en[, j] <- log(validation$lpsm) - log(y_hat_en[, j])
}

data_cv <- data.frame(
  lambda = lambda_en_grid,
  MAE = apply(resid_en, 2, function(x) mean(abs(x)))
)

lambda_en_optimal <- lambda_en_grid[which.min(data_cv$MAE)]
lambda_en_optimal #  0.0009399377

y_hat_en <- predict(m_en, newx = model.matrix(lpsm ~ ., data = validation)[, -1], s = lambda_en_optimal)
y_hat_en_trans <- exp(y_hat_en)*validation$square_meters
MAE(actual_price, y_hat_en_trans) #  81371.02

## GAM ----
# simple GAM
m_gam_simple <- gam(lpsm ~ s(square_meters) + s(dist_duomo) + s(distance_metro) + s(building_age), 
                    data = train)
summary(m_gam_simple)
plot(m_gam_simple, scale = 0, se = FALSE)

m_gam <- gam(lpsm ~ s(square_meters) + s(dist_duomo) + s(distance_metro) + s(building_age) + 
                    bathrooms_number + rooms_number + s(total_floors_in_building) + 
                    s(condominium_fees) + floor + s(lat) + s(long) + 
                    features_score + exposure_score + zone + conditions + 
                    heating_centralized + energy_efficiency_grouped + lift + 
                    parking_score + garden + terrace + concierge_disabled_access, data = train, select = TRUE)
summary(m_gam)
plot(m_gam, scale = 0, se = FALSE)

y_hat_gam_simple <- predict(m_gam_simple, newdata = validation)
y_hat_gam_simple_trans <- exp(y_hat_gam_simple) * validation$square_meters
MAE_gam_simple <- mean(abs(actual_price - y_hat_gam_simple_trans)) # 107412.6

# GAM completo
y_hat_gam <- predict(m_gam, newdata = validation)
y_hat_gam_trans <- exp(y_hat_gam) * validation$square_meters
MAE_gam <- mean(abs(actual_price - y_hat_gam_trans)) # 79117.7

k_list <- sapply(m_gam$smooth, function(x) x$bs.dim)
k_list

## Risultati GAM su test ----
data <- ds_train
data$lpsm <- log(data$selling_price/data$square_meters)
data$selling_price <- NULL

m_gam_final <- gam(lpsm ~ s(square_meters) + s(dist_duomo) + s(distance_metro) + s(building_age) + 
                   bathrooms_number + rooms_number + s(total_floors_in_building) + 
                   s(condominium_fees) + floor + s(lat) + s(long) + 
                   features_score + exposure_score + zone + conditions + 
                   heating_centralized + energy_efficiency_grouped + lift + 
                   parking_score + garden + terrace + concierge_disabled_access, data = data, select = TRUE)
summary(m_gam_final)

y_hat_gam_test <- predict(m_gam_final, newdata = test)
test$prediction <- exp(y_hat_gam_test) * test$square_meters

res_gam <- data.frame(
  ID = ID_final,
  prediction = test$prediction
)

write.csv(res_gam, "submission_Perrelli.csv", row.names = FALSE)



