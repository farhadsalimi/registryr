## code to prepare `DATASET` dataset goes here
# it's better to save these files and use them, rather than making them here
set.seed(4)

n_sites <- 20
n_patients <- 2000

fake_data <-
  tibble(patient_id = 1:n_patients) %>%
  dplyr::slice(rep(patient_id, each = n_sites)) %>%
  mutate(site_id = rep(1:n_sites, n_patients),
         sex = rbinom(dplyr::n(), 1, 0.5),
         sex_cat = dplyr::recode(sex,
                                 `0` = "Female",
                                 `1` = "Male"),
         age = rpois(dplyr::n(), 50),
         death_prob = 0.1 + 0.03 * sex + 0.02 * (age / 5),
         mean_los = 10 + 2 * sex + 3 * (age / 5),
         los = rnorm(dplyr::n(), mean_los, 5)) %>%
  rowwise() %>%
  mutate(dead = rbinom(1, 1, death_prob)) %>%
  ungroup() %>%
  arrange(site_id)

usethis::use_data(fake_data, fake_data, overwrite = TRUE)
