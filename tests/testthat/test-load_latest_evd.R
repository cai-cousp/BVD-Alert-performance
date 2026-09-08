library(testthat)

source(here::here("alert_helpers/load_latest_evd.R"))

test_that("load_latest_evd loads the file with the latest numeric timestamp", {
  tmp <- tempfile("evd_out")
  dir.create(tmp, recursive = TRUE)

  old <- tibble::tibble(id = 1L, v = "old")
  new <- tibble::tibble(id = 2L, v = "new")
  saveRDS(old, file.path(tmp, "evd.clean_Int_2026_08_10_1200.rds"))
  saveRDS(new, file.path(tmp, "evd.clean_Int_2026_08_13_0903.rds"))

  expect_equal(load_latest_evd(tmp)$v, "new")

  unlink(tmp, recursive = TRUE)
})

test_that("load_latest_evd searches recursively through dated folders", {
  tmp <- tempfile("evd_rec")
  dir.create(tmp, recursive = TRUE)

  nested <- file.path(tmp, "13_August", "evd.cleaning")
  dir.create(nested, recursive = TRUE)
  saveRDS(tibble::tibble(x = 1L),
          file.path(nested, "evd.clean_Int_2026_08_13_0901.rds"))

  expect_equal(load_latest_evd(tmp)$x, 1L)

  unlink(tmp, recursive = TRUE)
})

test_that("pattern argument selects contact files and excludes contact.long", {
  tmp <- tempfile("ct_out")
  dir.create(tmp, recursive = TRUE)

  wide <- tibble::tibble(id_contact = 1L)
  long <- tibble::tibble(id_contact = 2L)
  saveRDS(wide, file.path(tmp, "contact.clean_Int_2026_08_13_0903.rds"))
  saveRDS(long, file.path(tmp, "contact.long.clean_Int_2026_08_13_0903.rds"))

  res <- load_latest_evd(tmp, pattern = "contact.clean_Int_.*\\.rds")
  expect_equal(res$id_contact, 1L)

  unlink(tmp, recursive = TRUE)
})

test_that("load_latest_evd errors when no file matches the pattern", {
  tmp <- tempfile("evd_empty")
  dir.create(tmp)

  expect_error(
    load_latest_evd(tmp),
    "No cleaned data file matching pattern"
  )

  unlink(tmp, recursive = TRUE)
})

test_that("load_latest_evd keeps classification_finale when already present", {
  tmp <- tempfile("evd_norm")
  dir.create(tmp)

  d <- tibble::tibble(
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif"
  )
  saveRDS(d, file.path(tmp, "evd.clean_Int_2026_08_13_0903.rds"))

  res <- load_latest_evd(tmp)
  expect_true("classification_finale" %in% names(res))
  expect_false("classification_finale_cas" %in% names(res))

  unlink(tmp, recursive = TRUE)
})

test_that("load_latest_evd normalizes classification_finale_cas to classification_finale", {
  tmp <- tempfile("evd_keep")
  dir.create(tmp)

  d <- tibble::tibble(
    classification_finale_cas = "Cas confirmé",
    lab_resultat_final = "Positif"
  )
  saveRDS(d, file.path(tmp, "evd.clean_Int_2026_08_13_0903.rds"))

  res <- load_latest_evd(tmp)
  expect_true("classification_finale" %in% names(res))
  expect_false("classification_finale_cas" %in% names(res))

  unlink(tmp, recursive = TRUE)
})
