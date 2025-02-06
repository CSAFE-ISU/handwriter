devtools::load_all()

# Added an alpha layer to the png file with Photoshop

actual <- readPNGBinary(testthat::test_path("fixtures", "alpha_channel", "w0030_s03_pWOZ_r01.png"))
saveRDS(actual, testthat::test_path("fixtures", "alpha_channel", "alpha.rds"))
