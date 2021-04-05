devtools::load_all()

baer_pdb <- read_pdb("../RPadrino/data-raw/Baer_2018_pdb.xlsx")

usethis::use_data(baer_pdb, overwrite = TRUE)
