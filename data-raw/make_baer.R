devtools::load_all()

baer_pdb <- read_pdb("../Padrino/padrino-database/xl/hand_cleaned_padrino.xlsx")

baer_pdb <- pdb_subset(baer_pdb, paste0("aaa", 312:318))

# baer_pdb <- read_pdb("../RPadrino/data-raw/Baer_2018_pdb.xlsx")

usethis::use_data(baer_pdb, overwrite = TRUE)
