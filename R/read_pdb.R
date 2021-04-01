#' @rdname read_write
#' 
#' @title Reads/writes all sheets from a PadrinoDB file
#' 
#' @param file The path to the Padrino database object, or the path write
#' the \code{pdb} object to.
#' 
#' @return \code{read}: a list with Padrino tables. \code{write}: \code{file},
#' invisibly.
#' 
#' @importFrom readxl excel_sheets read_excel
#' @export


read_pdb <- function(file) {
  
  sheets <- readxl::excel_sheets(file)
  
  out <- lapply(sheets,
                FUN = function(x) readxl::read_excel(file, sheet = x))
  
  names(out) <- sheets
  
  return(out)
  
}

#' @rdname read_write
#' @param pdb A list object representing a Padrino database
#' 
#' 
#' @importFrom writexl write_xlsx
#' @export

write_pdb <- function(pdb, file) {
  
  writexl::write_xlsx(x = pdb, path = file, col_names = TRUE)
  
  invisible(file)
  
}
