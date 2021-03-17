#' Reads all sheets from a PadrinoDB file
#' 
#' @param file The path to the Padrino database object.
#' 
#' @return a list with Padrino tables.
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
