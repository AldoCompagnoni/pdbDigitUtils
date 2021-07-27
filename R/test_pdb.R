#' Test a newly entered model
#' 
#' @param pdb The padrino database object
#' @param id The \code{ipm_id} you wish to test
#' @param iterations The number of iterations to run the model for.
#' @param tolerance The tolerance for differences between computed and stored
#' target to be considered the same. This is calculated as 
#' \code{tolerance * 10^(-target_prec)}, where \code{target_prec} is usually 
#' 2, but comes from Padrino itself. For example, \code{tolerance = 3} usually
#' allows for computed lambda to be considered the same if it is within +/- 0.03
#' of the target lambda value. 
#'
#' @return Either a data frame of errors and warnings, a set of lambdas to inspect by
#' hand (if there is not testTarget), or a message indicating the result of 
#' comparison with the TestTarget.
#' 
#' @description This function is still under development and may well be buggy.
#' Please file Issues in the GitHub repository describing the problems
#' you encounter so I can work on those: 
#' \url{https://github.com/levisc8/pdbDigitUtils/issues}.
#' 
#' @importFrom pander evals
#' @importFrom rlang list2 env :=
#' @importFrom RPadrino pdb_make_proto_ipm pdb_make_ipm
#' @export
#' 

test_model <- function(pdb, id, iterations = 100, tolerance = 2) {
  
  out <- list()
  errs <- data.frame(
    ipm_id = id,
    error  = NA_character_
  )
  
  ev_env <- rlang::env(pdb = pdb, use_id = id)
  
  temp <- pander::evals(
    "RPadrino::pdb_make_proto_ipm(pdb, ipm_id = use_id, det_stoch = 'det')",
    env = ev_env
  )
  
  if(!is.null(temp[[1]]$msg$errors) || 
     !is.null(temp[[1]]$msg$warnings)) {
    
    msgs <- paste("Warnings found: ", temp[[1]]$msg$warnings, 
                  "Errors found: ", temp[[1]]$msg$errors,
                  collapse = "; ")
    
    errs$error <- msgs
  } 
  
  # If there's a proto_ipm result, we can try building it
  if(!is.null(temp[[1]]$result)) {
    
    use_prot <- temp[[1]]$result
    
    make_args <- rlang::list2(!!id := list(iterate = TRUE,
                                           iterations = iterations))
    
    ev_env <- rlang::env(use_prot = use_prot,
                         make_args = make_args)
    
    test_ipm <- pander::evals(
      "RPadrino::pdb_make_ipm(use_prot, addl_args = make_args)",
      env = ev_env
    )
    
    
    if(!is.null(test_ipm[[1]]$msg$errors) || 
       !is.null(test_ipm[[1]]$msg$warnings)) {
      
      msgs <- paste(errs$error[!is.na(errs$error)],
                    "IPM Build Warnings found: ", test_ipm[[1]]$msg$warnings, 
                    "IPM Build Errors found: ", test_ipm[[1]]$msg$errors,
                    collapse = "; ")
      
      
      errs$error <- msgs
      
      if(is.null(test_ipm[[1]]$result)) return(errs)
      
    }
    
    if(!is.null(test_ipm[[1]]$result)) { # make_ipm success
      
      # Deterministic lambda only for now
      
      
      if(id %in% pdb$TestTargets$ipm_id) {
        
        .compare_targets(test_ipm[[1]]$result, pdb, id, "lambda", tolerance)
        
      } else {
        
        res <- lambda(test_ipm[[1]]$result[[1]], type_lambda = "last")
        
        res <- round(res, 4)
        
        if(!rlang::is_named(res)) {
          names(res) <- paste("lambda_", seq(1, length(res), 1), sep = "")
        }
        
        out <- paste("No test target found, here are lambdas:\n",
                     paste(paste(names(res), res, sep = ": "), 
                           collapse = "\n"))
        
        class(out) <- "pdb_missing_target"
        
        return(out)
        
      }
      
      
    }
    
  } else { # Proto IPM construction failure
    
    
    return(errs)
    
  }
  
  
  
}

#' @rdname test_model
#' @param x Output from \code{test_model}
#' @param ... Ignored
#' @export

print.pdb_missing_target <- function(x, ...) {
  
  cat(x)
  
  invisible(x)
  
}

#' @noRd

`%between%` <- function(x, y) {
  
  x >= y[1] & x <= y[2]
  
}

#' @noRd
#' @importFrom ipmr lambda

.compare_targets <- function(ipm, pdb, ipm_id, fun, tolerance) {
  
  ipm         <- ipm[[1]]
      
  target      <- pdb$TestTargets$target_value[pdb$TestTargets$ipm_id == ipm_id]
  
  target_prec <- pdb$TestTargets$precision[pdb$TestTargets$ipm_id == ipm_id]
  
  result      <- round(unlist(rlang::exec(fun, 
                                          ipm, 
                                          type_lambda = "last"), 
                              use.names = FALSE),
                       digits = target_prec)
  
  
  target_prec <- unique(target_prec)
  
  l_up <- target + (tolerance * 10 ^ (-target_prec))
  l_lo <- target - (tolerance * 10 ^ (-target_prec))
  
  test_res <- result %between% c(l_lo, l_up)
  
  if(test_res) {
    "Test passed, model is ready :)"
  } else {
    paste("Test failed. Differences between targets: ",
          paste(target - result, sep = "\n"))
  }
  
}