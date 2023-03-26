
#' @title Candidate Models for regression
#'
#' @description Function creates a candate model formulas
#' on the basis of the varaible names.
#'
#' @param   variable_names- series of varaible names [paste0("x", 1:10)]
#' @param   model_size- number of independent variables in the model formula. Default [3].
#' @param   groups- same size as model_size variable.
#'                  Defines the potential grouping of the variables. Default  [NA].
#' @param   max_mix- the maximum number of repeated variables from the same group in the model. Default  [1]
#' @param   dv_name- name of the dependent variable used in the formula.
#' Default  ["y"].
#' @param   output_short- if the output should be limited to the formulas only.   [FALSE]
#'
#' @examples
#'
#' cand_mod0 <- candidate_models(variable_names = paste0("x", 1:100),
#' groups = c(rep(1,30),rep(2,30), rep(3,20), rep(4,20)),
#' model_size = 4,
#' max_mix = c(1))
#'
#' dim(cand_mod0$candidate_all)
#' dim(cand_mod0$candidate_by_grouping)
#'
#' cand_mod1 <- candidate_models(variable_names = paste0("x", 1:100),
#'                               groups = c(rep(1,30),rep(2,30), rep(3,20), rep(4,20)),
#'                               model_size = 3,
#'                               max_mix = c(1),
#'                               output_short = FALSE)
#'
#' dim(cand_mod1$candidate_all)
#' dim(cand_mod1$candidate_by_grouping)
#'
#' cand_mod2 <- candidate_models(variable_names = paste0("x", 1:100),
#'                               groups = c(rep(1,30),rep(2,30), rep(3,40)),
#'                               model_size = 2,
#'                               max_mix = c(1),
#'                               output_short = FALSE)
#'
#'
#' dim(cand_mod2$candidate_all)
#' dim(cand_mod2$candidate_by_grouping)
#'
#'
#'
#'
#' @export
candidate_models <- function(variable_names = paste0("x", 1:10),
                             model_size = 3,
                             groups = NA,
                             max_mix =1,
                             dv_name = "y",
                             output_short = FALSE){

  if(all(!is.na(groups))){
    stopifnot(length(variable_names)==length(groups))
  }


  out1 <- combn(x = variable_names, m = model_size)


  if(all(!is.na(groups))){

    comd_groups <- combn(x = groups, m = model_size)

    comd_groups_tabulate <- combn(x = groups, m = model_size, tabulate, nbins = model_size)

    max_mix <- seq(0, max(max_mix, na.rm=T) )

    comd_groups_tabulate_indx <- apply(comd_groups_tabulate,
                                       MARGIN = 2,
                                       function(i){all(i%in%max_mix)})


    selected_by_grouping <- which(comd_groups_tabulate_indx)

  }else{

    selected_by_grouping <- seq(1,ncol(out1))

  }



  out2 <- out1[,selected_by_grouping]


  out3 <- apply(out2,2,function(i){

    paste(dv_name,  paste(i, collapse = " + "), sep = " ~ " )

  })

  if(!output_short){
    out <- list(candidate_all = out1,
                candidate_by_grouping = out2,
                candidate_models = out3,
                candidates_cols_ids =  selected_by_grouping
    )
  }else{

    out <- out3

  }




  return(out)

}

