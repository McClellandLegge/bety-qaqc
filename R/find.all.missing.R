##' Find All Missing Covariates
##'
##' Query the observations with the desired trait and their covariates recorded.
##' If there is not at least one of required covariates the trait_id will be
##' outputted with all of variables that are not present.
##'
##' @title find all missing covariates
##' @param data_list bety
##' @param most_recent a logical, the function will perform a find on all traits updated at
##' the most recent date
##' @param use_date a logical indicating whether or not a find should be performed using the
##' from_date specified
##' @param from_date an instant in the form 'yyyy-mm-dd' indicating
##' the earliest possile updated at date for records in the find
##' @param user_trait a string (from variables.name field, see \link{betydb.org/variables})
##' 'all' specifies that every trait should be checked
##' @param tbl_ex a logical indicating whether or not a table should be outputted in a .tex file
##' @param outfile a character string specifying the name of the .tex file to be outputted to
##' the working directory
##' @param is_test a logical indicating whether or not test data sets should be used
##' @param test_traits a data table or data frame, testing traits data
##' @param test_cov a data table or data frame, testing covariates data
##' @return Table of trait_id, variable_id and name for all traits that are missing
##'         required covariates
##' @export
##' @author McClelland Legge
find.all.missing <- function(data_list,
                             user_trait  = "All",
                             most_recent = FALSE,
                             use_date    = FALSE,
                             from_date   = NA,
                             tbl_ex      = FALSE,
                             outfile,
                             is_test     = FALSE,
                             test_traits = NA,
                             test_cov    = NA) {

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("`lubridate` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  traits     <- data_list$traits
  variables  <- data_list$variables
  covariates <- data_list$covariates

  if (missing(user_trait)) {
    stop("need to specify trait")
  }

  if (tbl_ex == TRUE & (missing(outfile) || !is.character(outfile))) {
    stop("need to specify outfile")
  }

  if (is_test == TRUE & (is.na(test_traits) | is.na(test_cov))) {
    stop("need to specify testing sets")
  }

  allowed_traits <- c(variables[agrep("resp", variables$name), ]$name, "Vcmax",
                      "root_respiration_rate", "stomatal_slope",
                      "stomatal conductance", "All")
  if (!user_trait %in% allowed_traits) {
    stop("Trait not recognized, consult variables table for correct spelling")
  }

  if (use_date == TRUE & !lubridate::is.instant(ymd(from_date)))
    stop("use any instant as defined by the package lubridate for from_date")

  if (most_recent == TRUE & use_date == TRUE)
    stop("only one of most_recent or use_date may be set to TRUE")

  if (user_trait == "All") {

    utrait = names(required.covariates)

    collect = vector("list")

    if (is_test == TRUE) {
      t <- as.data.frame(merge(test_traits, variables,
                               by = "variable_id"))
      cand = names(required.covariates)[names(required.covariates) %in% unique(t$name)]
    } else {
      t <- as.data.frame(merge(traits, variables,
                               by = "variable_id"))
      cand = names(required.covariates)[names(required.covariates) %in% unique(t$name)]
    }

    for (k in 1:length(cand)) {

      if (is_test == T) {

        traits <- test_traits

        covariates <- test_cov
      }

      if (most_recent == T) {

        traits = as.data.frame(subset(traits, traits$updated_at !=
                                        "0000-00-00 00:00:00"))
        traits$durations = duration(new_interval(ymd(paste0(traits$updated_at)),
                                                 today()))
        traits <- subset(traits, durations <= min(traits$durations))
      }

      if ((most_recent == F && use_date == T)) {

        traits = as.data.frame(subset(traits, paste0(traits$updated_at) !=
                                        "0000-00-00 00:00:00"))
        traits$durations = duration(new_interval(floor_date(ymd(from_date),
                                                            "day"), floor_date(ymd(paste0(traits$updated_at)),
                                                                               "day")))
        traits <- subset(traits, durations >= 0)
      }

      # as data tables:
      trait_tab.a <- as.data.frame(merge(traits, variables,
                                         by = "variable_id"))

      trait_tab <- subset(trait_tab.a, name == paste(cand[k]))

      if (nrow(trait_tab) == 0)
        break

      cov_tab <- merge(covariates, variables, by = "variable_id")

      # call the appropriate reference table
      ref <- required.covariates[[paste(cand[k])]]


      # find perpetrators
      missing = vector("list")

      total = length(unique(trait_tab$trait_id))

      for (i in 1:length(unique(trait_tab$trait_id))) {

        ind = 0

        set = subset(cov_tab, trait_id == sort(unique(trait_tab$trait_id))[i])

        comb <- c(ref$rcov1$variable_id, ref$rcov2$variable_id)

        if (nrow(set) == 0) {

          missing[[sort(unique(trait_tab$trait_id))[i]]] = data.frame(trait_id = sort(unique(trait_tab$trait_id))[i],
                                                                      Missing_Covariate_id = 0, Name = "All")
        } else {

          v1 = ref$rcov1$variable_id[!(ref$rcov1$variable_id %in%
                                         set$variable_id)]

          if (length(v1) > length(ref$rcov1$variable_id) -
                1) {
            m.rcov1 = data.frame(trait_id = sort(unique(trait_tab$trait_id))[i],
                                 Missing_Covariate_id = v1, Name = subset(variables,
                                                                          variable_id %in% v1)$name)
            ind = 1
          } else {
            m.rcov1 = NULL
          }

          if (!(is.null(ref$rcov2$variable_id))) {

            v2 = ref$rcov2$variable_id[!(ref$rcov2$variable_id %in%
                                           set$variable_id)]

            if (length(v2) > length(ref$rcov2$variable_id) -
                  1) {
              m.rcov2 = data.frame(trait_id = sort(unique(trait_tab$trait_id))[i],
                                   Missing_Covariate_id = v2, Name = subset(variables,
                                                                            variable_id %in% v2)$name)
              ind = 2
            } else {
              m.rcov2 = NULL
            }
          }

          if (ind == 1) {
            missing[[sort(unique(trait_tab$trait_id))[i]]] = m.rcov1
          }

          if (ind == 2) {
            missing[[sort(unique(trait_tab$trait_id))[i]]] = rbind(m.rcov1,
                                                                   m.rcov2)
          }

        }

      }

      collect[[cand[k]]] <- do.call(rbind, missing)

      if (tbl_ex == T) {
        if (is.null(collect[[cand[k]]]) != T) {
          out = paste0(outfile, ".", cand[k], ".tex")
          if (use_date == T) {
            dte <- paste0(from_date)
            tbl <- xtable(collect[[cand[k]]], caption = paste0(cand[k],
                                                                   " Traits with Missing Covariates Updated on or After ",
                                                                   month(dte, label = TRUE, abbr = F), " ",
                                                                   day(dte), "th ", year(dte)))
          }
          if (most_recent == T) {
            dte <- today() - duration(min(traits$durations),
                                      "seconds")
            tbl <- xtable(collect[[cand[k]]], caption = paste0(cand[k],
                                                                   " Traits with Missing Covariates Updated on ",
                                                                   month(dte, label = TRUE, abbr = F), " ",
                                                                   day(dte), "th ", year(dte)))
          }
          if (most_recent == F && use_date == F) {
            tbl <- xtable(collect[[cand[k]]], caption = paste0(cand[k],
                                                                   " Traits with Missing Covariates"))
          }
          print(tbl, file = out)

        }
      }


    }

    master = do.call(rbind, collect)

    rownames(master) = NULL

    if (tbl_ex == T) {
      out = paste0(outfile, ".master", ".tex")
      if (use_date == T) {
        dte <- paste0(from_date)
        tbl <- xtable(master, caption = paste0("All Traits with Missing Covariates Updated on or After ",
                                               month(dte, label = TRUE, abbr = F), " ", day(dte),
                                               "th ", year(dte)))
      }
      if (most_recent == T) {
        dte <- today() - duration(min(traits$durations),
                                  "seconds")
        tbl <- xtable(master, caption = paste0("All Traits with Missing Covariates Updated on ",
                                               month(dte, label = TRUE, abbr = F), " ", day(dte),
                                               "th ", year(dte)))
      }
      if (most_recent == F && use_date == F) {
        tbl <- xtable(master, caption = paste0("All Traits with Missing Covariates"))
      }
      print(tbl, file = out)
    }


  } else {

    if (is_test == T) {
      traits <- test_traits
      covariate <- test_cov
    }

    trait_tab.a <- as.data.frame(merge(traits, variables,
                                       by = "variable_id"))

    trait_tab <- subset(trait_tab.a, name == paste(user_trait))

    cov_tab <- merge(covariates, variables, by = "variable_id")

    # call the appropriate reference table
    ref <- required.covariates[[paste(user_trait)]]

    if (most_recent == T) {
      traits = as.data.frame(subset(traits, traits$updated_at !=
                                      "0000-00-00 00:00:00"))
      traits$durations = duration(new_interval(ymd(paste0(traits$updated_at)),
                                               today()))
      traits <- subset(traits, durations <= min(traits$durations))
    }

    if ((most_recent == F && use_date == T)) {
      traits = as.data.frame(subset(traits, paste0(traits$updated_at) !=
                                      "0000-00-00 00:00:00"))
      traits$durations = duration(new_interval(floor_date(from_date,
                                                          "day"), floor_date(ymd(paste0(traits$updated_at)),
                                                                             "day")))
      traits <- subset(traits, durations >= 0)
    }

    # find perpetrators
    missing = vector("list")

    total = length(unique(trait_tab$trait_id))

    for (i in 1:length(unique(trait_tab$trait_id))) {
      ind = 0
      set = subset(cov_tab, trait_id == sort(unique(trait_tab$trait_id))[i])
      comb <- c(ref$rcov1$variable_id, ref$rcov2$variable_id)

      if (nrow(set) == 0) {
        missing[[sort(unique(trait_tab$trait_id))[i]]] = data.frame(trait_id = sort(unique(trait_tab$trait_id))[i],
                                                                    Missing_Covariate_id = 0, Name = "All")
      } else {

        v1 = ref$rcov1$variable_id[!(ref$rcov1$variable_id %in%
                                       set$variable_id)]
        if (length(v1) > length(ref$rcov1$variable_id) -
              1) {
          m.rcov1 = data.frame(trait_id = sort(unique(trait_tab$trait_id))[i],
                               Missing_Covariate_id = v1, Name = subset(variables,
                                                                        variable_id %in% v1)$name)
          ind = 1
        } else {
          m.rcov1 = NULL
        }

        if (!(is.null(ref$rcov2$variable_id))) {
          v2 = ref$rcov2$variable_id[!(ref$rcov2$variable_id %in%
                                         set$variable_id)]

          if (length(v2) > length(ref$rcov2$variable_id) -
                1) {
            m.rcov2 = data.frame(trait_id = sort(unique(trait_tab$trait_id))[i],
                                 Missing_Covariate_id = v2, Name = subset(variables,
                                                                          variable_id %in% v2)$name)
            ind = 2
          } else {
            m.rcov2 = NULL
          }
        }

        if (ind == 1) {
          missing[[sort(unique(trait_tab$trait_id))[i]]] = m.rcov1
        }

        if (ind == 2) {
          missing[[sort(unique(trait_tab$trait_id))[i]]] = rbind(m.rcov1,
                                                                 m.rcov2)
        }

      }
    }


    collect <- do.call(rbind, missing)

    if (tbl_ex == T) {
      out = paste0(outfile, ".", user_trait, ".tex")

      if (use_date == T) {
        dte <- paste0(from_date)
        tbl <- xtable(collect, caption = paste0(user_trait,
                                                " Traits with Missing Covariates Updated on or After ",
                                                month(dte, label = TRUE, abbr = F), " ", day(dte),
                                                "th ", year(dte)))
      }
      if (most_recent == T) {
        dte <- today() - duration(min(traits$durations),
                                  "seconds")
        tbl <- xtable(collect, caption = paste0(user_trait,
                                                " Traits with Missing Covariates Updated on ",
                                                month(dte, label = TRUE, abbr = F), " ", day(dte),
                                                "th ", year(dte)))
      }
      if (most_recent == F && use_date == F) {
        tbl <- xtable(collect, caption = paste0(user_trait,
                                                " Traits with Missing Covariates"))
      }
      print(tbl, file = out)
    }
  }

  if (user_trait == "All") {
    if (length(collect) != 0) {
      return(list(traits = collect, master = master))
    } else {
      return("No traits with missing covariates under current search parameters")
    }
  } else {
    if (nrow(collect) != 0) {
      return(collect)
    } else {
      return("No traits with missing covariates under current search parameters")
    }
  }

}
