#' Filtering Life Events by Age and Influence
#'
#' \code{filter_le} filters life events by a specified age and influence at
#' time of interview. By default life events occuring at the same age year as
#' the specified age filter are kept. Life events passing the filteres are
#' indicated by 1 and are otherwise 0.
#'
#' @param data A data frame containing the life events data
#' @param min.age.filter A logical. If min.age.filter = T, life events will be filtered
#' according to the min.age and include.first.year settings. Missing age information
#' will set life event to missing.
#' @param min.age A string. Name of the column with the minimum allowable age. Since
#' the age at which a life event happened is only available at whole years,
#' this can also include events, which happened before the allowable
#' age, if include.first.year = T.
#' @param include.first.year A logical. If TRUE, life events occuring at the
#' same age as "min.age" are kept. Thus some events might have happened before
#' "max.age". If FALSE, life events occuring at the same age will be set as not
#' having occured. Default is FALSE.
#' @param max.age.filter A logical. If max.age.filter = T, life events will be filtered
#' according to the max.age and include.last.year settings. Missing age information
#' will set life event to missing.
#' @param max.age A string. Name of the column with the maximum allowable age. Since
#' the age at which a life event happened is only available at whole years,
#' this can also include events, which happened after the allowable
#' age, if include.last.year = T.
#' @param include.last.year A logical. If TRUE, life events occuring at the
#' same age as "max.age" are kept. Thus some events might have happened after
#' "max.age". If FALSE, life events occuring at the same age will be set as not
#' having occured. Default is FALSE.
#' @param influence.filter A logical. If influence.filter = T, life events will
#' be filtered according to the min.influence setting. Missing influence
#' information will set life events to missing.
#' @param  min.influence A numeric value. The minimum amount of influence
#' at the time of interview.
#' \enumerate{
#'  \item 'no influence / geen'
#'  \item 'a little influence / een beetje'
#'  \item 'moderate influence / tamelijk veel'
#'  \item 'a lot of influence / heel veel'
#' }
#' @param reliable A logical. Indicates whether life events should be omitted,
#' when they were judged to be unreliable, e.g. when language difficulties
#' were noted during the interview. reliable = T omits unreliable life events,
#' reliable = F keeps them. Missing reliability information is assumed to
#' be reliable.
#' @return A matrix containing information whether life events occured (1)
#' or not (0).
#'
#' @export

filter_le <- function(data, min.age.filter=F, min.age="", include.first.year = F,
                      max.age.filter=F, max.age="", include.last.year = F,
                      influence.filter=F, min.influence=1,
                      reliable=T) {
  # Prepare variable names and data frame
  le.names <- paste0("le", 1:26)
  le_age.names <- paste0("le", 1:26, "age")
  le_influence.names <- paste0("le", 1:26, "influence")
  tofilter.data <- data[ , c(le.names, le_age.names,
                             le_influence.names, "unreliable", min.age, max.age)]

  # Convert data to numeric and age variables to integer
  tofilter.data <- as.data.frame(sapply(tofilter.data, function(x) as.numeric(x)))
  tofilter.data[,min.age] <- as.integer(tofilter.data[,min.age])
  tofilter.data[,max.age] <- as.integer(tofilter.data[,max.age])

  # Set life events data to missing when unreliable
  if (reliable) {
    # For all life events
    reliable.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste0("le", x, sep = "")
      # Set to missing, when unreliable
      filtered <- ifelse(tofilter.data[ ,"unreliable"] == 1 |
                           is.na(tofilter.data[ ,"unreliable"]),
                         tofilter.data[ ,le], NA)
    })
    colnames(reliable.data) <- le.names
    tofilter.data <- cbind(reliable.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", min.age, max.age)])
  }

  # Filter for age, set to missing, when age missing and include last year
  if (min.age.filter & include.first.year) {
    # For every life event
    age.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste0("le", x, sep = "")
      le_age <- paste0("le", x, "age", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or below age, set to 0
        ifelse(tofilter.data[ ,le] == 1 |
                 tofilter.data[ ,le] == 2 &
                 (tofilter.data[,le_age] < tofilter.data[,min.age]),
               1,
               # If life event occured and above min.age, then 1, else NA
               ifelse(tofilter.data[ ,le] == 2 &
                        (tofilter.data[,le_age] >= tofilter.data[,min.age]),
                      2,
                      NA))
    })
    colnames(age.data) <- le.names
    tofilter.data <- cbind(age.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", min.age, max.age)])
    # Filter for min.age, but do not include first year
  } else if (min.age.filter & !include.first.year) {
    # For every life event
    age.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste0("le", x, sep = "")
      le_age <- paste0("le", x, "age", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or below age, set to 0
        ifelse(tofilter.data[ ,le] == 1 |
                 tofilter.data[ ,le] == 2 &
                 (tofilter.data[,le_age] <= tofilter.data[,min.age]),
               1,
               # If life event occured and above age, then 1, else NA
               ifelse(tofilter.data[ ,le] == 2 &
                        (tofilter.data[,le_age] > tofilter.data[,min.age]),
                      2,
                      NA))
    })
    colnames(age.data) <- le.names
    tofilter.data <- cbind(age.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", min.age, max.age)])
  }

  # Filter for age, set to missing, when age missing and include last year
  if (max.age.filter & include.last.year) {
    # For every life event
    age.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste0("le", x, sep = "")
      le_age <- paste0("le", x, "age", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or above age, set to 0
        ifelse(tofilter.data[ ,le] == 1 |
                 tofilter.data[ ,le] == 2 &
                 (tofilter.data[,le_age] > tofilter.data[,max.age]),
               1,
               # If life event occured and below max.age, then 1, else NA
               ifelse(tofilter.data[ ,le] == 2 &
                        (tofilter.data[,le_age] <= tofilter.data[,max.age]),
                      2,
                      NA))
    })
    colnames(age.data) <- le.names
    tofilter.data <- cbind(age.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", min.age, max.age)])
    # Filter for max.age, but do not include last year
  } else if (max.age.filter & !include.last.year) {
    # For every life event
    age.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste0("le", x, sep = "")
      le_age <- paste0("le", x, "age", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or above age, set to 0
        ifelse(tofilter.data[ ,le] == 1 |
                 tofilter.data[ ,le] == 2 &
                 (tofilter.data[,le_age] >= tofilter.data[,max.age]),
               1,
               # If life event occured and below age, then 1, else NA
               ifelse(tofilter.data[ ,le] == 2 &
                        (tofilter.data[,le_age] < tofilter.data[,max.age]),
                      2,
                      NA))
    })
    colnames(age.data) <- le.names
    tofilter.data <- cbind(age.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", min.age, max.age)])
  }

  # Filter for influence, set to missing, when influence is missing
  if (influence.filter) {
    # For every life event
    influence.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste0("le", x, sep = "")
      le_influence <- paste0("le", x, "influence", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or below influence level, set to 0
        ifelse(tofilter.data[ ,le] == 1 |
                 tofilter.data[ ,le] == 2 &
                 (tofilter.data[ ,le_influence] < min.influence),
               1,
               # If life event occured and above influence, then 1, else NA
               ifelse(tofilter.data[ ,le] == 2 &
                        (tofilter.data[ ,le_influence] >= min.influence),
                      2,
                      NA))
    })
    colnames(influence.data) <- le.names
    tofilter.data <- cbind(influence.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", min.age, max.age)])
  }

  # Change values, so that no life event = 0 and life event experienced = 1
  # Return filtered life event data
  return(sapply(tofilter.data[le.names], function(x) x-1))
}

#' Compute number of life events
#'
#' \code{sum_le} computes the number of life events experienced. Options
#' include to compute the number omitting open life events, as well as
#' how to handle missing values.
#'
#' @param data A matrix, which includes information, whether life evetns
#' occured (1) or not (0).
#' @param include.open A logical. include.open = T will include open life
#' events in calculations.
#' @param na.rm A logical. na.rm=T will treat missing life events as 0.
#' na.rm = F will set number of life events to missing in presence of missing
#' life events.
#'
#' @export
sum_le <- function(data, include.open, na.rm) {
  # Include closed life events plus open life events, in case include.open == T
  le <- paste0("le",1:ifelse(include.open,26,24))
  # data frame with life events occured yes/no
  filtered.data <- data[, le]
  # Sum the life events, if na.rm = F, missing life events will set
  # sum to missing
  rowSums(filtered.data, na.rm=na.rm)
}

