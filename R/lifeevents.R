#' Filtering Life Events by Age and Influence
#'
#' \code{filter_le} filters life events by a specified age and influence at
#' time of interview. By default life events occuring at the same age year as
#' the specified age filter are kept. Life events passing the filteres are
#' indicated by 1 and are otherwise 0.
#'
#' @param data A data frame containing the life events data
#' @param age.filter A logical. If age.filter = T, life events will be filtered
#' according to the age and include.last.year settings. Missing age information
#' will set life event to missing.
#' @param age A numeric value. The maximum allowable age. Since the age at which
#' a life event happened is only available at whole years, this by default can
#' also include events, which happened after the allowable age, if
#' include.last.year = T. A missing life event age will return a missing life
#' event.
#' @param influence.filter A logical. If influence.filter=T, life events will
#' be filtered according to the min.influence setting. Missing influence
#' information will set life to missing.
#' @param  min.influence A numeric value. The minimum amount of influence
#' at the time of interview. A missing value will set the life event to missing
#' .
#' 1 'no influence / geen'
#' 2 'a little influence / een beetje'
#' 3 'moderate influence / tamelijk veel'
#' 4 'a lot of influence / heel veel'
#' @param reliable A logical. Indicates whether life events should be omitted,
#' when they were judged to be unreliable, e.g. when language difficulties
#' were noted during the interview. reliable = T omits unreliable life events,
#' reliable = F keeps them. Missing reliability information is assumed to
#' be reliable.
#' @return A data.frame containing information whether life events occured.
#'
#' @export

filter_le <- function(data, age.filter, age, include.last.year = T,
                      influence.filter, min.influence =  1, missing.influence = T,
                      reliable) {
  le.names <- paste("le", 1:26, sep="")
  le_age.names <- paste("le", 1:26, "age", sep="")
  le_influence.names <- paste("le", 1:26, "influence", sep="")
  tofilter.data <- data[ , c(le.names, le_age.names,
                             le_influence.names, "unreliable", age)]

  # Set life events data to missing when unreliable
  if (reliable == T) {
    # For all life events
    reliable.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste("le", x, sep = "")
      # Set to missing, when unreliable
      filtered <- ifelse(tofilter.data[ ,"unreliable"] == 0 |
                           is.na(tofilter.data[ ,"unreliable"]),
                         tofilter.data[ ,le], NA)
    })
    colnames(reliable.data) <- le.names
    tofilter.data <- cbind(reliable.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", age)])
  }

  # Filter for age, set to missing, when age missing and include last year
  if (age.filter & include.last.year) {
    # For every life event
    age.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste("le", x, sep = "")
      le_age <- paste("le", x, "age", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or above age, set to 0
        ifelse(tofilter.data[ ,le] == 0 |
                 tofilter.data[ ,le] == 1 &
                 (tofilter.data[,le_age] > tofilter.data[,age]),
               0,
               # If life event occured and below age, then 1, else NA
               ifelse(tofilter.data[ ,le] == 1 &
                        (tofilter.data[,le_age] <= tofilter.data[,age]),
                      1,
                      NA))
    })
    colnames(age.data) <- le.names
    tofilter.data <- cbind(age.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", age)])
    # Filter for age, but do not include last year
  } else if (age.filter & include.last.year == F) {
    # For every life event
    age.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste("le", x, sep = "")
      le_age <- paste("le", x, "age", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or above age, set to 0
        ifelse(tofilter.data[ ,le] == 0 |
                 tofilter.data[ ,le] == 1 &
                 (tofilter.data[,le_age] >= tofilter.data[,age]),
               0,
               # If life event occured and below age, then 1, else NA
               ifelse(tofilter.data[ ,le] == 1 &
                        (tofilter.data[,le_age] < tofilter.data[,age]),
                      1,
                      NA))
    })
    colnames(age.data) <- le.names
    tofilter.data <- cbind(age.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", age)])
  }

  # Filter for influence, set to missing, when influence is missing
  if (influence.filter) {
    # For every life event
    influence.data <- sapply(1:26, function(x) {
      # Set current variable name
      le <- paste("le", x, sep = "")
      le_influence <- paste("le", x, "influence", sep = "")
      # Filter life events
      filtered <-
        # If life events did not occur or below influence level, set to 0
        ifelse(tofilter.data[ ,le] == 0 |
                 tofilter.data[ ,le] == 1 &
                 (tofilter.data[ ,le_influence] < min.influence),
               0,
               # If life event occured and above influence, then 1, else NA
               ifelse(tofilter.data[ ,le] == 1 &
                        (tofilter.data[ ,le_influence] >= min.influence),
                      1,
                      NA))
    })
    colnames(influence.data) <- le.names
    tofilter.data <- cbind(influence.data,
                           tofilter.data[c(le_age.names, le_influence.names,
                                           "unreliable", age)])
  }
  return(filtered.data <- tofilter.data)
}

#' Compute number of life events
#'
#' \code{sum_le} computes the number of life events experienced. Options
#' include to compute the number omitting open life events, as well as
#' how to handle missing values.
#'
#' @param data A data.frame, which includes information, whether life evetns
#' occured or not.
#' @param include.open A logical. include.open = T will include open life
#' events in calculations.
#' @param na.rm A logical. na.rm=T will treat missing life events as 0.
#' na.rm = F will set number of life events to missing in presence of missing
#' life events.
#'
#' @export
sum_le <- function(data, include.open, na.rm) {
  # If open life events are supposed to be included
  if(include.open) {
    le <- paste("le", 1:26, sep="")
    # data frame with life events occured yes/no
    filtered.data <- data[, le]
    # Sum the life events, if na.rm = F, missing life events will set
    # sum to missing
    rowSums(filtered.data, na.rm=na.rm)
    # If only closed life events are supposed to be included
  } else {
    le <- paste("le", 1:24, sep="")
    # data frame with life events occured yes/no
    filtered.data <- data[, le]
    # Sum the life events, if na.rm = F, missing life events will set
    # sum to missing
    rowSums(filtered.data, na.rm=na.rm)
  }
}
