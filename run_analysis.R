# Getting and Cleaning Data Peer Assessment

# author: Piotr Cwiklinski

# I will start with adding a function to instal, when necessary the necessary packages
#' Simplified loading and installing of packages
#'
#' This is a wrapper to \code{\link{require}} and \code{\link{install.packages}}.
#' Specifically, this will first try to load the package(s) and if not found
#' it will install then load the packages. Additionally, if the 
#' \code{update=TRUE} parameter is specified it will check the currently 
#' installed package version with what is available on CRAN (or mirror) and 
#' install the newer version.
#' 
#' @param pkgs a character vector with the names of the packages to load.
#' @param install if TRUE (default), any packages not already installed will be.
#' @param update if TRUE, this function will install a newer version of the
#'        package if available.
#' @param quiet if TRUE (default), package startup messages will be suppressed.
#' @param verbose if TRUE (default), diagnostic messages will be printed.
#' @param ... other parameters passed to \code{\link{require}}, 
#'            \code{\link{install.packages}}, and 
#'            \code{\link{available.packages}}.
#' @return a data frame with four columns and rownames corresponding to the
#'         packages to be loaded. The four columns are: loaded (logical 
#'         indicating whether the package was successfully loaded), installed 
#'         (logical indicating that the package was installed or updated), 
#'         loaded.version (the version string of the installed package), and 
#'         available.version (the version string of the package currently 
#'         available on CRAN). Note that this only reflects packages listed in 
#'         the \code{pkgs} parameter. Other packages may be loaded and/or 
#'         installed as necessary by \code{install.packages} and \code{require}.
#'         If \code{verbose=FALSE} the data frame will be returned using 
#'         \code{\link{invisible}}.
#' @export
#' @example
#' \dontrun{
#' package(c('devtools','lattice','ggplot2','psych'))
#' }
package <- function(pkgs, install=TRUE, update=FALSE, quiet=TRUE, verbose=TRUE, ...) {
  myrequire <- function(package, ...) {
    result <- FALSE
    if(quiet) { 
      suppressMessages(suppressWarnings(result <- require(package, ...)))
    } else {
      result <- suppressWarnings(require(package, ...))
    }
    return(result)
  }
  mymessage <- function(msg) {
    if(verbose) {
      message(msg)
    }
  }
  
  installedpkgs <- installed.packages()
  availpkgs <- available.packages()[,c('Package','Version')]
  if(nrow(availpkgs) == 0) {
    warning(paste0('There appear to be no packages available from the ',
                   'repositories. Perhaps you are not connected to the ',
                   'Internet?'))
  }
  # It appears that hyphens (-) will be replaced with dots (.) in version
  # numbers by the packageVersion function
  availpkgs[,'Version'] <- gsub('-', '.', availpkgs[,'Version'])
  results <- data.frame(loaded=rep(FALSE, length(pkgs)),
                        installed=rep(FALSE, length(pkgs)),
                        loaded.version=rep(as.character(NA), length(pkgs)),
                        available.version=rep(as.character(NA), length(pkgs)),
                        stringsAsFactors=FALSE)
  row.names(results) <- pkgs
  for(i in pkgs) {
    loadedPkgs <- search()
    needInstall <- FALSE
    if(i %in% row.names(installedpkgs)) {
      v <- as.character(packageVersion(i))
      if(i %in% row.names(availpkgs)) {
        if(v != availpkgs[i,'Version']) {
          if(!update) {
            mymessage(paste0('A newer version of ', i, 
                             ' is available ', '(current=', v, 
                             '; available=',
                             availpkgs[i,'Version'], ')'))
          }
          needInstall <- update
        }
        results[i,]$available.version <- availpkgs[i,'Version']
      } else {
        mymessage(paste0(i, ' is not available on the repositories.'))
      }
    } else {
      if(i %in% row.names(availpkgs)) {
        needInstall <- TRUE & install
        results[i,]$available.version <- availpkgs[i,'Version']
      } else {
        warning(paste0(i, ' is not available on the repositories and ',
                       'is not installed locally'))
      }
    }
    if(needInstall | !myrequire(i, character.only=TRUE)) {
      install.packages(pkgs=i, quiet=quiet)
      if(!myrequire(i, character.only=TRUE, ...)) {
        warning(paste0('Error loading package: ', i))
      } else {
        results[i,]$installed <- TRUE
        results[i,]$loaded <- TRUE
        results[i,]$loaded.version <- as.character(packageVersion(i))
      }
    } else {
      results[i,]$loaded <- TRUE
      results[i,]$loaded.version <- as.character(packageVersion(i))
    }
    loadedPkgs2 <- search()
    for(j in loadedPkgs2[!loadedPkgs2 %in% loadedPkgs]) {
      try(detach(j, character.only=TRUE), silent=TRUE)
    }
  }
  if(verbose) {
    return(results)
  } else {
    invisible(results)
  }
}

#packages
package("data.table")
package("reshape2")
library(data.table)
library(reshape2)

# cleaning variables in the workspace to avoid wrong input and output
rm(list=ls(all=TRUE))

# set the working directory for the project
setwd('C:/Users/p.cwiklinski/Documents/')

# Tasks:
# You should create one R script called run_analysis.R that does the following:

# Part 1

# Merges the training and the test sets to create one data set.

# First, I will load the data from being activity labels 
activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt')[,2]
head(activity_labels)

# and load and acquire headers for the data set - names of data columns
feature_names <- read.table("UCI HAR Dataset/features.txt")[,2]
head(feature_names)

# read in test data
X_test <- read.table('UCI HAR Dataset/test/X_test.txt')
head(X_test)

Y_test <- read.table('UCI HAR Dataset/test/Y_test.txt')
head(Y_test)

subject_test <- read.table('UCI HAR Dataset/test/subject_test.txt')
head(subject_test)

# load train data
X_train <- read.table('UCI HAR Dataset/train/X_train.txt')
head(X_train)

Y_train <- read.table('UCI HAR Dataset/train/Y_train.txt')
head(Y_train)

subject_train <- read.table('UCI HAR Dataset/train/subject_train.txt')
head(subject_train)

# Part 3
# set names of headers on test and train data
names(X_train) <- feature_names
names(X_test) <- feature_names
Y_test[,2] <- activity_labels[Y_test[,1]]
Y_train[,2] = activity_labels[Y_train[,1]]

Y_names <- c("Activity_ID", "Activity_Label")
id_subject <- "subject"
names(Y_test) <- Y_names
names(Y_train) = Y_names
names(subject_test) <- id_subject
names(subject_train) <- id_subject

# Part 2
# extracts only the measurements on the mean and standard deviation for each measurement.
mean_and_std_columns <- grepl('mean|std', feature_names)
X_test <- X_test[,mean_and_std_columns]
X_train <- X_train[,mean_and_std_columns]

# end of Part 1
# binding data and building one data set from train and test data
data_train <- cbind(as.data.table(subject_train), Y_train, X_train)
data_test <- cbind(as.data.table(subject_test), Y_test, X_test)
data_set <- rbind(data_test, data_train)

# Part 4
# uses descriptive activity names to name the activities in the data set
labels <- c("subject", "Activity_ID", "Activity_Label")
data_labels <- setdiff(colnames(data_set), labels)
data_w_labels <- melt(data_set, id = labels, measure.vars = data_labels)

# Part 5
# Creates a second, independent tidy data set with the average of each variable for each activity 
# and each subject
tidy_data_set <- dcast(data_w_labels, subject + Activity_Label ~ variable, mean)
head(tidy_data_set)

# write the tidy data
write.table(tidy_data_set, file = "UCI HAR Dataset/tidy_data.txt", row.name=FALSE)