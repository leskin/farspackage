
#' Read a data file and store the results in a data frame tbl
#'
#' This is a simple function that reads a data file named "filename" into a dataframe
#' using the read_csv function in the readr package. The function stops if the data file
#' does not exist.
#'
#' @param filename A character string giving the name of the data file to read
#'
#' @return This function returns a data frame tbl (tbl_df) which contains the data read from the file "filename".
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_data_table <- fars_read("data/accident_2013.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a character string for the name of a fars database accident file for a given year
#'
#' This is a simple function that creats a filename for a compressed (.bz2) comma-separated-variable (.csv)
#' data file for accidents recorded in the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (fars) that occured for a given year ("year").  The created filename is of
#' the form: "accident_year.csv.bz2" where "year" is replaced with the desired year.  There are no
#' conditions that will cause an error.
#'
#' @param year A character string giving the desired year for which to create the filename
#'
#' @return This function returns a character string containing the desired filename.
#'
#' @examples
#' \dontrun{file2013 <- make_filename("2013")}
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple years of accident data from the fars database into a list
#'
#' This is a simple function that takes a list of years as character string input
#' and reads the approprite accident data files from the US National Highway Traffic
#' Safety Administration's Fatality Analysis Reporting System (fars) for the specified years.
#' The results are returned in a list.  If data for a requested year is not available,
#' a warning is produced and nothing is returned for the given year.
#'
#' @param years A list of character strings giving the desired years to read from the fars database
#'
#' @return This function returns a list with the desired accident data for the specified list of years.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{accidentData <- fars_read_years(c("2013","2015"))}
#'
#'@export
fars_read_years <- function(years) {
        MONTH <- NULL # set vars to NULL to appease R CMD check
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize multiple years of accident data from the fars database into a data frame tbl
#'
#' This is a simple function that takes a list of years as character string input,
#' reads the appropriate accident data files from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (fars) for the requested years into a data.frame and then
#' summarizes the accident data by month and by year.  The function has no error conditions,
#' but an invalid year request will cause a warning to be issued by the fars_read_years function.
#'
#' @param years A list of character strings giving the desired years to read from the fars database
#'
#' @return This function returns a data frame tbl (tbl_df) which summarizes the number of accidents for each month and for each year requested.
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{accidentSummary <- fars_summarize_years(c("2014","2015"))}
#'
#' @export
fars_summarize_years <- function(years) {
        MONTH <- n <- year <- NULL # set vars to NULL to appease R CMD check
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot fatal accident data for a given state and year on a map
#'
#' This is a simple function that plots all fatal accidents on a state map for
#' accidents recorded in the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (fars) that occured within that state during a given year.
#' An error will occur and the function will stop if the specified state number is invalid, and
#' a message will be shown if there are no fatal accidents within the specified state
#' for the specified year.
#'
#' @param state.num A character string giving the state number
#' @param year A character string giving the year for the accident data to plot
#'
#' @return This function creates a plot but does not return an object.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state("1","2013")}
#'
#' @export
fars_map_state <- function(state.num, year) {
        MONTH <- STATE <- n <- NULL # set vars to NULL to appease R CMD check
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

