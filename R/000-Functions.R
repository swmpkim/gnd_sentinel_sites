#' Attach site names to a data frame containing SET identifiers
#'
#' This function generates a data frame of the 3 individual SETs belonging to each of 5 sites in Grand Bay's Sentinel Site Program. It reads in a data frame containing only SET IDs and adds a column containing the corresponding site.
#'
#' @param dat data frame you want to work with
#' @param set_clm put this in quotes; the name of the column containing your SET IDs. Defaults to "SET" if nothing entered.
#'
#' @return the function returns a data frame.
#' @export
#'

attach_site_names <- function(dat, set_clm = "SET") {

    # set_clm defaults to being named "SET" - needs to be specified, in quotes, if it's something different

    siteID <- data.frame("CLMAJ" = c("CLMAJ-1", "CLMAJ-2", "CLMAJ-3",
                                     "CLMAJ_1", "CLMAJ_2", "CLMAJ_3"),
                         "JURO_High" = c("PANNE-1", "PANNE-2", "PANNE-3",
                                         "JURO_High_1", "JURO_High_2", 
                                         "JURO_High_3"),
                         "JURO_Low" = c("JURO-1", "JURO-2", "JURO-3",
                                        "JURO_Low_1", "JURO_Low_2", "JURO_Low_3"),
                         "JURO_Mid" = c("JURO-4", "JURO-5", "JURO-6", 
                                        "JURO_Mid_1", "JURO_Mid_2", "JURO_Mid_3"),
                         "SPAL" = c("SPAL-1", "SPAL-2", "SPAL-3",
                                    "SPAL_1", "SPAL_2", "SPAL_3")) %>%
        tidyr::gather(key = "site", value = "SET") %>%
        dplyr::mutate(site = as.factor(site),
               SET = as.factor(SET))

    # make sure the original SET names column is named "SET" in the output; this also makes left_join possible
    dat <- dplyr::rename(dat, SET = !! set_clm)

    # join that with the original data frame
    dat_out <- dplyr::left_join(dat, siteID, by = "SET")

    return(dat_out)
}

