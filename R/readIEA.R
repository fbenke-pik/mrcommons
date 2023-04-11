#' Read IEA
#'
#' Read-in an IEA csv file as magpie object
#'
#' @param subtype data subtype. Either "EnergyBalances" or "Emissions"
#' @return magpie object of the IEA
#' @author Anastasis Giannousakis, Lavinia Baumstark, Renato Rodrigues
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "IEA", subtype = "EnergyBalances")
#' }
#'
#' @importFrom data.table fread :=
#' @importFrom dplyr %>%
#' @importFrom madrat toolCountry2isocode
#'
readIEA <- function(subtype) {
  if (subtype == "EnergyBalances") { # IEA energy balances until 2020 (incomplete 2021) (data updated in August, 2022)

    energyBalancesFile <- "IEA-Energy-Balances-2022/worldbig.csv"
    data <- fread(
      file = energyBalancesFile,
      col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "ktoe"),
      colClasses = c("character", "character", "character", "numeric", "character"),
      sep = ";", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 2, showProgress = FALSE
    )

    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE)

    data <- data %>%
      filter(!is.na(!!sym("ktoe")), !is.na(!!sym("COUNTRY"))) %>%
      mutate(!!sym("ktoe") := as.numeric(!!sym("ktoe")))

    mdata <- as.magpie(data,
      datacol = dim(data)[2], spatial = which(colnames(data) == "COUNTRY"),
      temporal = which(colnames(data) == "TIME")
    )

  } else if (subtype == "Emissions") {
    data.ghg <- fread( # nolint object_name_linter
      file = "IEA-GHG-Emissions-2022/WORLD_GHG.TXT",
      col.names = c("COUNTRY", "PRODUCT", "TIME", "FLOW", "GAS", "value"),
      colClasses = c("character", "character", "numeric", "character", "character", "character"),
      stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 0, showProgress = FALSE
    ) %>%
      mutate(!!sym("value") := as.numeric(!!sym("value"))) %>%
      filter(!is.na(!!sym("value"))) %>%
      select("COUNTRY", "TIME", "PRODUCT", "FLOW", "GAS", "value")

    data.co2 <- fread( # nolint object_name_linter
      file = "IEA-GHG-Emissions-2022/WORLD_BIGCO2.TXT",
      col.names = c("COUNTRY", "PRODUCT", "TIME", "FLOW", "value"),
      colClasses = c("character", "character", "numeric", "character", "character"),
      stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 0, showProgress = FALSE
    ) %>%
      mutate(!!sym("value") := as.numeric(!!sym("value")), GAS = "CO2") %>%
      filter(!is.na(!!sym("value"))) %>%
      select("COUNTRY", "TIME", "PRODUCT", "FLOW", "GAS", "value")

    mdata <- as.magpie(rbind(data.ghg, data.co2), spatial = 1) %>%
      suppressWarnings()

    getItems(mdata, dim = 1) <- toolCountry2isocode(getItems(mdata, dim = 1), warn = FALSE)
    mdata <- mdata[!is.na(getItems(mdata, dim = 1)), , ]

  } else {
    stop("Not a valid subtype!")
  }
  return(mdata)
}
