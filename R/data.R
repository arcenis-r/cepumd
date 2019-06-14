#' Data for calculating weighted, estimated mean pet expenditures.
#'
#' Data containing weights, adjusted weights, ucc's, expenditure value columns
#' prepared for calculating weighted, estimated mean pet expenditures.
#'
#' @format A data frame with 42943 rows and 52 variables:
#' \describe{
#'   \item{newid}{A consumer unit (CU), or household, identifier}
#'   \item{finlwt21}{CU weight variable}
#'   \item{wtrep01}{CU replicate weight 1}
#'   \item{wtrep02}{CU replicate weight 2}
#'   \item{wtrep03}{CU replicate weight 3}
#'   \item{wtrep04}{CU replicate weight 4}
#'   \item{wtrep05}{CU replicate weight 5}
#'   \item{wtrep06}{CU replicate weight 6}
#'   \item{wtrep07}{CU replicate weight 7}
#'   \item{wtrep08}{CU replicate weight 8}
#'   \item{wtrep09}{CU replicate weight 9}
#'   \item{wtrep10}{CU replicate weight 10}
#'   \item{wtrep11}{CU replicate weight 11}
#'   \item{wtrep12}{CU replicate weight 12}
#'   \item{wtrep13}{CU replicate weight 13}
#'   \item{wtrep14}{CU replicate weight 14}
#'   \item{wtrep15}{CU replicate weight 15}
#'   \item{wtrep16}{CU replicate weight 16}
#'   \item{wtrep17}{CU replicate weight 17}
#'   \item{wtrep18}{CU replicate weight 18}
#'   \item{wtrep19}{CU replicate weight 19}
#'   \item{wtrep20}{CU replicate weight 20}
#'   \item{wtrep21}{CU replicate weight 21}
#'   \item{wtrep22}{CU replicate weight 22}
#'   \item{wtrep23}{CU replicate weight 23}
#'   \item{wtrep24}{CU replicate weight 24}
#'   \item{wtrep25}{CU replicate weight 25}
#'   \item{wtrep26}{CU replicate weight 26}
#'   \item{wtrep27}{CU replicate weight 27}
#'   \item{wtrep28}{CU replicate weight 28}
#'   \item{wtrep29}{CU replicate weight 29}
#'   \item{wtrep30}{CU replicate weight 30}
#'   \item{wtrep31}{CU replicate weight 31}
#'   \item{wtrep32}{CU replicate weight 32}
#'   \item{wtrep33}{CU replicate weight 33}
#'   \item{wtrep34}{CU replicate weight 34}
#'   \item{wtrep35}{CU replicate weight 35}
#'   \item{wtrep36}{CU replicate weight 36}
#'   \item{wtrep37}{CU replicate weight 37}
#'   \item{wtrep38}{CU replicate weight 38}
#'   \item{wtrep39}{CU replicate weight 39}
#'   \item{wtrep40}{CU replicate weight 40}
#'   \item{wtrep41}{CU replicate weight 41}
#'   \item{wtrep42}{CU replicate weight 42}
#'   \item{wtrep43}{CU replicate weight 43}
#'   \item{wtrep44}{CU replicate weight 44}
#'   \item{sex_ref}{Sex of the CU reference person}
#'   \item{mo_scope}{ Months in scope}
#'   \item{popwt}{An adjusted weight meant to account for the fact that a CU's
#'     value of finlwt21 is meant to be representative of only 1 quarter of
#'     data}
#'   \item{aggwt}{The sum of popwt (used for calculating estimated means)}
#'   \item{ucc}{The UCC for a given expenditure}
#'   \item{cost}{The value of the expenditure (in U.S. Dollars)}
#' }
#' @source \url{http://www.bls.gov/cex/pumd_data.htm}
"ce_pets17"
