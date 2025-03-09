#' Customer dataset
#'
#' A comprehensive dataset containing customer information including personal details,
#' geographic information, demographics, and other relevant customer attributes.
#'
#' @format A data frame with rows of customer data and 24 columns:
#' \describe{
#'   \item{customer_key}{Unique identifier for each customer}
#'   \item{geo_area_key}{Geographic area identifier}
#'   \item{start_dt}{Start date of customer relationship}
#'   \item{end_dt}{End date of customer relationship (if applicable)}
#'   \item{continent}{Continent where the customer is located}
#'   \item{gender}{Customer's gender}
#'   \item{title}{Customer's title (Mr., Mrs., Ms., etc.)}
#'   \item{given_name}{Customer's first name}
#'   \item{middle_initial}{Customer's middle initial}
#'   \item{surname}{Customer's last name}
#'   \item{street_address}{Customer's street address}
#'   \item{city}{City where the customer resides}
#'   \item{state}{State abbreviation}
#'   \item{state_full}{Full state name}
#'   \item{zip_code}{Postal/ZIP code}
#'   \item{country}{Country code}
#'   \item{country_full}{Full country name}
#'   \item{birthday}{Customer's date of birth}
#'   \item{age}{Customer's age in years}
#'   \item{occupation}{Customer's occupation or profession}
#'   \item{company}{Company where the customer is employed}
#'   \item{vehicle}{Customer's vehicle information}
#'   \item{latitude}{Geographic latitude of customer's location}
#'   \item{longitude}{Geographic longitude of customer's location}
#' }
#' @source Internal customer database Generated from `fpaR::sales`
"customer"

#' Sales Transactions Dataset
#'
#' This dataset contains sales transaction data, including order details, customer information, pricing, and revenue metrics.
#'
#' @format A data frame with multiple rows and 17 columns:
#' \describe{
#'   \item{order_key}{\code{integer}. Unique identifier for each order.}
#'   \item{line_number}{\code{integer}. Line number within an order, representing individual items.}
#'   \item{order_date}{\code{Date}. Date when the order was placed.}
#'   \item{delivery_date}{\code{Date}. Date when the order was delivered.}
#'   \item{customer_key}{\code{integer}. Unique identifier for the customer.}
#'   \item{store_key}{\code{integer}. Unique identifier for the store where the transaction occurred.}
#'   \item{product_key}{\code{integer}. Unique identifier for the product.}
#'   \item{quantity}{\code{numeric}. Number of units sold in the transaction.}
#'   \item{unit_price}{\code{numeric}. Price per unit of the product in the original currency.}
#'   \item{net_price}{\code{numeric}. Final price per unit after discounts.}
#'   \item{unit_cost}{\code{numeric}. Cost per unit of the product.}
#'   \item{currency_code}{\code{character}. Currency code (e.g., "USD", "EUR").}
#'   \item{exchange_rate}{\code{numeric}. Exchange rate applied to the transaction currency.}
#'   \item{gross_revenue}{\code{numeric}. Total revenue before any deductions.}
#'   \item{net_revenue}{\code{numeric}. Revenue after deductions such as discounts and taxes.}
#'   \item{cogs}{\code{numeric}. Cost of goods sold (COGS).}
#'   \item{margin}{\code{numeric}. Profit margin calculated as \code{net_revenue - cogs}.}
#' }
#'
#' @source Generated from `fpaR::sales`
#'
#' @examples
#' data(sales)
#' head(sales)
#' summary(sales)
"sales"


#' Date Dimension Table
#'
#' This dataset provides a comprehensive date dimension table, including various calendar attributes such as year, quarter, month, and day-related information.
#'
#' @format A data frame with multiple rows and 18 columns:
#' \describe{
#'   \item{date}{\code{Date}. The actual calendar date.}
#'   \item{date_key}{\code{integer}. Unique identifier for the date (often used in data warehouses).}
#'   \item{year}{\code{integer}. The calendar year (e.g., 2024).}
#'   \item{year_quarter}{\code{character}. Year and quarter combination (e.g., "2024-Q1").}
#'   \item{year_quarter_number}{\code{integer}. Numeric representation of year and quarter (e.g., 202401 for Q1 of 2024).}
#'   \item{quarter}{\code{integer}. The quarter of the year (1 to 4).}
#'   \item{year_month}{\code{character}. Year and month combination (e.g., "2024-01").}
#'   \item{year_month_short}{\code{character}. Abbreviated year and month (e.g., "Jan 2024").}
#'   \item{year_month_number}{\code{integer}. Numeric representation of year and month (e.g., 202401 for January 2024).}
#'   \item{month}{\code{character}. Full month name (e.g., "January").}
#'   \item{month_short}{\code{character}. Abbreviated month name (e.g., "Jan").}
#'   \item{month_number}{\code{integer}. Numeric representation of the month (1 to 12).}
#'   \item{dayof_week}{\code{character}. Full name of the day of the week (e.g., "Monday").}
#'   \item{dayof_week_short}{\code{character}. Abbreviated day of the week (e.g., "Mon").}
#'   \item{dayof_week_number}{\code{integer}. Numeric representation of the day of the week (1 for Monday to 7 for Sunday).}
#'   \item{working_day}{\code{logical}. Indicates if the date is a working day (TRUE/FALSE).}
#'   \item{working_day_number}{\code{integer}. Sequential working day number within the year.}
#' }
#'
#' @source Generated from `fpaR::date`
#'
#' @examples
#' \dontrun{
#' data(date)
#' head(date)
#' summary(date)
#' }
"date"

#' Foreign Exchange Rates Dataset
#'
#' This dataset contains foreign exchange rates between different currencies for specific dates.
#'
#' @format A data frame with multiple rows and 4 columns:
#' \describe{
#'   \item{date}{\code{Date}. The date for which the exchange rate is recorded.}
#'   \item{from_currency}{\code{character}. The source currency code (e.g., "USD").}
#'   \item{to_currency}{\code{character}. The target currency code (e.g., "EUR").}
#'   \item{exchange}{\code{numeric}. The exchange rate from \code{from_currency} to \code{to_currency}.}
#' }
#'
#' @source Generated from `fpaR::fx`
#'
#' @examples
#' \dontrun{
#' data(fx)
#' head(fx)
#' summary(fx)
#' }
"fx"


#' Product Master Dataset
#'
#' This dataset contains detailed information about products, including their identifiers, attributes, pricing, and categorization.
#'
#' @format A data frame with multiple rows and 14 columns:
#' \describe{
#'   \item{product_key}{\code{integer}. Unique identifier for the product.}
#'   \item{product_code}{\code{character}. Internal or SKU code for the product.}
#'   \item{product_name}{\code{character}. Name of the product.}
#'   \item{manufacturer}{\code{character}. Name of the product manufacturer.}
#'   \item{brand}{\code{character}. Brand associated with the product.}
#'   \item{color}{\code{character}. Color of the product.}
#'   \item{weight_unit}{\code{character}. Unit of measurement for weight (e.g., "kg", "lb").}
#'   \item{weight}{\code{numeric}. Weight of the product in specified units.}
#'   \item{cost}{\code{numeric}. Cost price of the product.}
#'   \item{price}{\code{numeric}. Selling price of the product.}
#'   \item{category_key}{\code{integer}. Unique identifier for the product category.}
#'   \item{category_name}{\code{character}. Name of the product category.}
#'   \item{sub_category_key}{\code{integer}. Unique identifier for the product sub-category.}
#'   \item{sub_category_name}{\code{character}. Name of the product sub-category.}
#' }
#'
#' @source Generated from `fpaR::product`
#'
#' @examples
#' \dontrun{
#' data(product)
#' head(product)
#' summary(product)
#' }
"product"

#' Store Master Dataset
#'
#' This dataset contains information about stores, including their identifiers, geographic details, operational status, and physical attributes.
#'
#' @format A data frame with multiple rows and 11 columns:
#' \describe{
#'   \item{store_key}{\code{integer}. Unique identifier for the store.}
#'   \item{store_code}{\code{character}. Internal code assigned to the store.}
#'   \item{geo_area_key}{\code{integer}. Unique identifier for the geographic area.}
#'   \item{country_code}{\code{character}. ISO country code (e.g., "US", "DE").}
#'   \item{country_name}{\code{character}. Full name of the country where the store is located.}
#'   \item{state}{\code{character}. State or region where the store is located.}
#'   \item{open_date}{\code{Date}. Date when the store was opened.}
#'   \item{close_date}{\code{Date}. Date when the store was closed (if applicable).}
#'   \item{description}{\code{character}. Additional details or notes about the store.}
#'   \item{square_meters}{\code{numeric}. Store size in square meters.}
#'   \item{status}{\code{character}. Current operational status of the store (e.g., "Open", "Closed").}
#' }
#'
#' @source Generated from `fpaR::store`
#'
#' @examples
#' \dontrun{
#' data(store)
#' head(store)
#' summary(store)
#' }
"store"


