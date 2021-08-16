#data documentation

#' Cursive written word: csafe
#'
#' @format Binary image matrix. 111 rows and 410 columns.
#' @examples
#' csafe_document = list()
#' csafe_document$image = csafe
#' plotImage(csafe_document$image)
#' csafe_document$thin = thinImage(csafe_document$image)
#' plotImageThinned(csafe_document$image, csafe_document$thin)
#' csafe_processList = processHandwriting(csafe_document$thin, dim(csafe_document$image))
"csafe"

#' Cursive written word: London
#'
#' @format Binary image matrix. 148 rows and 481 columns.
#' @examples
#' london_document = list()
#' london_document$image = london
#' plotImage(london_document$image)
#' london_document$thin = thinImage(london_document$image)
#' plotImageThinned(london_document$image, london_document$thin)
#' london_processList = processHandwriting(london_document$thin, dim(london_document$image))
"london"

#' Full page image of the handwritten London letter.
#'
#' @format Binary image matrix. 1262 rows and 1162 columns.
#' @examples
#' \dontrun{
#' message_document = list()
#' message_document$image = message
#' plotImage(message_document$image)
#' message_document$thin = thinImage(message_document$image)
#' plotImageThinned(message_document$image, message_document$thin)
#' message_processList = processHandwriting(message_document$thin, dim(message_document$image))
#' }
"message"

#' Full page image of the 4th sample (nature) of handwriting from the first writer.
#'
#' @format Binary image matrix. 811 rows and 1590 columns.
#' @examples
#' \dontrun{
#' nature1_document = list()
#' nature1_document$image = nature1
#' plotImage(nature1_document$image)
#' nature1_document$thin = thinImage(nature1_document$image)
#' plotImageThinned(nature1_document$image, nature1_document$thin)
#' nature1_processList = processHandwriting(nature1_document$thin, dim(nature1_document$image))
#' }
"nature1"

#'Two sentence printed example handwriting
#'
#' @format Binary image matrix. 396 rows and 1947 columns
#' @examples
#' \dontrun{
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' plotImage(twoSent_document$image)
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' plotImageThinned(twoSent_document$image, twoSent_document$thin)
#' twoSent_processList = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' }
"twoSent"
