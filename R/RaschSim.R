#' RaschSim() Function
#'
#' This function generates dataset which fits the Rasch measurement model
#' @param items: the number of items
#' @param persons: the number of persons
#' @return
#'
#' i.loc: generating item difficulty parameter
#'
#' p.loc: generating person ability parameter
#'
#' resp: generating personXitem matrix
#'
#' @examples
#'    x<- RaschSim(10, 100)
#'    x
#' @export

RaschSim <- function(items, persons) {

  sim.items <- runif(items,-3, 3)
  sim.persons <- rnorm(persons, 0, 1)

  dif <- outer(sim.persons, sim.items, "-")
  prob <- exp(dif) / (1 + exp(dif))
  prob
  sim.data <- (prob >= 0.5) * 1

  output        <- list()
  output$i.loc  <- sim.items
  output$p.loc  <- sim.persons
  output$resp   <- sim.data
  return(output)
}
