#' RaschSim() Function
#'
#' This function generates dataset which fits the Rasch measurement model
#' @param items,persons: two numbers to be operated
#' @keywords RaschSim
#' @export
#' @examples
#' RaschSim(10, 100)

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
