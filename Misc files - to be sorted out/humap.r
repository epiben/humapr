# \code{humap()} takes relevant specifications from user and uses those to invoke \code{ggplot()}. 
# Output: \code{ggplot2} object that the user may modify with, e.g., \code{theme}.

humap <- function(data, region) {
  # data = tidy data frame
  # region = name of columns in data frame that contains counts for each region
  # tab = logical flag indicating whether input data are already tabulated
  
  surface_env <- new.env(parent = .GlobalEnv)
  surface_env$surf <- grImport::readPicture("data/body_male_front.xml")
    # This needs to take into account further specification from the user
  
  pars <- as.character(structure(as.list(match.call())[-1], class = "uneval")) # => user may supply parameters w/o quotation marks
  
  ggplot(data, aes_string(x = pars["region"], fill = "-..count..")) +
    geom_body()
    # + theme(...) at some point
}
