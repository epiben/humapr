## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align = "center",
  fig.height = 4,
  fig.width = 3,
  comment = "#>"
)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  devtools::install_github("benskov/humapr")

## ---- message=FALSE------------------------------------------------------
for (p in c("dplyr", "tibble", "humapr", "gridExtra", "grid")) 
    library(p, character.only = TRUE)

sampler <- function(x, n) 
    sample(x, n, TRUE, sample(seq(0.5, 1, length = length(x)), length(x), TRUE))

## ------------------------------------------------------------------------
body_data <- data.frame(loc = sampler(c("head", "neck", "chest", "abdomen", "arm", 
                                        "forearm", "hand", "thigh", "leg", "foot"), 200),
                        side = sampler(c("left", "right"), 200),
                        gender = sample(c("male", "female"), 200, TRUE),
                        year = 2010:2019) 

## ------------------------------------------------------------------------
head(body_data, 10)

## ------------------------------------------------------------------------
humap() +
    geom_body(aes(loc = loc), body_data) 

## ------------------------------------------------------------------------
humap() +
    geom_body(aes(loc = loc, side = side), body_data) 

## ------------------------------------------------------------------------
organs <- c("urogenital", "right_kidney", "left_kidney", "pancreas", "liver", "spleen", 
            "oesophagus", "stomach", "intestine", "heart", "trachea", "hyoid", "larynx", 
            "right_lung", "left_lung")

organ_data <- data.frame(loc = sampler(organs, 300),
                         gender = sampler(c("Female", "Male"), 300),
                         period = sampler(c("2001-05", "2006-10"), 300))

## ------------------------------------------------------------------------
humap() +
    geom_organs(aes(loc = loc), organ_data)

## ---- fig.width=4--------------------------------------------------------
humap() +
    geom_body(aes(loc = loc, side = side), body_data, annotate = "all") 

## ------------------------------------------------------------------------
humap() +
    geom_body(aes(loc = loc), body_data, annotate = NULL) 

## ------------------------------------------------------------------------
humap() +
    geom_organs(aes(loc = loc), organ_data) +
    scale_fill_gradient(low = "grey95", high = "darkgreen")

## ---- fig.width=6--------------------------------------------------------
humap() +
    geom_organs(aes(loc = loc), organ_data) +
    facet_wrap(~ period)

## ---- fig.width=4--------------------------------------------------------
males <- humap() + 
    geom_organs(aes(loc = loc), filter(organ_data, gender == "Male"), annotate = NULL) +
    facet_wrap(~ gender)
females <- humap() + 
    geom_organs(aes(loc = loc), filter(organ_data, gender == "Female"), annotate = NULL) +
    facet_wrap(~ gender)
grid.arrange(males, females, nrow = 1)

