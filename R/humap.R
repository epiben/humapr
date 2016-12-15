# \code{humap()} takes relevant specifications from user and uses those to invoke \code{ggplot()}.
# Output: \code{ggplot2} object that the user may modify with, e.g., \code{theme}.

humap <- function(data, region) {
  # data = tidy data frame
  # region = name of columns in data frame that contains counts for each region
  # tab = logical flag indicating whether input data are already tabulated

  surface_env <- new.env(parent = .GlobalEnv)
  surface_env$surf <- grImport::readPicture("data/body_male_front.xml")
    # This needs to take into account further specification from the user

  # Defining crosswalk between various coding systems and ids of pictureGrob shapes.
  # For now, only AIS coding is supported but implemented in list form to facilitate potential extension to other
  # classification schemes later on
  # This crosswalk thing should probably be in the humap() call instead, so it's just done once...
  crosswalk <- list(
      ais = c( # Ensure to make it a named vector (names are path ids)
          51, # Scalp
          52, # Forehead
          53, # Face
          54, # Eye
          55, # Eyelid
          56, # Ear
          57, # Nose
          58, # Lip
          59, # Neck
          60, # Shoulder
          61, # Arm (upper)
          62, # Elbow
          63, # Forearm
          64, # Wrist
          65, # Hand
          66, # Finger
          # Theres's no LOC = 67 in the AIS scheme
          68, # Back
          # Theres's no LOC = 69 in the AIS scheme
          70, # Chest
          71, # Abdomen
          72, # Buttocks
          73, # Genitalia
          74, # Perineum
          75, # Hip
          76, # Thigh
          77, # Knee
          78, # Leg
          79, # Ankle
          80, # Foot
          81), # Toe
      simple = c( # Based on data from my thesis, mainly for testing purposes but might be useful also "post-publication"
          11, # Head, frontside
          12, # Head, posterior side
          21, # Neck, frontside
          22, # Neck, posterior side
          31, # Chest, frontside
          32, # Chest, posterior side
          41, # Abdomen, frontside
          42, # Abdomen, posterior side
          51, # Pelvic area, frontside
          52, # Pelvic area, posterior side
          61, # Upper arm, frontside
          62, # Upper arm, posterior side
          64, # Forearm, frontside
          65, # Forearm, posterior side
          71, # Palm
          72, # Back of hand
          81, # Legs and feet, frontside
          82) # Legs and feet, posterior side
  )
  names(crosswalk$simple) <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)

  # User may supply a function that changes (i.e., simplifies) the coding, e.g., merging facial sub-regions
  if (exists(surface_env$simplify_coding)) crosswalk[surface_env$simplify_coding[1]] <- surface_env$simplify_coding[2]

  pars <- as.character(structure(as.list(match.call())[-1], class = "uneval")) # => user may supply parameters w/o quotation marks

  ggplot(data, aes_string(x = pars["region"], fill = "-..count..")) +
    geom_body()
    # + theme(...) at some point
}
