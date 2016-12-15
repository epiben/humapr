# \code{humap()} takes relevant specifications from user and uses those to invoke \code{ggplot()}.
# Output: \code{ggplot2} object that the user may modify with, e.g., \code{theme}.

humap <- function(data, var, region = "body", gender = "male", type = "topo",
                  quality = "normal", side = "front") {
    # data = tidy data frame
    # var = name of columns in data frame that contains counts for each region
    # region = body region, options: body (default), head, neck, arm,
    # type = topo(graphical), derma(tomes)
    # gender = n(eutral), f(emale), m(male)
    # side = front, back, both
    # quality = draft, normal, high

    # Set up dedicated environment for the humap, so it's easy to call objects
    # and values within the geom functions
        humapr_env <<- new.env(parent = emptyenv())

    # Import appropriate XML file
        geom_file <- paste(paste(type, region, gender, side, quality, sep = "_"), "xml", sep = ".")
        humapr_env$surf <- grImport::readPicture(paste0("data/", geom_file))

    # Coding of body regions
        # Crosswalks between coding systems and pictureGrob shape id's
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
        names(crosswalk$ais) <- c()
        names(crosswalk$simple) <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)

        # User may supply a function that changes (i.e., simplifies) the coding, e.g., merging facial sub-regions
        if (exists("humapr_env$simplify_coding")) crosswalk[humapr_env$simplify_coding[1]] <- humapr_env$simplify_coding[2]

    ggplot(data, aes_string(x = var, fill = "-..count..")) +
        geom_body()
        # + theme(...) at some point
}
