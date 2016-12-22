mapped_regions <- c("body", "head", "neck", "chest", "abdomen", "pelvis",
                    "arm", "elbow", "forearm", "thigh", "knee", "leg", "foot")

# DO THIS WITH EXCEL INSTEAD, SAVE AS .CSV FILE AND IMPORT APPROPRIATE FILE HERE

ais <- matrix(NA, ncol = length(mapped_regions), nrow = 29,
              dimnames = list(code = c(51:66, 68, 70:81),
                              mapped_region = mapped_regions))
    ais[rownames(ais) %in% 51:58, "body"] <- 1
    ais[rownames(ais) %in% 59, "body"] <- 2
    ais[rownames(ais) %in% 51:58, "body"] <- 1

crosswalks <- list(
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
        "11" = 1, # Head, frontside
        "12" = 1, # Head, posterior side
        "21" = 2, # Neck, frontside
        "22" = 2, # Neck, posterior side
        "31" = 3, # Chest, frontside
        "32" = 3, # Chest, posterior side
        "41" = 4, # Abdomen, frontside
        "42" = 4, # Abdomen, posterior side
        "51" = 5, # Pelvic area, frontside
        "52" = 5, # Pelvic area, posterior side
        "61" = 6, # Upper arm, frontside
        "62" = 6, # Upper arm, posterior side
        "64" = 7, # Forearm, frontside
        "65" = 7, # Forearm, posterior side
        "71" = 8, # Palm
        "72" = 8, # Back of hand
        "81" = 9, # Legs and feet, frontside
        "82" = 9) # Legs and feet, posterior side
)