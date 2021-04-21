# useful general color scheme
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"),
            f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921",
            "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E",
            "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B",
            "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0",
            "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64",
            "#599861" )

# colors for CCS Level 1 causes of death
colors.ccs.level.1 <- mycols[c( 62,  # Cardiovascular diseases
                                48,  # Respiratory diseases
                                57,  # Cancers
                                40,  # Injuries
                                12,  # Neuropsychiatric disorders
                                30,  # Blood diseases
                                22,  # Digestive system diseases
                                13,  # Endocrine disorders
                                36,  # Genitourinary diseases
                                60,  # Infectious and parasitic diseases
                                6,   # Musculoskeletal and connective tissue diseases
                                46,  # Nervous system diseases
                                43,  # Skin and subcutaneous tissue diseases
                                24)] # Other

# colors for storms and hurricanes
colors.storm.hurricane = mycols[c(13, # Storms
                                58)]  # Hurricanes

# colors for ER and non-ER
colors.er.noner =        mycols[c(4,  # ER
                                44)]  # non-ER
