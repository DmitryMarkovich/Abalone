############################## Constants defined for the project
## kName - syntax for defining a constant variable
kDataFname <- "abalone_data.txt";

kAttributeNames <- c("Sex", "Length", "Diameter", "Height", "WhlWght",
                     "ShckdWght", "VscrWght", "ShllWght", "Rings");
kBinarizeAttributeNames <- c("Length", "Diameter", "Height", "WhlWght",
                             "ShckdWght", "VscrWght", "ShllWght", "Rings",
                             "Male", "Female", "Infant");

kAttributeLevels <- list(
    Sex = c("M", "F", "I"), Length = NA, Diameter = NA, Height = NA,
    WhlWght = NA, ShckdWght = NA, VscrWght = NA, ShllWght = NA, Rings = NA
    );

kClassColors <- c("black", "red", "green");

kAttributeImportance <- c("Sex", "Length", "Height", "WhlWght", "ShllWght",
                          "VscrWght", "Diameter", "ShckdWght");

kAttributeNamesPCA <- c("Age", "Steroid", "Antivirals", "Fatigue",
                        "Malaise", "Anorexia", "LiverBig", "LiverFirm",
                        "SpleenPalpable", "Spiders", "Ascites", "Varices",
                        "Bilirubin", "AlkPhosphate", "SGOT", "Albumin", "Protime",
                        "Histology", "Male", "Female");
