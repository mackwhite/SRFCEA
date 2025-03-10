###project: SRFCEA
###author(s): Mack White
###goal(s): FACT out-of-system detections POR update ----
###date(s): March 2025
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(readr, tidyverse, scales, ggplot2)

oos22 <- read_csv("../../../../../../Downloads/srfcea_qualified_ext_detections_2022/srfcea_qualified_ext_detections_2022.csv")
oos23 <- read_csv("../../../../../../Downloads/srfcea_qualified_ext_detections_2023/srfcea_qualified_ext_detections_2023.csv")
oos24 <- read_csv("../../../../../../Downloads/srfcea_qualified_ext_detections_2024/srfcea_qualified_ext_detections_2024.csv")
