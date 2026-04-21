## Loading Some Data to Use/ Cleaning just for me. 

goats = read.csv("data/goats-la-data-cleaned.csv")

View(goats)

str(goats)

library(dplyr)

goats_subset = goats %>%
  select(Size, UdderDepth, Rear.Udder.Height, Rear.Udder.Arch, 
         Medial.Suspensory.Ligament, Teat.Placement, 
         Teat.Diameter, Teat.Length)

goats_subset

teats_subset = goats_subset %>% 
  select(Size, Teat.Placement, 
         Teat.Diameter, Teat.Length)

teats_subset

# Translating Points to Inches 

# LINEAR SCALE – TEAT LENGTH
# Standard Numbers 

# 5.0”  50
# 4.5”  45
# 4.0”  40
# 3.5”  35
# 3.0”  30
# 2.5”  25
# 2.0”  20
# 1.5”  15
# 1.0”  10
# 0.5”  5

View(teats_subset)

trf_length = teats_subset$Teat.Length[1:5]/10
trf_length

# trf_dia = teats_subset$Teat.Diameter[1:5]

trf_plc = teats_subset$Teat.Placement



