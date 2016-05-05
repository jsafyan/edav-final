#hpd
library(dplyr)

hpd_complaints <- request311 %>%
  filter(Agency == "HPD")


duplicate_complaints <- grep('duplicate', hpd_complaints$Resolution.Description,ignore.case=TRUE)
hpd_duplicates <- hpd_complaints[duplicate_complaints,]
hpd_no_duplicates <- hpd_complaints[-duplicate_complaints,]


complaints_inspected <- hpd_no_duplicates[grep("inspected|inspecting", hpd_no_duplicates$Resolution.Description, ignore.case = TRUE),]
complaints_not_inspected <- hpd_no_duplicates[-grep("inspected|inspecting", hpd_no_duplicates$Resolution.Description, ignore.case = TRUE),]

#final
not_inspected_verified_resolved <- complaints_not_inspected[grep("verified that the following conditions were corrected", complaints_not_inspected$Resolution.Description, ignore.case = TRUE),]
not_inspected_verified_resolved$Resolution.Description <- "Complainant verified resolved"

not_inspected_not_verified_resolved <- complaints_not_inspected[-grep("verified that the following conditions were corrected", complaints_not_inspected$Resolution.Description, ignore.case = TRUE),]

attempted_inspected_unable_to_gain_access<- not_inspected_not_verified_resolved[grep("was unable to access", not_inspected_not_verified_resolved$Resolution.Description, ignore.case = TRUE),]

#final
#attempted_inspected_violation1 <- attempted_inspected_unable_to_gain_access[grep("a violation was issued", attempted_inspected_unable_to_gain_access$Resolution.Description, ignore.case = TRUE),]
#attempted_inspected_violation1$Resolution.Description <- "Attempted inspection, violation issued"
#final
attempted_inspected_no_violation1 <- attempted_inspected_unable_to_gain_access[grep("No violations were issued", attempted_inspected_unable_to_gain_access$Resolution.Description, ignore.case = TRUE),]
attempted_inspected_no_violation1$Resolution.Description <- "Attempted inspection, no violation issued"

attempted_inspected_not_able_to_gain_access<- not_inspected_not_verified_resolved[grep("not able to gain access", not_inspected_not_verified_resolved$Resolution.Description, ignore.case = TRUE),]

#final
attempted_inspected_violation2 <- attempted_inspected_not_able_to_gain_access[grep("a violation was issued", attempted_inspected_not_able_to_gain_access$Resolution.Description, ignore.case = TRUE),]
attempted_inspected_violation2$Resolution.Description <- "Attempted inspection, violation issued"
attempted_inspected_no_violation2 <- attempted_inspected_not_able_to_gain_access[-grep("a violation was issued", attempted_inspected_not_able_to_gain_access$Resolution.Description, ignore.case = TRUE),]
attempted_inspected_no_violation2$Resolution.Description <- "Attempted inspection, no violation issued"

#final
attempted_inspected_heat_restored <- not_inspected_not_verified_resolved[grep("was advised by a tenant in the building that heat and hot water had been restored", not_inspected_not_verified_resolved$Resolution.Description, ignore.case = TRUE),]
attempted_inspected_heat_restored$Resolution.Description <- "Attempted inspection, no violation issued"

#final, deleting this group
not_inspected_complaint_open <- not_inspected_not_verified_resolved[grep("following complaint conditions are still open", not_inspected_not_verified_resolved$Resolution.Description, ignore.case = TRUE),]

#final
attempted_inspection_no_heat_required <- not_inspected_not_verified_resolved[grep("Heat was not required at the time of the inspection. No violations were issued", not_inspected_not_verified_resolved$Resolution.Description, ignore.case = TRUE),]
attempted_inspection_no_heat_required$Resolution.Description <- "Attempted inspection, no violation issued"

#final
attempted_inspection_potential_lead_paint <- not_inspected_not_verified_resolved[grep("identified potential lead-based paint conditions", not_inspected_not_verified_resolved$Resolution.Description, ignore.case = TRUE),]
attempted_inspection_potential_lead_paint$Resolution.Description <- "Attempted inspection, potential lead paint"


#complaints_inspected <- hpd_no_duplicates[grep("inspected|inspecting", hpd_no_duplicates$Resolution.Description, ignore.case = TRUE),]

#final
attempted_inspection_violation3 <- complaints_inspected[grep("inspecting another apartment and a violation was issued", complaints_inspected$Resolution.Description, ignore.case = TRUE),]
attempted_inspection_violation3$Resolution.Description <- "Attempted inspection, violation issued"

#final
inspected_violations_previously_issued <- complaints_inspected[grep("inspected the following conditions. Violations were previously issued", complaints_inspected$Resolution.Description, ignore.case = TRUE),]
inspected_violations_previously_issued$Resolution.Description <- "Inspected, violation already issued"

#final
inspected_violation_issued <- complaints_inspected[grep("inspected the following conditions. Violations were issued", complaints_inspected$Resolution.Description, ignore.case = TRUE),]
inspected_violation_issued$Resolution.Description <- "Inspected, violation issued"

#final
inspected_no_violation <- complaints_inspected[grep("inspected the following conditions. No violations were issued", complaints_inspected$Resolution.Description, ignore.case = TRUE),]
inspected_no_violation$Resolution.Description <- "Inspected, no violation"

#final
inspected_section8_failure <- complaints_inspected[grep("A Section 8 Failure was issued", complaints_inspected$Resolution.Description, ignore.case = TRUE),]
inspected_section8_failure$Resolution.Description <- "Inspected, section 8 failure"

final_HPD <- rbind(not_inspected_verified_resolved,attempted_inspected_violation2, attempted_inspected_no_violation2,
                   attempted_inspected_no_violation1, attempted_inspected_heat_restored,
                   attempted_inspection_no_heat_required, attempted_inspection_potential_lead_paint,
                   attempted_inspection_violation3, inspected_violations_previously_issued,
                   inspected_violation_issued, inspected_no_violation, inspected_section8_failure)



#Different options
#Violation inspected and violation issued
#Violation inspected and no violation issue
#Attempted inspection and violation issued
#Attempted inspect and no violation issued
#Attempted inspection and verified resolved
#Complainant verified it was resolved


myLocation <- 'New York City'
maptype = 'terrain'
myMap <- get_map(location=myLocation,
                 source="google", maptype=maptype, crop=FALSE,
                 zoom=11)

attempted_inspection_no_violation_issued <- final_HPD %>%
  filter(Resolution.Description == "Attempted inspection, no violation issued")
ainvi <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = attempted_inspection_no_violation_issued, 
             alpha = 0.5) +
  ggtitle('Attempted inspection, no violation issued')

ainvi

attempted_inspection_potential_lead_paint <- final_HPD %>%
  filter(Resolution.Description == "Attempted inspection, potential lead paint")
aiplp <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = attempted_inspection_potential_lead_paint, 
             alpha = 0.5) +
  ggtitle('Attempted inspection, potential lead paint')

aiplp

attempted_inspection_violation_issued <- final_HPD %>%
  filter(Resolution.Description == "Attempted inspection, violation issued")
aivi <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = attempted_inspection_violation_issued, 
             alpha = 0.5) +
  ggtitle('Attempted inspection, violation issued')

aivi

complainant_verified_resolved <- final_HPD %>%
  filter(Resolution.Description == "Complainant verified resolved")
cvr <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = complainant_verified_resolved, 
             alpha = 0.5) +
  ggtitle('Complainant verified resolved')

cvr

inspected_no_violation <- final_HPD %>%
  filter(Resolution.Description == "Inspected, no violation")
inv <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = inspected_no_violation, 
             alpha = 0.5) +
  ggtitle('Inspected, no violation')

inv

inspected_section8_failure <- final_HPD %>%
  filter(Resolution.Description == "Inspected, section 8 failure")
is8f <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.7, data = inspected_section8_failure, 
             alpha = 0.5) +
  ggtitle('Inspected, section 8 failure')

is8f

inspected_violation_previously_issued <- final_HPD %>%
  filter(Resolution.Description == "Inspected, violation already issued")
ivai <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = inspected_violation_previously_issued, 
             alpha = 0.5) +
  ggtitle('Inspected, violation previously issued')

ivai

inspected_violation_issued <- final_HPD %>%
  filter(Resolution.Description == "Inspected, violation issued")
ivi <- ggmap(myMap, extent = "device")+
  geom_point(aes(x = Longitude, y = Latitude),
             size=0.005, data = inspected_violation_issued, 
             alpha = 0.5) +
  ggtitle('Inspected, violation issued')

ivi


ggplot(final_HPD, aes(x = Resolution.Description)) + 
  geom_bar(position = "stack") + ggtitle('Resolution Description') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
