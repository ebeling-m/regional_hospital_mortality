# regional_hospital_mortality
Regional mortality differences after emergency hospital admission in Germany 


# Created variables in SAS code 
#######################################
# yearGRP: yearly variable  
# alterGRP: age group variable (10 year interval)
# alterGRP_5: age group variable (5 year interval) 
# entl_grdNew: categorical variables for reason of hopsital discharge based on original vraiable (7, 1 = (1,2), 6 = (6,8,9,10), 999 = other )
# icd_hd3NEW:  categorical variables of main diagnoses (1: myo. infraction, 2: ischeamic hearty disease, 3: stroke, 4: Pneumonia, 999: Other )
# ndNUM_RANK: categorical variables of quartile of number of secondary diagnoses of patient (0, 3), distribution taken caluclated for ten year age group and three pooled calendar years and women and men seperately 
# ndDEM: Indicator variable if patient had a secondary diagnoses in ICD chapter F (0 - No, 1- Yes)   
# ndDIA: Indicator variable if patient had a secondary diagnoses in ICD chapter E (0 - No, 1- Yes)   
# ndALK:  Indicator variable if patient had a secondary diagnoses in ICD chapter K (0 - No, 1- Yes)   
# ndCAN: Indicator variable if patient had a secondary diagnoses in ICD chapter C-D49 (0 - No, 1- Yes)  
# Heart:  Indicator variable if patient received Percutaneous transluminal vascular intervention on heart and coronary vessels (0 - No, 1- Yes) 
# StrokeT: Indicator variable if patient received systemic thrombolysis (0 - No, 1- Yes), (not used in this version) 
# Intence: Indicator variable if patient received intensive care (0 - No, 1- Yes)																						  	
# Cases: Number of cases with similar variable combination (Aggregate)
