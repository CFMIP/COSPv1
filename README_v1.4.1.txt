1/24/2015 (Dustin Swales):
A small fix was made to MODIS/moid_simulator.F90. The patch for inline COSP has also been updated
The following were added @ lines 539-541 of MODIS/moid_simulator.F90
+    Cloud_Fraction_Total_Mean(1:nPoints) = Cloud_Fraction_Total_Mean(1:nPoints) /nSubcols
+    Cloud_Fraction_Ice_Mean(1:nPoints)   = Cloud_Fraction_Ice_Mean(1:nPoints)   /nSubcols
+    Cloud_Fraction_Water_Mean(1:nPoints) = Cloud_Fraction_Water_Mean(1:nPoints) /nSubcols
