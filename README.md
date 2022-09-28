# Interpretation-of-sea-level-projections
Supplementary Materials of Rohmer et al. (2020): https://egusphere.copernicus.org/preprints/2022/egusphere-2022-435/

Available files are:
- run_SupplMat.R: the main R script to perform the diagnostics and the different analyses (levels 1 - 3)
- utilsPLOT.R: functions for plotting
- Diagnostics.zip: the zip file with the png figures, named 'GrIS_CaseXXX_yYYY.png', that depict the diagnostic for case XXX for prediction time YYY
./SupplementaryMaterials:
- RData files for each prediction time YYY "Shapley_yYYY" with:
S: matrix N=55 cases x d+1: SHAP values for the d inputs
YHAT: ML-based predictions of the sea level for the 55 cases
YTRUE: true values for the 55 cases
mae: mean absolute error
- RData file containing the design of experiments "DOE_GrIS_MIROC5-RCP85.RData"
doe: matrix with values of the d=9 inputs
