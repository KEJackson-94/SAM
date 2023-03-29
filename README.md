# SAM
The following repository is an update to the original github published by Yao Goulin on SAM (https://github.com/yaoguolin/SustainableAgricultureMatrix)

Main Folders

data_archive: contains original data files pulled and used in calculated of data_raw files
data_raw: contains raw data (i.e., not yet transformed for monotonicity) for environmental, economic, and social dimensions. We maintain folders for archived data from previous years/editions of SAM for recorded keeping purposes and to identify any discrepencies or changes made to the data from their original sources. 
data_trans: contains transformed data based on raw data values so that larger values indicate higher sustainability.
data_score: contains unbounded and bounded scores based on thresholds, and scores derived from normalization methods (SAM_score_normalization).
scripts: folder containing R and python scripts named after the folders in which script outputs are sent.

Additonally you can observe the following folder/file structure to better navigate the repository for your research needs:
~//SAM//
~//data_archive//
~//2021_ed//
~//Econ//
~//Env//
~//Soc//
~//2023_ed//
~//Econ//
~//Env//
~//Soc//
~//data_raw//
-SUSI_Raw.csv (exm)
~//data_score//
~//bounded//
- SUSI_Score_bounded.csv (exm)
~//unbounded//
- SUSI_Score_unbounded.csv (exm)
~//normalization//
- SUSI_Score_normalized
~//data_trans//
- SUSI_trans.csv (exm)
~//scripts//
-data_raw.R
-data_score.R
-data_trans.R
-data_products.R
~//data_products//
  
  
