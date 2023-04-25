# SAM
The following repository is an update to the original github published by Yao Goulin on SAM (https://github.com/yaoguolin/SustainableAgricultureMatrix)

**Main Folders**

**data_archive:** contains original data files pulled and used in calculated of data_raw files

**data_raw:** contains raw data (i.e., not yet transformed for monotonicity) for environmental, economic, and social dimensions. We maintain folders for archived data from previous years/editions of SAM for recorded keeping purposes and to identify any discrepencies or changes made to the data from their original sources.

**data_trans_score:** contains transformed data based on raw data values so that larger values indicate higher sustainability. Scores are then derived from normalization methods (SAM_score_normalization).

**scripts:** folder containing R and python scripts named after the folders in which script outputs are sent.

**data_reference:** this is a temporary folder containing data_raw and data_score files. Rewriting scripts in R and python will rquire that we attempt to replicate
these results.

Additonally you can observe the following folder/file structure to better navigate the repository for your research needs

  - **//data_archive//**
    - //2021_ed//
      - //Econ//
      - //Env//
      - //Soc//
    - //2023_ed//
      - //Econ//
      - //Env//
      - //Soc//
  - **//data_raw//**
    - //2021_ed//
      - *SUSI_Raw.csv* (exm)
    - //2023_ed//
  - **//data_score//**
    - //2021_ed//
      - //bounded//
        - *SUSI_Score_bounded.csv* (exm)
      - //unbounded//
        - *SUSI_Score_unbounded.csv* (exm)
      - //normalization//
        - *SUSI_Score_normalized.csv* (exm)
    - //2023_ed//
      - //bounded//
      - //unbounded//
      - //normalization//
  - **//data_trans//**
    - //2021_ed//
      - *SUSI_trans.csv* (exm)
    - //2023_ed//
  - **//scripts//**
    - *data_raw.R*
    - *data_trans_score.R*
    - *data_products.R*
  - **//data_products//**
    - //2021_ed//
      - *score_report.zip*
      - *patchplot.zip*
      - *worldmap.zip*
      - *correlation_matrix.zip*
    - //2023_ed//
  - **//data_reference//**
    -  //data_raw//
    -  //data_score//
