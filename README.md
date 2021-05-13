# ValenceMetaAnalysis

R meta-analysis code for "Valence framing effects on moral judgments: A meta-analysis" (McDonald, Graves, Yin, Weese, Sinnott-Armstrong, 2021).  

The file `Meta_Analysis_CleaningNew.R` takes data from a shared-author Google Sheet used to collect meta-analysis data on the included articles, cleans the data, and prepares the dataframe df.RData that is used for statistical modeling and analysis.

The file `MetaMultiLevelModel_KelSubgroups.R` uses df.RData to fit the statistical model both for the overall pooled effect size models and for the subgroup analyses reported in the paper.

Any questions should be directed to the corresponding author: walter.sinnott-armstrong@duke.edu. 
