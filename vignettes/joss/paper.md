---
title: 'dtrackr: An R package for tracking the provenance of data'
tags:
  - R
  - data pipeline
  - consort diagram
  - strobe statement
  - data quality
  - reproducible research
authors:
  - name: Robert Challen
    orcid: 0000-0002-5504-7768
    affiliation: "1, 2"
affiliations:
 - name: Engineering Mathematics, University of Bristol, Bristol, UK
   index: 1
 - name: College of Engineering, Mathematics and Physical Sciences, University of Exeter, Devon, UK
   index: 2
date: 28 June 2022
bibliography: ../dtrackr.bib
---

# Summary

An accurate statement of the provenance of data is essential in bio-medical research. Powerful data manipulation tools available in the `tidyverse` R package ecosystem [@wickhamWelcomeTidyverse2019a] provide the infrastructure to assemble, clean and filter data prior to statistical analysis. Manual documentation of the steps taken in the data pipeline and the provenance of data is a cumbersome and error prone task which may restrict reproducibility. `dtrackr` is a thin wrapper around the standard `tidyverse` data manipulation tools that automatically tracks the processing steps applied to a data set, prior to statistical analysis. It allows early detection and reporting of data quality problems, and automatically documents a set of data transformations as a flowchart in a format suitable for scientific publication, including, but not limited to CONSORT diagrams [@schulzCONSORT2010Statement2010].

# Statement of need

The collection of experimental or observational data for research is often an iterative endeavour, involving curation of complex data sets designed for multiple goals. Systematic data quality checking for such sets is a major challenge, particularly when they are assembled to identify emerging or rapidly evolving issues. Feedback from early data analysis can identify specific data quality issues, resolution of which can considerably improve data for the task at hand. However this requires a clear understanding of why and when individual data items are excluded, which is potentially tedious and may be seen as lower priority compared to statistical analysis. 

Data analysis using `tidyverse` in R is a rapid means of transforming raw data into a format suitable for statistical analysis. The transformations involved can, however affect the results of statistical analysis, and meticulous care must be taken to ensure that any assumptions made during data processing are well documented. It is often too easy to inadvertently exclude data where filtering on missing items, or joining linked data sets with incomplete foreign key relationships.

In complex data analysis, the use of interactive programming environments such as Read-Eval-Print Loops (REPL) in R markdown documents, interim caching of results, or conditional branching data pipelines, can result in the current state of a processed data set becoming decoupled from the code that is designed to generate them. 

To surface these issues bio-medical journal articles are usually required to report data manipulation to an agreed standard. For example, CONSORT diagrams are part of the requirements in reporting parallel group clinical trials. They are described in the updated 2010 CONSORT statement [@schulzCONSORT2010Statement2010], and clarify how patients were recruited, selected, randomized and followed up. For observational studies, such as case control designs, an equivalent requirement is the STROBE statement [@vonelmStrengtheningReportingObservational2008]. There are many other similar requirements for other types of study, such as the TRIPOD statement for multivariate models [@collinsTransparentReportingMultivariable2015]. Maintaining such CONSORT diagram over the course of a study when data sets are being actively collected and data quality issues being addressed is time-consuming.

`dtrackr` addresses these issues by instrumenting standard `tidyverse` data manipulation pipelines. It records the steps taken, records excluded and a summary of the result of each data processing step, as part of the data set itself. In this way data sets retain an accurate history of their own provenance regardless of the actual route taken to assemble them. This history includes a complete record of any data quality issues that lead to excluded records. The history is a directed graph which can be expressed in the commonly used `GraphViz` language [@gansnerOpenGraphVisualization2000] and may be visualised as a flowchart such as in \autoref{fig:figure1}; this uses the Indian Liver Patients disease (ILPD) data set [@ramanaCriticalComparativeStudy; @ramanaCriticalStudySelected2011; @Dua2019] as an example of an observational study and produces a STROBE like flowchart.

![An example flowchart derived directly from a simple analysis of the ILPD dataset demonstrating use of `dtrackr` to generate the key parts of a STROBE or CONSORT diagram. \label{fig:figure1}](figure1-ilpd-consort.pdf)

`dtrackr` was originally conceptualized during an analysis I undertook of the severity of the Alpha variant of SARS-CoV-2 [@challenRiskMortalityPatients2021], and has since been used for other epidemiological studies including an analysis of the incidence of hospitalization of acute lower respiratory tract disease in Bristol [@hyamsIncidenceCommunityAcquired2022], and a comparative analysis of the severity of the SARS-CoV-2 Omicron variant, versus the Delta variant against a range of hospital outcomes [reference pending].

Although the specific example presented here is in the biomedical domain, tracking the provenance of data is a much broader issue, and we anticipate there are many other applications for `dtrackr`.

# Acknowledgements

Thanks for contributions from TJ McKinley. I gratefully acknowledge the financial support of the EPSRC via grants EP/N014391/1, EP/T017856/1, the MRC (MC/PC/19067), and from the Somerset NHS Foundation Trust, Global Digital Exemplar programme.

# References
