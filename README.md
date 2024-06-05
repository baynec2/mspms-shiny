
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mspms_shiny

<!-- badges: start -->
<!-- badges: end -->

# About

[mspms-shiny](https://gonzalezlab.shinyapps.io/mspms_shiny/) is a Shiny
app designed to be a basic interface for the [mspms R
package](https://github.com/baynec2/mspms) to analyze data from the
[Multiplex Substrate Profiling by Mass Spectrometry (MSP-MS)
method](https://pubmed.ncbi.nlm.nih.gov/36948708/)

On the side bar, you will also see four tabs: About, File Upload,
Output, Stats, and DataViz

## About

The About tab links to the instructions shown on this page.

## File Upload

The File Upload tab is where you will upload your files.

You must upload a design matrix file, which should be a .csv file.

The design matrix file should be a csv file with the following columns:
“sample”, “group”,“condition”,“time”. The sample column should contain
the names of the samples as found in the PEAKS files. The group column
should contain the group that the sample belongs to. This can be any
thing, as long as it is the same per group. The time column should
contain the duration of incubation, and the condition column should
include any experimental condition such as protease inhibitor.

Depending on what upstream proteomics software you are using, you will
then upload export files to the appropriate loader.

### Peaks

Upload your LFQ and ID file in the indicated location. These should be
.csv files.

### Proteome Discoverer.

Upload your proteome discoverer file to the indicated location. This
should be a .xlsx file

## Output

Output contains a rendering of the normalized data, and a button that
says download. This button will allow you to download the data that has
been processed as a .csv file.

## Stats

Stats contains a table of the results of the t-tests (for each condition
and time relative to time 0) as well as ANOVAs testing for an effect of
time. Be patient, when it is finished running a table will appear and
the data will be ready for download.

Both of these data frames can be downloaded by using the download
results

## Dataviz.

The DataViz tab contains a PCA plot, heatmap of the data, volcano plots,
icelogo, and the count of cleavages per position of all the data. Be
patient, these plots take a while to render.

The volcano plots show the results of the t-tests relative to T0 for
each condition.

The ice logo shows the cleavage sequences were found to be significantly
different (qvalue \<= 0.05) and with a log2fc \> 3 relative to T0 for
any condition.

The count of cleavages per position plot shows the count of cleavages
per position of the peptide library for peptides found to significantly
different (qvalue \<= 0.05) and with a log2fc \> 3 relative to T0 for
any condition.
