Subclone-Size-Distribution
==============================

This is a datascience pipeline to calculate the subclone size distribution, number of EdU positive points per subclone, subclone coordinate derivation, number of points in the colony touching the boundary, and segmentation of individual subclones by location, extracted from colour and location within the colony. Several calculations are made, namely developed in R, following segmentation in ImageJ using a dedicated Macro for this purpose.

Project Organization
--------------------

``  Subclone-Size-Distribution/
  ├── config
  ├── data
  │   ├── R-outputs
  │   ├── processed-acqusitions
  │   └── raw-acquisitions
  ├── reports
  │   └── figures
  ├── scheduling
  │   ├── errors
  │   └── reports
  └── src
      ├── ImageJ-scripts
      ├── R-analysis-scripts
      ├── tools
      └── visualization``



Per Datapoint folder, structural organization of data (example):
----------------------------------------------------------------

``  /Users/bhargaa/Documents/EXP087-Subclone-size-distribution-measurement/data/processed/
  └── 01-CTRL-N3
      ├── 01-CTRL-N3-Colony-Data
      │   ├── 01-CTRL-N3-Colony-Coordinates-ROI.roi
      │   └── 01-CTRL-N3-Colony-Coordinates.csv
      ├── 01-CTRL-N3-EdU-Coordinates
      │   ├── 01-CTRL-N3-EdU-Maxima.csv
      │   └── 01-CTRL-N3-EdU-ROI-List.zip
      ├── 01-CTRL-N3-dTomato-Clones
      │   ├── 01-CTRL-N3-dTomato-Clones-Coordinates.csv
      │   └── 01-CTRL-N3-dTomato-Clones-ROI-List.zip
      ├── 01-CTRL-N3-yPET-Clones
      │   ├── 01-CTRL-N3-yPET-Clones-Coordinates.csv
      │   └── 01-CTRL-N3-yPET-Clones-ROI-List.zip
      ├── 01-CTRL-N3.jpg
      └── 01-CTRL-N3.tif``

A CSV file will exist for each item analyzed. The following data-classes exist:
- Colony-Coordinates
- EdU-Maxima
- dTomato-subclones
- yPET-Subclones

From all dataclasses, a unique identifier for each can be used to match data to the final dataframe, this is: N-ID-Treatment


## Notes

### _22/02/20_

Need to create two plots. One plot is going to be one where a histogram of the frequency distribution is described for each colony, and then another for all colonies within the grouping treatment. Finally, an ECDF is to be plotted for all conditions and all treatments and a comparison is to be made where the distributions are tested using the Komologrov-Smirnov test.

The K-S test does as follows: The KS test is premised on testing the "sameness" of two independent samples from a continuous distribution. If that is the case then the probability of ties should be astonishingly small. The test statistic is the maximum distance between the ECDF's of the two samples. The p-value is the probability of seeing a test statistic as high or higher than the one observed if the two samples were drawn from the same distribution.

### _06/04/20_

I need to do the following:

- perform an update to the clonal statistics calling functions such that when colony coordinates are interpreted, knowledge of the identity of the colony coordinates are also carried through to the analysis. This means that when the fp-distance-from-boundary-analysis.R is conducted, and minimum distance is derived, the derivation coordinates are tested against the positivity towards the boundary coordinates list.

- to execute this, all colony coordinate files must be modified such that they contain an extra column that indicates whether the coordinate is touching the well boundary or not. This can be done as a separate file.

### _10/04/20_

- Script writing is complete, and program is ready to be executed. Following this, I will sort through the images by creating a python script to concatenate the images, and identify which subclones need to be manually removed. I will also use the output of the script to calculate which subclones are indeed boundary and which are non-boundary.

- Following this I will start plotting the data and the outcomes from this experiment.  

### _11/04/20_

- Successfully ran the processing toolkit for subclone data.
- defined the following for processing RDS file exported from SLURM script:
  # 1) Named Spurious subclones are to be removed following curation in images with faulty segmentations
  # 2) Subclone size is to be normalized to colony size
  # 3) Number of EdU points is to be normalized to colony size
  # 4) Boundary subclones are to be identified as kind(Boundary/Interior/Surfing) when they meet the following conditions:
      # A) The distance from the subclone to the boundary edge (defined as D.Boundary) is less than 40 pixels (which is 100 microns) ==> Boundary

      # To be defined later #
      #######################
      # B) The distance from the subclone to the Surfing edge (defined as D.Colony) is less than the mean EdU distance from the boundary ==> Surfing
      # C) The distance from the subclone to the Surfing edge (defined as D.Colony) is greater than the mean EdU distance from the boundary ==> Interior
      #######################
      # How will B, C be defined: Computing the mean distance from a list of distances for each EdU point from the non-well boundary
