## Size Distribution Analysis for Labeling Days

This is just to showcase that the clone sizes in 2D conform broadly to those in 3D, suggesting that both boundary driven growth and imposing boundary conditions (2D: Well, 3D: Maintaining force balance without causing distension)

### Folder Organization

        .
        ├── AUTHORS.md
        ├── LICENSE
        ├── README.md
        ├── bin
        ├── config
        ├── data
        │   ├── external
        │   ├── interim
        │   ├── processed
        │   └── raw
        ├── docs
        ├── notebooks
        ├── reports
        │   └── figures
        └── src
            ├── data
            ├── external
            ├── models
            ├── tools
            └── visualization


### Notes

#### Folding observations

<!-- Not yet! : Due to the proximity of labeling within groups observed, labeling relationships are paired thusly (15, 18) ==> DAY 3,  (12, 09) ==> DAY 10, (06, 03) ==> DAY 15 -->


#### What to extract for this experiment:

I need the following information (that can be reasonably gathered from this dataset):

- Clone Size Distribution (x 3 Subclonal Colors)
- Distance from boundary (boundary class doesn't matter)
- Colony area

#### 22/05/20

Using settings of 20 for gaussian blur may be obscuring the data for Day 15 - if the analysis doesn't match up, i will repeat these analyses for this data.
