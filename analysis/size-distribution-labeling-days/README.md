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

- Clone Size Distribution ECDF (Comparison between Colors on each condition)
- Clone Size Distribution EDCF (Aggregate of x 3 Subclonal Colors)
- Distance from boundary (boundary class doesn't matter)
- Colony area
- Diffusion characteristics for large subclones extending to the boundary? The way to quantify this would be to have a measure similar to the one that was done by Hallatsheck back in 2008. The way to do it would be to find domains that are clear, segment the colony for them, rederive the coordinates for those domains, and then calculate the line of best fit (and the coordinates) for those paths. Then calculate the transverse displacement coordinates for the lines.


#### 22/05/20

Using settings of 20 for gaussian blur may be obscuring the data for Day 15 - if the analysis doesn't match up, i will repeat these analyses for this data.


#### 01/06/20

The fact is that for subclones at different labeling days, for large subclones there may either be a preference for yPET or dTomato subclones. That fact is not reflected, however, in the data. What needs to be done is that the probability for encountering subclones of either color MORE frequently for all (or potentially sizes larger than a certain value) sizes  needs to be demonstrated. THe way I propose  on doing that is by showing the number of times Frequency for dTomato was greater than yPET (and vice verse). Graph X axis will be number of times subclones of all sizes were more frequent for dTomato (1) and yPET (2) and then the same for sizes above 10^-3 um^2  


#### 03/06/20

I recognize now that at the colony size observed, it would  be impossible to call the distance relationship because there's no idea what the growth frontier of the colony looks like. This means that within the data there is a lot of observations of small subclones near the boundary simply because the cells in the colony hit the boundary early in growth. Because the colony is nearly at the boundary at every point along the well edge it is impossible to call what the growth boundary was and what the well boundary was. I am leaving this out of the calculation and putting it in later images.

#### 04/06/20

Need to calculate the two graphs in my notebook. 
