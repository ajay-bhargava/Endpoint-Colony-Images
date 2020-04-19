#### Analysis of Perimeter Subclones
_For a figure to assess the length of the total colony perimeter occupied by subclones_

The proposed workflow is as follows:

1. Boundary Subclone ID's will be extracted using `%>%` strategy.
2. Boundary coordinates will be extracted, alongside colony coordinates which have condition that they aren't against the *well*
3. For each boundary subclone, a distance measure will be performed to determine the amount of the subclone that sits at the length of the boundary for a given distance of 10~20 pixels.
4. The length of the resulting contiguous set of coordinates will be calculated using the distance function.
5. Data will be summarized and plotted.

#### Expectations
_What the results may look like_

The expectations here are as follows:

   The expectation is that the average boundary subclone perimeter length (summarized) by colony and then compared against condition, will result in shorter boundary lengths being occupied for conditions where mixing is a factor.

##### Project Organization

    .
    ├── AUTHORS.md
    ├── README.md
    ├── data
    │   └── Therapy-Resistant-Subclone-Size-Distribution.rds
    ├── environments
    ├── reports
    │   └── figures
    ├── scheduling
    │   ├── errors
    │   └── reports
    └── src
