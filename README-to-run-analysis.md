# To run this analysis yourself

By Daniel Moul


* Install the prerequisites. I did the initial analysis with these versions; earlier versions might work too.
    - R 4.2.1 or later
    - RStudio 2022.07.1 or later (or if you are not using RStudio, install quarto directly)
    - make sure you have installed the R packages listed in _targets.R

* Clone this repo: https://github.com/dmoul/american-whaling

* From the R console, in the root directory of the project, run: targets::tar_make()) and debug if necessary
  - Note: It's normal to see these warning messages:
    ```
    Warning messages:
    1:  8874 failed to parse. 
    2:  9267 failed to parse. 
    3: 1 targets produced warnings. Run tar_meta(fields = warnings, complete_only = TRUE) for the messages. 
    ```

* From a terminal in the root directory of this project, run: quarto render
  - Note: the first run can take 30 minutes or more on a MacBook Pro 2019
  - Note: filesystem space required for source and generated pages after first run is about 812 MB
  - Note: run it again if you get this error:
    ```
    Quitting from lines 8-21 (index.qmd) 
    Error:
    ! missing files: _targets/meta/meta
    
    Execution halted
    ```

  - Note: it's normal to get these warning messages
    ```
    output file: crew.knit.md
    
    Warning message:
    Transformation introduced infinite values in continuous x-axis 
    WARNING: Warning: diff of engine output timed out. No source lines will be available.
    ```
  
    ```
    output file: whales.knit.md

    There were 50 or more warnings (use warnings() to see the first 50)
    ```
    
    ````
    output file: products.knit.md

    Warning messages:
    1: Problem while computing `date_out = ymd(day_out)`.
    ℹ  448 failed to parse. 
    2: Problem while computing `date_in = ymd(day_in)`.
    ℹ  526 failed to parse. 
    3: Removed 6 rows containing non-finite values (stat_smooth). 
    4: Removed 6 rows containing missing values (geom_point). 
    ````
    
    ```
    output file: financial.knit.md

    Warning message:
    position_dodge requires non-overlapping x intervals 
    ```
    
    ```
    output file: appendix.knit.md
    
    Warning messages:
    1: Problem while computing `date_out = ymd(day_out)`.
    ℹ  448 failed to parse. 2: Problem while computing `date_in = ymd(day_in)`.
    ℹ  526 failed to parse. 
    3: Problem while computing `tonnage_max = max(c_across(x1:x5), na.rm = TRUE)`.
    ℹ no non-missing arguments to max; returning -Inf
    ℹ The warning occurred in row 1. 
    4: Problem while computing `tonnage_max = max(c_across(x1:x5), na.rm = TRUE)`.
    ℹ no non-missing arguments to max; returning -Inf
    ℹ The warning occurred in row 93. 
    ```

* Open american-whaling-by-the-numbers/index.html to view the analysis

