# todo.md

## index.qmd:

-   DONE: Use something other than "Welcome"?
    -   changed to Introduction, and introduction.qmd changed to Overview

## products.qmd:

-   DONE: change references to df\_\*
-   DONE: calculate n_voyages_for_plot after all filtering out
-   DONE: Reword "Below is an alternate view, taking..."

## crew.qmd:

-   DONE: replace "Caucasian"
-   STET: Figure 8.6: Crew age - better to use white lines
-   DONE: Replace footnote https://en.wikipedia.org/wiki/Average_human_height_by_country, measured 2015-2018 with normal reference

## whales.qmd:

-   DONE: Voyages recording at least 100 strikes is missing a caption
-   DONE: Make it clearer that fin and finback whales are the same. Better would be to use 'fin whale' instead

## financial.qmd:

-   DONE Double-check annual numbers in Figure 9.4: Hypothetical investors' view: cumulative return from Charles W. Morgan's voyages
-   DONE: Figure 9.6: TODO: Update this ...Median revenue per voyage declined during C.W. Morgan's operation
-   DONE: Figure 9.11: Revenue per day during C.W. Morgan's operation by decade
    -   DONE: are duration_days above correct? Seems like there are voyages some with too few days for the amount of revenue
    -   DONE: can I find more voyages with duration_days than the 800+ plotted above?
    -   DONE: Clarify that the above includes simulated day-of-year data
-   DONE: Sale prices for Charles W. Morgan's product is missing a figure number
-   DONE: Inflation index during the years of the CWM?

## appendix.qmd:

-   DONE: Add more about data cleaning for products.qmd
-   DONE: Add more commentary in Voyage outlier data likely to be data errors
-   DONE: Make a proper reference of http://www.petroleumhistory.org/OilHistory/pages/Whale/whale_barrel.html

## References

\@website{XXX2022,

title = "Finback whale",

organization = "Endangered Species Coalition",

howpublished = "web page",

year = "Accessed 2022-08-26",

url = "https://www.endangered.org/animals/finback-whale/",

copyright = "2020 Endangered Species Coalition"

}

## In general

### Code

-   Should I move away from chunking data into decades, for example

    -   Figure 3.11: Number of observations per voyage - by decade
    -   Figure 3.12: Number of observations per voyage - by decade
    -   Figure 4.3: Vessel tonnage - by decade
    -   Figure 7.2: Number of encounters by species each decade (most common species)
    -   Figure 7.3: Number of encounters by species each decade (less common species)
    -   Figure 7.18: Percentage of whales sighted and struck during in the decade - most common species
    -   Figure 7.19: Percentage of whales sighted and struck during in the decade - less common species
    -   Figure 7.21: Percentage of species' sightings or strikes that occurred in the decade
    -   Figure 7.24: Average whales tried per strike
    -   Figure 7.26: Percentage of species' tries that occurred in the decade
    -   no figure number Sale prices for Charles W. Morgan's product
    -   Figure 9.2: Revenue for Charles W. Morgan's product each voyage

-   DONE: Ensure a replicatable way of creating the list of libraries to load

    -   in \_targets.R

-   DONE: "excluding" vs "not showing" outliers in plots

-   DONE: Ensure I am including all segments of voyages when adding up product, revenue, distance, etc.

    -   voyage segments in df_voyage are distinguished by a change in the master. Do I need to do anything other than keep the first segment in df_voyage_distinct where I later report complete voyage metric? No, because...
        -   product totals are replicated for all segments
        -   year_out, day_out, year_in, day_in are replicated or all segments
        -   voyage segments are not included in df_log, so it's irrelevant

-   DONE: Minimize duplicates in crew members when there are multiple crewlists for one voyage, so df_crew is more accurate

    -   addressed in functions.R prepare_crew_list(); added crewentry_id_unique column in df_crew
    -   there are still some duplicates due to variations in spelling first or last name; individuals are not worth the effort to correct
    -   add comment in the Appendix re: identifying unique crew members

-   DONE: Is there a way to avoid dropping voyages if port visits last \>= 7 days? Would that provide significantly more good voyage tracks?

    -   moving max_obs_gap from 7 to 14 includes a lot of bad plots with lines that cross continents.
    -   the 138 tracks we get with max_obs_gap == 7 is enough

-   DONE: Change map image dimensions so that there aren't white bands at top and bottom of image block

    -   moved to saving images with ggsave then plotting them
    -   OPTIONAL TODO: don't hard-code directory in which figure output is saved

-   DONE: Make voyage plot lines thinner

-   DONE: Be consistent in financial.qmd in how I refer to C.W. Morgan

-   DONE: Move mp4 files created in whales.qmd somewhere more appropriate

    -   putting them in figure-output with other generated plots

-   Look for "TODO" comments in code and decide whether to do anything

    -   DONE: functions.R
    -   DONE: mapping-prep.R
    -   DONE: constants.R
    -   DONE: settings.R
    -   DONE: \_targets.R

-   Clean up comments at the beginning of most functions

-   Address plot warning messages related to transformation: \>Warning message: \>Transformation introduced infinite values in continuous x-axis \> \>Removed 6 rows containing non-finite values (stat_smooth).

    -   crew.qmd
    -   whales.qmd: There were 19 warnings (use warnings() to see them)

-   DONE: ERROR: whaling.qmd 7.5 Effectiveness over time

    -   Error in scale_y\_percent(): could not find function "scale_y\_percent" - change to scale_y\_continuous()

-   DONE: Reduce black light color bloom in 100% area plots in whales.qmd

    -   use alpha = 0.8 so it's not so much like a black light color bloom

-   Use better distance function for determining unique crew members across voyages

Define distance function to identify identical crew among the `r nrow(df_crew) crew records` (which already have been reduced from a larger number by consolidating crew records when voyages have multiple segments)

+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| test                                   | weighting | notes                                                                                                                                                                                                     |
+========================================+===========+===========================================================================================================================================================================================================+
| name_last = name_last                  |           | must be identical                                                                                                                                                                                         |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| birth_year = birth_year                |           | must be identical; if this is too strict due to data error, consider using a range instead (e.g, +/- 3 years                                                                                              |
|                                        |           |                                                                                                                                                                                                           |
|                                        |           | birth_year \<- year_out - age                                                                                                                                                                             |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| name_first = name_first                |           | must be identical, after expanding common abbreviations                                                                                                                                                   |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| birthplace = birthplace                |           | do some manual cleaning; if that's not enough consider fuzzy match                                                                                                                                        |
|                                        |           |                                                                                                                                                                                                           |
|                                        |           | 24% of records have birthpalce                                                                                                                                                                            |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| res_city = res_city                    |           | do some manual cleaning; if that's not enough consider fuzzy match                                                                                                                                        |
|                                        |           |                                                                                                                                                                                                           |
|                                        |           | 57% of records have res_city                                                                                                                                                                              |
|                                        |           |                                                                                                                                                                                                           |
|                                        |           | what if people move during their lifetime?                                                                                                                                                                |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| height = height                        |           | likely to be accurate for 18 \<= age \<= 50)                                                                                                                                                              |
|                                        |           |                                                                                                                                                                                                           |
|                                        |           | 42% of records have height                                                                                                                                                                                |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| hair                                   |           | not reliable; do not use                                                                                                                                                                                  |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| skin                                   |           | probably not reliable, since description is so subjective; higher-level category might be somewhat more useful                                                                                            |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| citizenship                            |           | not reliable due to limited data and inconsistent expression                                                                                                                                              |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| not on another voyage at the same time |           | where certainty is low, check whether record for candidate unique person includes a differnt voyage_id with overlapping years between year_out and year_in (allowing overlap on the year_out and year_in) |
+----------------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

Crew uniqueness distance algorithm issues and how I will deal with them

-   include birth_year, birthplace, res_city in df_crew
    -   Do I need to do any manual cleaning of birthplace, res_city?

    -   Which fuzzymatch() should I use?
-   What to do about missing data?
    -   Only use comparison if data exists

in test-unique-crewmember.qmd

-   df5_count: 85123 rows

-   check

    | Colson | Herbert |
    |--------|---------|

### Formatting & generation

-   DONE: Explore having a title page

    -   Not as relevant for html output

-   DONE: Try to generate to docx (and open in LibreOffice and Apple Pages)

    -   size is 37 MB in general: gt-generated tables do not render. See for example:
        -   Figure 8.26: Sometimes there were conditions placed up cooks receiving slush
        -   Table 2.1: Summary of whale species encountered - did not render
    -   in general: images are low res and don't span the body text width (as they do in html)
    -   vessels: Figure 5.7: Vessels' fates each voyage by decade - text too big / cropped at edge of image
    -   whales: picture at beginning didn't render
    -   whales: animations are not included: Figure 9.9: Sperm whale encounters - yearly animation

-   DONE: use docx to find spelling errors

-   DONE: Try to generate to pdf

    -   with this YAML in \_quarto.yml

    <!-- -->

        format: pdf
        number-sections: true
        colorlinks: true
        cover-image: "./images/baleen-large-5733725597_0f034cf53d_k_d.jpg"

    -   Had make all image references local (couldn't get them over the web)
    -   Had to disable mp4 files (can't embed videos)
    -   had to disable multi-page gt()-generated table in the appendix - got this error:
        -   "compilation failed- error<br>LaTeX Error: Not in outer par mode."
    -   then it generated a 55MB PDF
    -   image quality is much better than docx
    -   however it had a number of deficiencies
        -   default format has way too much white space
        -   @sec references are not resolved beyond the current doc
            -   looks like this: "See Chapter Masters, Chapter Crew and Chapter Financial returns."
            -   I believe I would need to prefix each with \<filname.qmd:@sec-...\>
        -   reference text is printed inline but does not link to the reference at the end
        -   some images are too small and default horizontal location is disappointing (out-width doesn't seem to work)
        -   some images are too big
            -   example in financial.qmd: Figure 1: The Charles W. Morgan moored at Mystic Seaport
        -   Figure numbering starts from 1 in each qmd document
        -   TOC lists everything but does not include hotspots to navigate to the element
        -   wide tables are truncated on the right,
            -   e.g. in crew.qmd: Figure 26: Sometimes there were conditions placed upon cooks receiving slush

-   DONE: Try other bootstrap themes beyond cosmo (e.g., darkly)

    -   don't want to use darkly: white text on dark-grey background is too stark
    -   slate is better than darkly but white text is still too stark
    -   yeti seems good (but not better than cosmo)

-   DONE: How big is a zip of the html pages and related files? 143 MB

-   DONE: Try turning on in-book search

    -   didn't work

-   Directory name of files (change from "\_doc")

    -   changed to \_whaling-by-numbers
    -   Should I change again when publishing (depends where I publish)

-   CANCEL: Smaller font size of code extract in Appendix Crew skin color

    -   couldn't get fontsize yaml property to work within a code chunk

-   SKIP: Try other biblio formats - are any better?

    -   There are tons of CSL formats at https://github.com/citation-style-language/styles (too many to randomly try)

-   SKIP: Can I get search to work?

    -   doesn't seem to work even though (1) can get search box in header to show up; and (2) search.json content is created

-   Write readme for someone who wants to run the analysis (and test it)

-   For each abc.qmd why is there a directory abc_files in the same location after 'quarto render'

    -   should they be deleted automatically after files are moved to output dir? How to turn that on?

### Publish

-   name of project (directory and .Rproj file)

-   push to github

    -   what about mp4 files?

-   Publish on GitHub or Quarto Pub site?

-   add to sandbox project list and push to github

-   update my title on https://dmoul.github.io/papers-presentations/papers-presentations.html

-   Title above ToC should link to start (check once project is published)
