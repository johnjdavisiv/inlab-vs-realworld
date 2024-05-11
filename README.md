# In-lab versus real-world gait patterns

This repository contains the code to replicate the results of "Are Gait Patterns during in-Lab Running Representative of Gait Patterns during Real-World Training? An Experimental Study" by Davis et al. 2024.  Please cite if you use the data or the code.   


>Davis IV JJ, Meardon SA, Brown AW, Raglin JS, Harezlak J, Gruber AH. Are Gait Patterns during In-Lab Running Representative of Gait Patterns during Real-World Training? An Experimental Study. Sensors. 2024 May 1;24(9):2892.

**🔓 Paper:** [Davis et al. 2024](https://www.mdpi.com/1424-8220/24/9/2892)  
**💿 Data:** [FigShare doi: 10.6084/m9.figshare.23662662](https://figshare.com/articles/dataset/Wearable_sensor_data_during_in-lab_vs_real-world_running/23662662)  

## Data  

Download the data from the FigShare link above. The following four csv files should be unzipped directly into the `/data/` directory.  

`cohort1_inlab.csv` - ECU cohort, in-lab data from treadmill run  
`cohort1_realworld.csv` - ECU cohort, real-world data  
`cohort2_course.csv` - IU cohort, measured course run  
`cohort2_realworld.csv`- IU cohort, real-world run  

Device metrics, except speed, are prefixed by the device (`hrm` or `stryd`). Speed is `enhanced_speed` and is "enhanced" because it is higher bit precision (it is not altered or improved in any way). Note speed is from Stryd on indoor running, and from GNSS (GPS) in outdoor running.  Do *not* use "enhanced altitude" field for calculating incline; it's from the Stryd pod's altimeter, not from the mapping software.  

Note that for privacy reasons, GNSS location data are not available for the realworld cohorts. Processed elevation and turn rate data, however, are included, and GNSS data are included in the `cohort2_course.csv` file, as these runs took place on a standardized course at Indiana University.  

## Code

Code for running the analyses is in /Code/ folder. Each script is independent, you can run them in any order. Main results are from `univariate_analysis.R` and `depth_analysis.R`. Results are exported to appropriately named folders.  

Do be aware that the depth analysis can take >30 hours to run, even on a fast CPU, if you run it with the default `rw_thin_pct` (thinning percentage) of 0.25. Dial it back to 0.01 for testing or for an approximate reproduction of results.

For the MATLAB code that adds incline and turn rate, see `/turn and incline calcs/` which includes a demo GPX file.  