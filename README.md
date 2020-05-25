## COVID-19 PH Infographics
[![Website shields.io](https://img.shields.io/website-up-down-green-red/http/shields.io.svg)](https://rbrtbmnglg.shinyapps.io/covid19ph-infographic/)
[![Open Source Love svg2](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badges/)
[![HitCount](http://hits.dwyl.io/rbrtbmnglg/badges.svg)](http://hits.dwyl.io/rbrtbmnglg/_covid19phinfographics)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/rbrtbmnglg/_covid19phinfographics)
![GitHub contributors](https://img.shields.io/github/contributors/rbrtbmnglg/_covid19phinfographics)
![GitHub last commit](https://img.shields.io/github/last-commit/rbrtbmnglg/_covid19phinfographics)
![GitHub Release Date](https://img.shields.io/github/release-date/rbrtbmnglg/_covid19phinfographics)
<br><br>
Simple one-page, infographics written in R and Shiny using the latest available datasets of COVID-19 cases in PH provided by DOH on their public storage.

Visit the infographics here https://rbrtbmnglg.shinyapps.io/covid19ph-infographic/

## Updates
### :ear_of_rice: v1.0.0 Abra
- Selected cases information are displayed:
   - total # of cases
   - total # of deaths
   - total # of recoveries
   - total # of new cases (based on the latest dataset on the page not based on the latest dataset by DOH)
   - gender percentage
   - oldest age
   - average age
   - total # of pregnant
- Two plots are added.
   - cases over time (line graph)
   - top 5 region (pie graph)
- Supported both desktop and mobile display.
- Tested display on mobile with < 400px width.
- Info modal is displayed upon entering the page to inform visitor on the latest data date.

### :ear_of_rice: v1.0.1 Abra
- Added new cases information:
   - total # of new deaths
   - total # of new recoveries
- Implemented lubridate on two date columns
- New design for the new cases stats
- Added corona icon on top of the header title

### :ear_of_rice: v2.0.0 Benguet
- Complete redesigned:
   - combined total and new
   - changed layout
- Added new cases information:
   - total # of active cases
   - total # of critical
   - total # of severe
   - total # of mild
   - total # of asymptomatic
- New plot
   - death vs recovered time series linegraph
- Removed top 5 reagion plot for now
- Removed oldest case for now

## License
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/rbrtbmnglg/_covid19phinfographics/blob/master/LICENSE)
