# logLime

[![DOI](https://zenodo.org/badge/475898719.svg)](https://zenodo.org/badge/latestdoi/475898719)

## Installation/update

Currently package is not submitted to CRAN. Nevertheless you can easily install it using the *remotes* package:

1.  First you need to install the *remotes* package, **if you don't have it already**:\
    `r install.packages('remotes')`

2.  Next you can install *logLime*: `remotes::install_github('tzoltak/logLime')`

## Usage

### Preprocessing log-data exported from Lime Survey platform

1.  Use function `separate_logdata_types()` to transform raw log-data streams exported from Lime Survey in the form of answers to text-format questions into a list that contains three different types of *paradata*:
    -   *systemInfo* - information about respondents' system configuration,
    -   *inputPositions* - information on position of survey form controls on a given survey screen for a given respondent (useful for example for drawing backgrounds for cursor traces),
    -   *actions* - information about actual interactions of respondents with the survey interface.
2.  Then use `remove_problems()` to remove records that were identified to pose some problems in the further analysis - function works interactively describing problems that were identified in the data and asking whether records should be removed or kept.
3.  You may also use `separate_stagnations()` to extract as separate *actions* events describing no actual cursor moves (because of the way Java Script works respondent not moving the cursor is recorded as a long-lasting but short-distanced *mousemove* event by the *logDataLimeSurvey* applet; this function enables to split such an event into two: one describing no move and second describing actual move).
    -   Be aware that while whether you separate *stagnations* or not does not affect values of **most** of the process indicators, it affects average absolute acceleration indices describing cursor moves.
4.  You should use `compute_relative_positions()` if you want to compute relative position that enable better comparability of cursor move indices between respondents with different browser window size. You should also use it before drawing heatmaps aggregating behavior of different respondents.
5.  You may use `compute_cursor_positions()` to transform data describing cursor moves into a format in which cursor positions are reported in regular time spans. Analyzing this kind of data doesn't require weighing by duration what makes it suitable for drawing heatmaps (because functions estimating density generally don't support weighing cases).

### Computing process indicators

Having log-data preprocessed you may compute process indicators using the following functions:

-   `compute_cursor_indices()` to get measures describing cursor moves,
-   `compute_editing()` to get number of response edits,
-   `compute_hovering()` to get hovering indicators,
-   `compute_aat()` and `compute_aht()` to get answering speed indicators.

Specific set of indicators is described in a section *returns* in each function documentation.

## Caution

Support for surveys enabling backward navigation through survey screens is still a bit experimental.

## Funding

Package is developed within a project *Understanding response styles in self-report data: consequences, remedies and sources* that is financed by the Polish National Science Center (NCN) research grant ([2019/33/B/HS6/00937](https://projekty.ncn.gov.pl/index.php?projekt_id=446393)).

![NCN logo](https://rstyles.ifispan.edu.pl/wp-content/uploads/2021/01/xlogo-ncn-en-768x67.png.pagespeed.ic.prFVuamzNv.webp)
