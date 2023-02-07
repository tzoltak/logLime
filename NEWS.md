# logLime 0.1.4 (7.02.2023)

## New features

-   `separate_logdata_types()`:
    -   Handles data from the very, very first versions of the *logdataLimeSurvey* applet as well as data that do not contain information on input positions;
    -   Returns additional variable describing problems in the data: `problemsNoActions` that marks respondent-screens for which there are no *actions* at all in the processed log-data;
    -   Have new argument `inputsBoxCells` that enables to compute values that are further used to compute *relative* positions for table-format questions in a different way. In the new approach a box which upper left corner has relative coordinates (0,0) and bottom right corner has relative coordinates (1,1) is spanned not by the positions of inputs themselves but by boundaries (vertexes) of cells storing the inputs. This alternative specification is useful if one wants to analyse data in which different lengths of response scale were used.

## Bug fixes

-   `separate_logdata_types()` detects wider set of problems with time stamps.
-   `compute_hovering()` constructs ids of objects it computes hovering for in a manner that do not cause problems if `target.id` is missing in the data.

# logLime 0.1.3 (4.10.2022)

## Bug fixes

-   `compute_relative_positions()` checks whether columns that are supposed to be provided by the `systemInfo` argument exist also in the data frame provided by the `actions` argument (and if they do, it fails quickly).

## Other changes

-   `compute_relative_positions()` doesn't check any more whether data provided by the `actions` argument resembles `actions` element returned by `separate_logdata_types()` - now it requires it to contain only columns `pageX` and `pageY` (apart of those specified by the `screenId` argument).
-   Updates in README file.

# logLime 0.1.2 (10.06.2022)

## Bug fixes

-   `compute_hovering()` protects itself against failing because of empty *elementType* while question, subquestion or answer code being recognized (i.e. in situations where internal function `label_actions()` failed to recognize type of survey interface element, typically due to some atypical item format was included in the survey).
-   `label_actions()` recognizes *Bootstrap buttons* question format and slightly better labels LI elements.

# logLime 0.1.1 (9.06.2022)

The first usable version of the package.

## New features

-   Functions performing log-data preprocessing:
    -   `separate_logdata_types()`, `remove_problems()`, `separate_stagnations()`.
-   Functions computing process indicators:
    -   `compute_editing()`, `compute_hovering()`, `comput_aat()`, `compute_aht()`, `compute_cursor_indices()`.
-   Auxiliary functions:
    -   `compute_relative_positions()`, `compute_cursor_positions()`, `compute_number_of_items()`.
