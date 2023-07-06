# logLime 0.3.0 (5.07.2023)

## New features

-   The package supports data from surveys in which respondents can navigate back through survey screens. However, the feature is still experimental as it has not undergone extensive testing.
    - `separate_logdata_types()` by default identifies different entries on the survey screen, but this may be switched of using parameter `separateReturns`. See `?separate_returns` for a description of the caveats regarding the identification of navigating back through the survey.
    - Different entries on the survey screen are identified by an additional column *entry*.
    - The APIs has changed a bit for most of `compute_...` functions to enable returning results for separate *entries*. Please note that the default behavior (whether to report results for separate *entries* or for the whole survey screens) depends on the specific function.

## Bug fixes

-   Fixed a regression introduced in 0.1.4 making `preprocess_system_info()` (and consequently `separate_logdata_types()`) fail if no survey structure was provided.

# logLime 0.2.0 (7.02.2023)

## New features

-   `separate_logdata_types()`:
    -   Handles data from the very, very first versions of the *logdataLimeSurvey* applet as well as data that do not contain information on input positions;
    -   Returns additional variable describing problems in the data: `problemsNoActions` that marks respondent-screens for which there are no *actions* at all in the processed log-data;
    -   Have new argument `inputsBoxCells` that enables to compute values that are further used to compute *relative* positions for table-format questions in a different way. In the new approach a box which upper left corner has relative coordinates (0,0) and bottom right corner has relative coordinates (1,1) is spanned not by the positions of inputs themselves but by boundaries (vertexes) of cells storing the inputs. This alternative specification is useful if one wants to analyze data in which different lengths of response scale were used.

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
