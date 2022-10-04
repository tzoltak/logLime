# logLime 0.1.3 (4.10.2022)

## Bug fixes

-   `compute_relative_positions()` checks whether columns that are supposed to be provided by the `systemInfo` argument exist also in the data frame provided by the `actions` argument (and if they do, it fails quickly).

## Other changes

-   `compute_relative_positions()` doesn't check any more whether data provided by the `actions` argument resembles `actions` element returned by `separate_logdata_types()` - now it requires it to contain only columns `pageX` and `pageY` (apart of those specified by the `screenId` argument).
-   Updats in README file.

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
