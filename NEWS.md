# 1.1.0

- switch to Github Actions for CI
- units package now supported (#51)
- mutate: fix formatting issues for NAs (#48)
- use rlang::inform instead of message (#41)
- include dataset names in join messages (#46)
- mutate now reports dropped columns (#53)
- filter reports the number of groups (#52)
- fix bug with new dplyr::join_by syntax (#58)
- tidyr: support for separate_wider_* functions (#62)

# 1.0.2

- add a short benchmarking vignette
- report type for new variables (#45)
- add relocate, rename_with, slice_* from dplyr 1.0.0

# 1.0.1

- autogenerate documentation
- data argument now has same name as in tidyr/dplyr (#43)
- fixes #42
- fixes "ungroup" bug in tests (found by revdep check for dplyr 1.0.0)

# 1.0.0

- tidyr: support for pivot_longer, pivot_wider
- dplyr: support for ungroup (thanks @damianooldoni)
- dplyr: support for rename_* (#27)
- dplyr: support for top_frac, sample_n, sample_frac, slice
- tidyr: support for uncount

# 0.2.0
- added detailed merge information for joins (#25)
- added support for tidyr functions: gather, spread (thanks @WilDoane), drop_na (@jackhannah95), fill and replace_na
- use clisymbols for ellipsis
- add number of remaining rows to filter (#23)
- bugfix: do not report negative NA (#18)
- bugfix: avoid partial matching (closes #26)

# 0.1.0

initial release
