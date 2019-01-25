## promptr 1.0.1

changes:
  - modified prompts to work under WINDOWS
  - use `cat()` instead of `paste0()` to print the prompts, as otherwise the 
    `crayon` colors are not displayed correctly
  - `prompt_fancy` now only showing ok-status, memuse and git status
  - current prompts: `prompt_memuse`, `prompt_git`, `prompt_fancy`


## prompt 1.0.0

First public release.
