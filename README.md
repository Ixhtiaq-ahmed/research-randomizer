# Clinical Trial Randomization List Generator (web)

Browser interface to a clinical-trial randomization engine: complete randomization
lists by simple randomization or permuted blocks (fixed or variable block sizes),
equal or unequal allocation for 2 to 4 groups, optional stratification, four
selectable randomness sources, descriptive diagnostics, and CSV / XLSX / REDCap /
audit downloads.

This repository is a **deployment bundle**, generated from the development project by
`tools/build_deploy.R`. Edit the sources there, not the files here.

## Run locally

```r
shiny::runApp()
```

## Deploy

See DEPLOYMENT.md. In short: push this repository to GitHub, then publish it from
Posit Connect Cloud, which reads the code directly from the repository.

## Important

A published copy on a free, public plan has no login: anyone with the link can use it.
Generating a randomization list is not the same as allocation concealment. For a trial,
produce the list on infrastructure your institution controls and keep it, with its audit
record, in the custody of someone independent of recruitment.

Nothing is stored on the server: each browser session holds its own design and list in
memory only, and they are discarded when the session ends.
