
# Contributing

When contributing to this repository, please first discuss the change you wish
to make via issue, email, or any other method with the owners of this repository
before making a change.

Please note we have a code of conduct, please follow it in all your interactions
with the project.

## Submitting changes and raising issues

We aim to respond to all comments on issues, and keep update my development
plans there. Offers of help are always welcome.

If you are reporting an issue please could you try and reproduce it using one of
the data sets we've used in the vignettes (e.g. iris, diamonds, mtcars,
survival::cgd ) or include the data set in your issue.

If you can also tell us exactly what text you expect to see in the flowchart
that isn't there, that would be helpful.

Please fork the code and send a [GitHub Pull Request](https://github.com/terminological/dtrackr/new/master) with a clear list
of what you've done (read more about [pull requests](http://help.github.com/pull-requests/)).

## Testing

We'd love some help with better test cases.

For testing `dtrackr` uses the `testthat` framework. It is configured to run both
the unit tests and the functional tests in the code examples.

```R
# assuming dtrackr has been cloned from github into the working directory 
# location

devtools::load_all()

# Long list of system dependencies in Ubuntu 20.04 including all suggested 
# dependencies:
# librsvg2-dev libicu-dev libcurl4-openssl-dev libssl-dev libnode-dev make 
# pandoc imagemagick libmagick++-dev gsfonts default-jdk libxml2-dev 
# zlib1g-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev 
# libjpeg-dev libpng-dev libtiff-dev git libgit2-dev

pak::local_system_requirements("ubuntu","20.04")
install.packages(c("here","tidyverse","devtools","testthat","pkgdown"))

# Examples:
devtools::run_examples()

# automated testing with testthat (also runs all examples):
devtools::test()

# pkgdown site building (which executes all the vignettes):
pkgdown::build_site()
```

Github workflows are enabled on this repository for continuous integration. 
These perform an `R CMD check` on code commits, which will run all `testthat`
unit tests, run all man page examples, and build all the vignettes.

For vignette building there are dependencies on the `tidyverse` package, 
as well as other system libraries required for vignette building with `pandoc`.
The CI tests check the library can be installed on macOs, windows, and Ubuntu, 
with R versions 3.6.1, 4.1, 4.2 and the R development branch.

On tagged releases, additional Github workflows are triggered by in the 
`r-universe` repository, to build binary releases on a range of platforms.

## Code of Conduct

### Our Pledge

In the interest of fostering an open and welcoming environment, we as
contributors and maintainers pledge to making participation in our project and
our community a harassment-free experience for everyone, regardless of age, body
size, disability, ethnicity, gender identity and expression, level of experience,
nationality, personal appearance, race, religion, or sexual identity and
orientation.

### Our Standards

Examples of behaviour that contributes to creating a positive environment
include:

* Using welcoming and inclusive language
* Being respectful of differing viewpoints and experiences
* Gracefully accepting constructive criticism
* Focusing on what is best for the community
* Showing empathy towards other community members

Examples of unacceptable behavior by participants include:

* The use of sexualized language or imagery and unwelcome sexual attention or
advances
* Trolling, insulting/derogatory comments, and personal or political attacks
* Public or private harassment
* Publishing others' private information, such as a physical or electronic
  address, without explicit permission
* Other conduct which could reasonably be considered inappropriate in a
  professional setting

### Our Responsibilities

Project maintainers are responsible for clarifying the standards of acceptable
behavior and are expected to take appropriate and fair corrective action in
response to any instances of unacceptable behavior.

Project maintainers have the right and responsibility to remove, edit, or
reject comments, commits, code, wiki edits, issues, and other contributions
that are not aligned to this Code of Conduct, or to ban temporarily or
permanently any contributor for other behaviors that they deem inappropriate,
threatening, offensive, or harmful.

### Scope

This Code of Conduct applies both within project spaces and in public spaces
when an individual is representing the project or its community. Examples of
representing a project or community include using an official project e-mail
address, posting via an official social media account, or acting as an appointed
representative at an online or offline event. Representation of a project may be
further defined and clarified by project maintainers.

### Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be
reported by contacting the project team at rob@terminological.co.uk. All
complaints will be reviewed and investigated and will result in a response that
is deemed necessary and appropriate to the circumstances. The project team is
obligated to maintain confidentiality with regard to the reporter of an incident.
Further details of specific enforcement policies may be posted separately.

Project maintainers who do not follow or enforce the Code of Conduct in good
faith may face temporary or permanent repercussions as determined by other
members of the project's leadership.

### Attribution

This Code of Conduct is adapted from the [Contributor Covenant][homepage], version 1.4,
available at [http://contributor-covenant.org/version/1/4][version]

[homepage]: http://contributor-covenant.org
[version]: http://contributor-covenant.org/version/1/4/

