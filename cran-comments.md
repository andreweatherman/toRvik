---
editor_options: 
  markdown: 
    wrap: 72
---

# v1.1.1

[**This is an update submission for toRvik.**]{.underline}

Fixed critical error that persisted with a mistake to the
`current_season` function. Wrapped all examples inside `try`.

## Test environments

-   local: Apple Silicon macOS 12.3.0; Darwin 21.4.0; R 4.2.1
-   local: windows-x86_64; R 4.2.1
-   rhub: Debian Linux (R 4.2.1); Oracle Solaris 10, x86, 32 bit (R
    4.2.1)

## R CMD check results:

<https://barttorvik.com/> and <https://linkedin.com/andreweatherman>
**are** valid links. Windows checks will fail them because of how the
sites are designed. This was not a problem in 1.0.0 or 1.1.0 -- but is
the reason why 1.1.1 was continuously rejected by the auto-submission
form.

### macOS:

0 errors \| 0 warnings \| 0 notes

### Windows:

0 errors \| 0 warnings \| 0 notes

### Linux (Debian)

0 errors \| 0 warnings \| 0 notes

Warning: <anonymous>: ... may be used in an incorrect context

gh_data_path: ... may be used in an incorrect context

Warning: <anonymous>: ... may be used in an incorrect context

**`...` is not used in an incorrect context and works as intended and
expected.**

### Solaris:

0 errors \| 0 warnings \| 0 notes

Warning: <anonymous>: ... may be used in an incorrect context

gh_data_path: ... may be used in an incorrect context

Warning: <anonymous>: ... may be used in an incorrect context

**`...` is not used in an incorrect context and works as intended and
expected.**

# v1.1.0

[**This is an update submission for toRvik.**]{.underline}

Re-submission:

-   Added '<https://>' to a url in the README.

Updated many functions to be an API wrapper for a dedicated college
basketball API that I run. The update also address two function errors
that were recently brought to my attention by the CRAN team.

## Test environments

-   local: Apple Silicon macOS 12.3.0; Darwin 21.4.0; R 4.2.1
-   local: windows-x86_64; R 4.2.1
-   rhub: Debian Linux (R 4.2.1); Oracle Solaris 10, x86, 32 bit (R
    4.2.1)

## R CMD check results

### macOS:

0 errors \| 0 warnings \| 0 notes

### Windows:

0 errors \| 0 warnings \| 0 notes

### Linux (Debian)

0 errors \| 0 warnings \| 0 notes

### Solaris:

0 errors \| 0 warnings \| 0 notes

# v1.0.2

## Updated submission

Updated package with recommendations from first CRAN submission. These
include:

-   Omitted 'Functions' from package title
-   Provided links to utilized web services in DESCRIPTION
-   Changed logical variables package-wide from 'T' and 'F' to 'TRUE'
    and 'FALSE'
-   Added `\values{}` field to function documentation

## Test environments

-   local: Apple Silicon macOS 12.3.0; Darwin 21.4.0; R 4.1.0
-   win-builder: windows-x86_64-devel; R 4.2.0
-   rhub: Debian Linux (R 4.1.0); Oracle Solaris 10, x86, 32 bit (R
    4.1.0)

## R CMD check results

### macOS:

0 errors \| 0 warnings \| 1 note

-   This is a new release.

**This is a new submission.**

### Windows:

0 errors \| 0 warnings \| 2 notes

-   Possibly misspelled words in DESCRIPTION: Barttorvik (4:9, 10:5)

**This is the name of the website and not a misspelling.**

-   Found the following (possibly) invalid URLs: URL:
    <https://barttorvik.com/> From: inst/doc/introduction.html README.md
    Status: 403 Message: Forbidden

**This is the correct URL and directs to the website in the README. For
whatever reason, the website denies the default user agent of R on
Windows, so this returns a 403 error. I have spoken with the site owner,
and he confirms that the user agent is actively being denied but does
not know why. I worked with him to resolve this issue for the functions,
and everything in the package works as intended.**

### Linux (Debian)

0 errors \| 0 warnings \| 1 note

-   Possibly misspelled words in DESCRIPTION: Barttorvik (4:9, 10:5)

**This is the name of the website and not a misspelling.**

### Solaris:

0 errors \| 0 warnings \| 1 note

-   This is a new release.

**This is a new submission.**
