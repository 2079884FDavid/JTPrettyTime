# JTPrettyTime

A small Haskell library for simplified date and time handling.

## Getting started

The following instructions will show you how to incorporate this library into
your git project using submodules. For more information about the functionality
of submodules check out this [link](https://gist.github.com/gitaarik/8735255).

### Prerequisites/Dependencies

    cabal install time
    cabal install unix-time

### Add the library to your project

You have an existing project:

    myproject$ tree -aL 1
    .
    ├── .git
    └── Main.hs

    1 directory, 1 file

To add this library to your `myproject` simply run the following command at the
root of your repository (or wherever you keep your source code). *If you have a
libraries directory (such as "lib") specifically for your project run the
command inside that directory instead.*

    myproject$ git submodule add https://github.com/2079884FDavid/JTPrettyTime.git

To refer to the library from your source code simply use `import
JTPrettyTime.[module]`

### Update to latest library release

    cd JTPrettyTime
    git pull
    cd ..
    git add JTPrettyTime
    git commit -m "Updated to latest JTPrettyTime release"

### Checkout your repository

If your repository contains this library as a submodule run the following:

    git checkout https://myproject
    git submodule update --init


# Usage Examples

For implementation examples, see `test/example[0-9]*.hs`.

### `JTPrettyTime.Local`

**`getCurDay`** `:: IO Day`

Gets the current day taking the local timezone of the machine into account.

    *JTPrettyTime.Local> getCurDay
    2018-10-21

**`getCurTimeString`** `:: IO String`

Gets the local date, time and timezone and returns it in a human readible
format.

    *JTPrettyTime.Local> getCurTimeString
    "2018-10-21 18:58:45 [CEST]"

**`getTime`** `:: IO LocalTime`

Gets the local time of the machine.

    *JTPrettyTime.Local> getTime
    2018-10-21 18:59:11.721698145

### `JTPrettyTime.Util`

**`diffYears`** `:: Day -> Day -> Integer`

Different in years between two dates. This only looks at the year not at the
day or month. That means if one day is in December and the second day is a day
in January of the following year `diffYears` will return 1.

    *JTPrettyTime.Util> let d1 = parseIso8601 "2016-12-25" :: Day
    *JTPrettyTime.Util> let d2 = parseIso8601 "2017-01-01" :: Day
    *JTPrettyTime.Util> diffYears d1 d2
    1

**`isAnniversary`** `:: Day -> Day -> Bool`

Checks if the two dates are following on the same month and day of the month.

    *JTPrettyTime.Util> let d1 = parseIso8601 "2001-09-11" :: Day
    *JTPrettyTime.Util> let d2 = parseIso8601 "2013-09-11" :: Day
    *JTPrettyTime.Util> isAnniversary d1 d2
    True

### `JTPrettyTime.Parsec.ParseIso8601`

**`parseIso8601`** `:: String -> Either String Int`

Parses a string which contains a date and time in the ISO8601 format. The 'T'
delimiter between date and time is optional. '1970-01-01T00:00+00:00' is the
assumed default. For instance, if the string is equivalent to "2019-09", the
implied time is "2019-09-01T00:00+00:00". The parser parses from left to right
as far as possible. Hence, the string can contain trailing characters. Note
however, that this can lead to surprising results. In the example below the
timezone 'CET' is silently ignored (because according to the ISO8601 standard,
timezones have to be written as absolute values with the exception of 'Z'). If
you want to ensure that the entire string is parsed, use parseIso8601Strict
instead. Returns (Right Unix-Timestamp) or, if there was a parsing error, (Left
Error-Message).

    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601 "2019-08-01 18:30-06:00"
    Right 1564705800
    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601 "2019-08-01 18:30 CET"
    Right 1564684200

**`parseIso8601Strict`** `:: String -> Either String Int`

Equivalent to parseIso8601 but the entire string needs to be parsable.

    parseIso8601Strict "2019-08-01 18:30-06:00"
    Right 1564705800
    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601Strict "2019-08-01 18:30 CET"
    Left "\"Failed to parse ISO8601 from string \"2019-08-01 18:30 CET\"\" (line 1, column 17):\nunexpected ' '\nexpecting \":\", \"Z\", \"+\", \"-\" or end of input"

# Testing

All tests for this library are saved in `test/[module]Test.hs` and are combined
in `test/Tests.hs`. The `test/` directory is solely for testing this library.

### Test dependencies

All tests are written as [HUnit](http://hackage.haskell.org/package/HUnit) or
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) tests and are
implemented using the
[test-framework](http://batterseapower.github.com/test-framework/) package. All
dependencies can be installed with cabal:

    $ cabal install test-framework
    $ cabal install test-framework-quickcheck2
    $ cabal install test-framework-hunit

### Run tests

To run the test suite use:

    JTHTML$ cd test/
    test$ ./run_tests.sh

Test results are shown on `stdout` and are additionally saved in
`test/test-results.xml`, a JUnit-compatible XML file, which can be parsed by
[Jenkins](https://jenkins.io/) or similar.

# Style Guide

All source lines of this library should be at most 70 characters long (which
can be checked with grep). Moreover,
[`hlint`](http://community.haskell.org/~ndm/darcs/hlint/hlint.htm) should be
used to assure high quality of the code. Both of this can be done as show
below.

    JTHTML$ cabal update
    JTHTML$ cabal install hlint
    JTHTML$
    JTHTML$ hlint .
    JTHTML$ grep -r --include \*.hs -n '.\{70\}' .

# Misc

Developer workflow and release management [as
described](https://nvie.com/posts/a-successful-git-branching-model/) by Vincent
Driessen.

Please check the [LICENSE](LICENSE) file for legal information.

Please get [in touch](http://www.jacktex.eu/about/contact.php) if you would
like to contribute to this project.

### Version

v1.0
