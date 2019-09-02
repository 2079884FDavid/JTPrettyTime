# JTPrettyTime

A small Haskell library for simplified date and time handling.

If you just want some time arithmetics in your program and don't want to worry
about data types, this library is for you. Time is represented as either an
Integer (unix-timestamp) or a String (ISO8601). However, if you are a strong
believer in the Haskell philosophy and love types you will probably not like
this library.

## Getting started

The following instructions will show you how to incorporate this library into
your git project using submodules. For more information about the functionality
of submodules check out this [link](https://gist.github.com/gitaarik/8735255).

### Prerequisites/Dependencies

    cabal install parsec
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

Listed below are only the most relevant functions to help you getting started.
If you are trying to do more complicated things, there are more functions for
you available. In that case, please read the source code or use Haskell's time
library directly.

---
*NOTE*

This library uses the `TimeZone` data type from the standard Haskell library,
because it is the best possible implementation for a timezone. You can create
`TimeZone` objects on the fly such as:

    read "+02:30"

To get the current local timezone use:

    import Data.Time.LocalTime

    getCurrentTimeZone :: IO TimeZone  -- CEST

---

### `JTPrettyTime.Convert.*`

Contains a number of functions to incorporate Haskell's time library.

### `JTPrettyTime.Fetch`

**`getCurrentUtcIso8601`** `:: IO String`

Get the current time as an ISO8601 string for UTC.

    *JTPrettyTime.Fetch> getCurrentUtcIso8601
    "2019-08-27T19:42:46+00:00"

**`getCurrentSpecificIso8601`** `:: TimeZone -> IO String`

Get the current time as an ISO8601 string for the given timezone.

    *JTPrettyTime.Fetch> getCurrentSpecificIso8601 (read "+06:00")
    "2019-08-28T01:44:24+06:00"

**`getCurrentLocalIso8601`** `:: IO String`

Get the current time as an ISO8601 string for the local timezone.

    *JTPrettyTime.Fetch> getCurrentLocalIso8601
    "2019-08-27T21:45:41+02:00"

**`getCurrentUnixTime`** `:: IO Integer`

Get the current unix timestamp.

    *JTPrettyTime.Fetch> getCurrentUnixTime
    1566935167

### `JTPrettyTime.Format`

**`unixToUtcIso8601`** `:: Integer -> String`

Turn the given unix timestamp to an ISO8601 string for UTC.

    *JTPrettyTime.Format> unixToUtcIso8601 1234567890
    "2009-02-13T23:31:30+00:00"

**`unixToSpecificIso8601`** `:: TimeZone -> Integer -> String`

Turn the given unix timestamp to an ISO8601 string for the specified timezone.

    *JTPrettyTime.Format> unixToSpecificIso8601 (read "-09:00") 1234567890
    "2009-02-13T14:31:30-09:00"

**`unixToLocalIso8601`** `:: Integer -> IO String`

Turn the given unix timestamp to an ISO8601 string for the local timezone.

    *JTPrettyTime.Format> unixToLocalIso8601 1234567890
    "2009-02-14T01:31:30+02:00"

### `JTPrettyTime.Parsec.ParseIso8601`

**`parseIso8601`** `:: String -> Either String Integer`

Parses a string which contains a date and time in the ISO8601 format. The 'T'
delimiter between date and time is optional. '1970-01-01T00:00+00:00' is the
assumed default. For instance, if the string is equivalent to "2019-09", the
implied time is "2019-09-01T00:00+00:00". The parser parses from left to right
as far as possible. Hence, the string can contain trailing characters. Note
however, that this can lead to surprising results. In the example below the
timezone 'CET' is silently ignored (because according to the ISO8601 standard,
timezones have to be written as absolute values with the exception of 'Z'). If
you want to ensure that the entire string is parsed, use `parseIso8601Strict`
instead. Returns (Right Unix-Timestamp) or, if there was a parsing error, (Left
Error-Message).

    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601 "2019-08-01 18:30-06:00"
    Right 1564705800
    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601 "2019-08-01 18:30 CET"
    Right 1564684200

**`parseIso8601Strict`** `:: String -> Either String Integer`

Equivalent to parseIso8601 but the entire string needs to be parsable.

    parseIso8601Strict "2019-08-01 18:30-06:00"
    Right 1564705800
    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601Strict "2019-08-01 18:30 CET"
    Left "\"Failed to parse ISO8601 from string \"2019-08-01 18:30 CET\"\" (line 1, column 17):\nunexpected ' '\nexpecting \":\", \"Z\", \"+\", \"-\" or end of input"

**`parseIso8601Forced`** `:: String -> Integer`

Same as `parseIso8601` but always returns an Integer. Use this if you are sure
that the input cannot be malformed.  **WARNING:** There is no safety net here.
If you provide wrong input, your program will go into an undefined state
(returns unix-timestamp=0) and things will go badly. **DO NOT TRUST USER
INPUT!**

    *JTPrettyTime.Parsec.ParseIso8601> parseIso8601Forced "2001-09-07"
    999820800

**`isIso8601StrictParseable`** `:: String -> Bool`

Checks if running `parseIso8601Strict` on the provided input would work.

    *JTPrettyTime.Parsec.ParseIso8601> isIso8601StrictParseable "2019-09-01"
    True
    *JTPrettyTime.Parsec.ParseIso8601> isIso8601StrictParseable "2019-09-01Tango"
    False

### `JTPrettyTime.Util`

For all functions in `Util.hs`, the given timestamp (last argument) is
interpreted to be in the UTC timezone. The reference timezone and timestamp
(first two arguments) are typically the local time of the machine.  The idea
behind this: the given timestamp comes most likely from a string such as
"2012-05-03" (note the missing timezone). All of the functions are also
available as `IO` functions which automatically fetch the current timestamp and
timezone.

**`unixDiffYears`** `:: TimeZone -> Integer -> Integer -> Integer`

Years between the timestamps. Note that this is simply the distance between the
years. For example, "2018-12-31" is one year apart from "2019-01-01" (though it
is only one day apart). Think of it as "last year" and "two years ago".

    *JTPrettyTime.Util> unixDiffYears (read "+00:00") 987654321 123456789
    28
    *JTPrettyTime.Util> unixDiffYears (read "+00:00") 123456789 987654321
    -28

**`unixDiffLocalYears`** `:: Integer -> IO Integer`

Same as `unixDiffYears` but automatically use the local timezone and current
timestamp as the reference date.

    *JTPrettyTime.Util> unixDiffLocalYears 123456789
    46

**`unixDiffDays`** `:: TimeZone -> Integer -> Integer -> Integer`

Days between the two timestamps.

    *JTPrettyTime.Util> unixDiffDays (read "+00:00") 654321 123456
    6
    *JTPrettyTime.Util> unixDiffDays (read "+00:00") 123456 654321
    -6

**`unixDiffLocalDays`** `:: Integer -> IO Integer`

Same as `unixDiffDays` but automatically use the local timezone and current
timestamp as the reference date.

    *JTPrettyTime.Util> unixDiffLocalDays 654321
    18133

**`isAnniversary`** `:: TimeZone -> Integer -> Integer -> Bool`

Are the two dates anniversaries (same month, same day)?

    *JTPrettyTime.Util> isAnniversary (read "-06:00") 1554961500 576633600
    True

**`isLocalAnniversary`** `:: Integer -> IO Bool`

Same as `isAnniversary` but automatically use the local timezone and current
timestamp as the reference date.

    *JTPrettyTime.Util> isLocalAnniversary 123456789
    False

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

v2.1
