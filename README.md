# JTPrettyTime
A small Haskell library for simplified date and time handling.

## Getting started

The following instructions will show you how to incorporate this library into your git project using submodules. For more information about the functionality of submodules check out this [link](https://gist.github.com/gitaarik/8735255).

### Prerequisites/Dependencies

    cabal install time

### Add the library to your project
You have an existing project:

    myproject$ tree -aL 1
    .
    ├── .git
    └── Main.hs

    1 directory, 1 file

To add this library to your `myproject` simply run the following command at the root of your repository (or wherever you keep your source code). *If you have a libraries directory (such as "lib") specifically for your project run the command inside that directory instead.*

    myproject$ git submodule add https://github.com/2079884FDavid/JTPrettyTime.git

To refer to the library from your source code simply use `import JTPrettyTime.[module]`

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

### `JTPrettyTime.Local`
**`getCurDay`** `:: IO Day`<br/>
Gets the current day taking the local timezone of the machine into account.

    *JTPrettyTime.Local> getCurDay
    2018-10-21

**`getCurTimeString`** `:: IO String`<br/>
Gets the local date, time and timezone and returns it in a human readible format.

    *JTPrettyTime.Local> getCurTimeString
    "2018-10-21 18:58:45 [CEST]"

**`getTime`** `:: IO LocalTime`<br/>
Gets the local time of the machine.

    *JTPrettyTime.Local> getTime
    2018-10-21 18:59:11.721698145

### `JTPrettyTime.Util`
**`diffYears`** `:: Day -> Day -> Integer`<br/>
Different in years between two dates. This only looks at the year not at the day or month. That means if one day is in December and the second day is a day in January of the following year `diffYears` will return 1.

    *JTPrettyTime.Util> let d1 = parseIso8601 "2016-12-25" :: Day
    *JTPrettyTime.Util> let d2 = parseIso8601 "2017-01-01" :: Day
    *JTPrettyTime.Util> diffYears d1 d2
    1

**`isAnniversary`** `:: Day -> Day -> Bool`<br/>
Checks if the two dates are following on the same month and day of the month.

    *JTPrettyTime.Util> let d1 = parseIso8601 "2001-09-11" :: Day
    *JTPrettyTime.Util> let d2 = parseIso8601 "2013-09-11" :: Day
    *JTPrettyTime.Util> isAnniversary d1 d2
    True

**`Iso8601`** `= String`<br/>
String in the ISO8601 format (`"1990-10-03"`).

**`parseIso8601`** `:: ParseTime t => Iso8601 -> t`<br/>
Parses an Iso8601 string into any parsable time format (such as `Day`).

    *JTPrettyTime.Util> parseIso8601 "1969-07-16" :: Day
    1969-07-16

# Misc
Developer workflow and release management [as described](https://nvie.com/posts/a-successful-git-branching-model/) by Vincent Driessen.

Please check the [LICENSE](LICENSE) file for legal information.

Please get [in touch](http://www.jacktex.eu/about/contact.php) if you would like to contribute to this project.

### Version
v1.0
