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

# Misc
Developer workflow and release management [as described](https://nvie.com/posts/a-successful-git-branching-model/) by Vincent Driessen.

Please check the [LICENSE](LICENSE) file for legal information.

Please get [in touch](http://www.jacktex.eu/about/contact.php) if you would like to contribute to this project.

### Version
v0.0
