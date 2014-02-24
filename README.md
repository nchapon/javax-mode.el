# Javax Mode #
Java mode extension for Emacs.
Extends _java-mode_ build source code and run tests with Maven.

## Installation ##

`git clone git@github.com:nchapon/javax-mode.el `

## Set up ##

You need to have installed Java and Maven and setup two environments variables by default in `.bash_profile`
* set `JAVA_HOME` to target Java home directory
* add `mvn` commnand  `PATH`

Add this in your emacs configuration
```cl
;; Set up ENV variables to have the same as bash
(when (file-exists-p "~/.bash_profile")
  (setenv "JAVA_HOME" (shell-command-to-string "source ~/.bash_profile; echo -n $JAVA_HOME"))
  (setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH")))

(require 'javax-mode)
```

## Usage ##
+ `C-c C-t` Jump between test and code
+ `C-c C-s` Open source file
+ `C-c C-o` Organize imports, remove all unused imports
+ `C-c C-r` Run test.
Use mvn command line to run a simgle test `mvn -Dtest=<testcase> test`
+ `C-c C-k` Compile source code from mvn command line `mvn -o compile`
+ `C-c C-b` Build project from mvn command line `mvn -f <pom-parent> clean install`

## Flycheck

TODO

## TODO ##
+ Inline variables
+ Remove or display unused / variable
+ Display class method names
+ Check syntax with Flycheck (in progress)
+ ...

## Development ##

### Source ###

You will find the source code at :
    https://github.com/nchapon/javax-mode.el


### Running tests ###

Tests are written with [Ecukes](https://github.com/ecukes/ecukes).
This library is used to write [Cucumber](http://cukes.info/)-like tests for Emacs.

+ Install [Cask](https://github.com/cask/cask)
+ Run cask to install all dependencies :
    `$ cd /path/to/javax-mode`
    `$ cask`
+ Run all the tests
    `$ cask exec ecukes --no-win`
