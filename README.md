# Javax Mode #
Java mode extension for Emacs.
Extends _java-mode_ build source code and run tests with Maven.

## Installation ##

`git clone git@github.com:nchapon/javax-mode.el `

## Set up ##

You need to have installed Java and Maven and setup two environments variables by default in `.bash_profile`
* set `JAVA_HOME` to target Java home directory
* add `mvn` commnand  in your `PATH`

Add this in your emacs configuration
```el
;; Set up ENV variables to have the same as bash
(when (file-exists-p "~/.bash_profile")
  (setenv "JAVA_HOME" (shell-command-to-string "source ~/.bash_profile; echo -n $JAVA_HOME"))
  (setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH")))

(require 'javax-mode)
```

## Usage ##
The only requirement is to have a Java Maven project.
+ `C-c j t` Jump between test and code
+ `C-c j s` Open source file
+ `C-c j o` Organize imports, remove all unused imports
+ `C-c j x t` Run test.
Use mvn command line to run a simgle test `mvn -Dtest=<testcase> test`
+ `C-c j c` Compile source code from mvn command line `mvn -o compile`
+ `C-c j b` Build project from mvn command line `mvn -f <pom-parent> clean install`

## Flycheck

Flycheck can be use to check java syntax on the fly with Eclipse Compiler for Java (ECJ)
ECJ is running in batch mode each time the source code is modified in a Java buffer.
The Java classpath is built from maven command `mvn dependency:tree`

### Installation ###
* First you need to install emacs flycheck package (see instructions [here](https://github.com/flycheck/flycheck#installation))
* Download [ECJ](http://download.eclipse.org/eclipse/downloads/drops4/R-4.3.2-201402211700/download.php?dropFile=ecj-4.3.2.jar)

### Configuration ###
* Configure `jx/mvn-repo-path` with your maven repository :
```el
(setq jx/mvn-repo-path "/home/nchapon/opt/m2_repo")
```

* Configure `jx/ecj-path` to target ECJ executable tool
```el
(setq jx/ecj-path "/home/nchapon/opt/bin/ecj-4.3.2.jar")
```

### Before starting ###
1. You should have a valid Java Maven Project.
2. Open any Java file from this project and type <kbd>jx/update-config</kbd>. This will generate a file *.javax-project.el* used to configure java classpath and ECJ options.


### Usage ###

In any java buffer simply enable flycheck mode, enter <kbd>M-x flycheck-mode</kbd>.

### Known Limitations ###

* At this moment could not work with Lombok



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


## TODO ##

+ Flycheck : create javax project config file if does not exist before running flycheck
+ Flycheck : add hook after save maven config file to update project config
+ Flycheck : customize JVM / ECJ options
+ Inline variables in source code
+ Organize imports : customize imports order
+ Display class method names
+ Extend navigate to source code in dependencies
+ Add a real autocomplete mode
+ Can execute test not only from test case but from java source code under test buffer
+ Flycheck should work with lombok
