# Javax Mode #
Java mode extension.
Need Maven.

## Installation ##

`git@github.com:nchapon/javax-mode.el `

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


## TODO ##
