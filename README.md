# pants.el

## Description

## Install

Get a copy of the repository:

```sh
git clone git@github.com:franckcuny/pants.el.git
```

Then update your emacs' configuration:

```elisp
(load-file "~/src/pants.el/pants.el")

(use-package pants
  :bind (("C-c b" . pants-find-build-file)
         ("C-c r" . pants-run-binary)
         ("C-c t" . pants-run-test))
  :config
  (progn
    (setq pants-source-tree-root "/Users/fcuny/src/source"
          pants-bury-compilation-buffer t
          pants-extra-args "-q")))
```

## Configuration

There's a few variables that you can set:

* **pants-source-tree-root**: Path to the repository.

* **pants-ini**: Name of the  pants.ini file to use (default is `pants.ini`).

* **pants-exec-name**: Path to the pants executable in the repository (default is `pants`)

* **pants-build-file**: Name of the BUILD file to look for

* **pants-bury-compilation-buffer**: Set to true if you want to bury the compilation buffer after running successfully a command

* **pants-extra-args**: Optional arguments to use with every call to `pants` (for example: "-q")

## Usage

### Go to the closest BUILD file

Do `m-x pants-find-build-file`.

### Run a binary target

Do `m-x pants-run-binary`. It will present a list of targets and let you select which one to run.

### Run a test target

Do `m-x pants-run-test`. It will present a list of targets and let you select which one to run.

### Jump to a REPL

Do `m-x pants-run-python-repl`. It will present a list of targets and create a REPL.
