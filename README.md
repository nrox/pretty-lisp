# pretty-lisp

Programming toy to edit Common Lisp code in the browser, where parenthesis are transformed into "pretty" svg boxes.

## Online demo

[pretty-lisp demo](https://assemblino.com/pretty-lisp/)

Open tutorial.lisp or other *.lisp files available.

## Current state

Tested on Linux with SBCL, probably runs with other implementations. Might run on Mac OS as well. 

The interface uses Chrome browser. It's not working with Firefox.

## Getting started

Usage example with SBCL, quicklisp, Chrome, on Linux Ubuntu

### In one go

        google-chrome "http://localhost:4555" && sbcl --load package.lisp --eval "( pretty-lisp:up :port 4555)"

### Step by step

Get into the repository

	cd pretty-lisp

Load the package

	sbcl --load package.lisp

In SBCL terminal, copy paste this line and press enter

	(pretty-lisp:up :port 4555)

Then use Chrome and navigate to

	http://localhost:4555

## Usage

In folder docs/ open tutorial.lisp or the files concerning a small presentation given at a Lispers meetup in Berlin.
