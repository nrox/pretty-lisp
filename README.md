# pretty-lisp

Programming toy to edit Common Lisp code in the browser, where parenthesis are transformed into "pretty" svg boxes.

## Current state

Tested on Linux with SBCL, probably runs with other implementations. Might run on Mac OS as well. 

The interface uses Chrome browser. It's not working with Firefox.

Some features are broken.

## Getting started

Usage example with SBCL, quicklisp on Linux Ubuntu

Get into the repository

	cd pretty-lisp

Load the package

	sbcl --load package.lisp

In SBCL terminal, copy paste this line and press enter

	(pretty-lisp:run :port 4555)

Then use Chrome and navigate to

	http://localhost:4555

## Usage

_TODO_ 
