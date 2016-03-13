# pretty-lisp

Programming toy to edit Common Lisp code in the browser, where parenthesis are transformed into "pretty" svg boxes.

Discussion on [hacker news](https://news.ycombinator.com/item?id=3649518).

## Current state

Backend tested on Linux with SBCL, probably runs with other implementations. Might run on Mac OS as well. Frontend
was tested with last versions (March 2015) of Firefox, Chrome and Safari. 

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
