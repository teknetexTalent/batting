DISCLAIMER: I am no baseball fan nor have I ever learned anything substantial
about baseball.  All I know is that it seems to be fun for players
and spectators alike! :)

This is a step-by-step approach to solve one of the requested solutions for
the developer test.

There is not enough free time for me to work on all three,
but I hope this gives you an idea on how I work and how I structure my work.

I could have used a SQL database and ran a few queries but I thought it would
be more fun to do this all in a programming language - no database required.

I chose Erlang for some reasons:
  * Even complex code can be readable
  * It's short enough to be a script, module or anything else in the future
  * It shows some very powerful list manipulation methods
  * It's not OOP, basically, what you see is what you get. No trap doors.

The easiest way to run it is to install the compiler from

  https://www.erlang-solutions.com/downloads/download-erlang-otp

choose your platform and install it.  It only takes a few minutes
to download and install.

Then drop into a shell and execute the provided shell script:

  ./startme.sh

This little script will start the Erlang Virtual Machine and start the
Erlang program.

A note on the datafiles:
There were errors.  The files that I am providing with this exercise have
been fixed.  There were not too many errors in these files, thus I fixed
them in the files themselves to keep the program sources short and sweet.

Another approach could have been to leave the files as they were and take
care of the different "patterns" of errors inside the source code.

One of Erlang's strengths is to work with pattern matching and it can put to
excellent use in CSV file processing for instance.  Pattern matching is not
regular expressions, there is an extra library (part of Erlang's core) for
just that.
