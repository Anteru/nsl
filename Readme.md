NSL
===

(original author: Matthäus G. Chajdas)

`nsl` is a toy programming language to explore various compilation techniques. It's meant to be easy to hack on, first and foremost, which is why it's written in Python, and makes extensive use of passes and visitors for processing.

License
-------

All of this is provided under the BSD license. See the `COPYING` file for details.

Requirements
------------

All you need is a working Python 3 installation. `nsl` has been tested with Python 3.4. In a virtualenv, install the dependencies as specified in  `requirements.txt`.

Running
-------

To run the tests, simply invoke `./run_tests.sh` from the main directory. To see it compile, use `nsl.py`, which parses a small example shader and pretty prints it back to the console after all compiler passes have been run.

Status
------

What's working?

* A lexer/parser built on [PLY](http://www.dabeaz.com/ply/)
* An abstract syntax tree is generated
* A type computation/legalization pass. This pass resolves overloaded functions, operators, and computes all types.
* Pretty printing
* Various debug outputs
* A minimal standard library

Contributing
------------

If you want to contribute, please fork the source and contact me so I know you're there and you want to work on this.
