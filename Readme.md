NSL
===

(original author: Matthäus G. Chajdas)

`nsl` is a toy programming language to explore various compilation techniques. It's meant to be easy to hack on, first and foremost, which is why it's written in Python, and makes extensive use of passes and visitors for processing.

License
-------

All of this is provided under the BSD license. See the `COPYING` file for details.

Requirements
------------

All you need is a working Python 3 installation. `nsl` has been tested with Python 3.4. In a virtual environment, install the dependencies as specified in  `requirements.txt`.

Running
-------

To run the tests, simply invoke `./run_tests.sh` from the main directory.

For any other code, generate the standard library by calling `nslc.py nsl/stdlib.nsl -o std.nslir` first, then:

* `nslc.py your-test-file.nsl -o your-test-file.nslir` to compile a file
* `nslr.py run your-test-file.nslir <function name> <arg0> <argN>` to run

To use WebAssembly, specify `--wasm` while compiling as well as running, and use `.wasm` as the file ending.

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
