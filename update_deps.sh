#!/bin/bash

make
g++ -std=c++14 -MM -Imisc/pegtl *.cc > deps.make
