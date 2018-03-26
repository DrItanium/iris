#!/bin/bash

g++ -std=c++17 -MM -Imisc/pegtl *.cc > deps.make
