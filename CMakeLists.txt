cmake_minimum_required(VERSION 3.17)

project(iris VERSION 1.0)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
add_library(iris 
	mem_bank.h
	types.h
	opcodes.h
	InstructionFormats.def
	iris.h 
	iris.cpp)
add_library(iris_memcore
	types.h
	mem_bank.h
	iris.h
	register.h
	opcodes.h
	InstructionFormats.def
	backends/memcore/IODevices.h
	backends/memcore/IODevices.cc
	backends/memcore/mem_core.h
	backends/memcore/mem_core.cc
	backends/memcore/exceptions.h
	backends/memcore/exceptions.cc)
add_executable(iris_test 
	cmd/opcode_tester.cc
	iris.h
	types.h
	register.h
	mem_bank.h
	opcodes.h
	InstructionFormats.def
	encoding.h)
add_executable(iris_generate_clp_code cmd/generate_clp_code.cc)
target_link_libraries(iris_test PUBLIC iris iris_memcore)
target_include_directories(iris_memcore PRIVATE .)
target_include_directories(iris_test PRIVATE .)
target_include_directories(iris_generate_clp_code PRIVATE .)

