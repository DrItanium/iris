/*
 * iris
 * Copyright (c) 2013-2018, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "Types.h"
#include "Core.h"
#include "Problem.h"
#include <iostream>
#include <fstream>
#include <list>
#include <type_traits>


void usage(const std::string& name) {
	std::cerr << name << ": <path-to-object> [more objects, -o fileName, --help]" << std::endl;
}
int main(int argc, char** argv) {
	if (argc == 1) {
		usage(argv[0]);
		return 1;
	}
	bool findOutput = false;
	std::string output = "iris.img";
	std::list<std::string> files;
	for (int i = 1; i < argc; ++i) {
		std::string value = argv[i];
		if (findOutput) {
			output = value;
			findOutput = false;
		} else if (value == "-o") {
			findOutput = true;
        } else if (value == "--help") {
            usage(argv[0]);
            return 1;
		} else {
			files.emplace_back(value);
		}
	}
	// TODO: continue this and generate a system image based off of it
	iris::Core core;
	core.init();
	for (auto const & path : files) {
		std::ifstream in(path.c_str(), std::ios::binary);
		auto getByte = [&in]() {
			union {
				char c;
				iris::byte b;
			} tmp;
			tmp.c = in.get();
			return tmp.b;
		};
		auto getAddress = [getByte]() {
			auto lower = iris::Address(getByte());
			auto upper = iris::Address(getByte()) << 8;
			return lower | upper;
		};
		while (in) {
			// read each entry to see what to do with it
			auto section = getAddress();
            if (in.gcount() == 0) {
                break;
            }
			auto address = getAddress();
			auto value = getAddress();
			switch (section) {
				case 0: // register
					core.install( address, value, iris::Core::InstallToRegister());
					break;
				case 1: // memory
					core.install( address, value, iris::Core::InstallToMemory());
					break;
				case 2: // core
					core.install( address, value, iris::Core::InstallToCore());
					break;
				default:
					std::cerr << "Got an illegal section, terminating..." << std::endl;
					std::cerr << "\tThe culprint file is: " << path << std::endl;
					return 1;
			}
		}
		if (in.bad()) {
			std::cerr << "A really bad error occurred while installing section entries, terminating..." << std::endl;
			std::cerr << "\tThe culprit file is: " << path << std::endl;
			return 1;
		}
		in.close();
	}
	core.shutdown();
	if (!output.empty()) {
		std::ofstream file(output.c_str(), std::ios::binary);
		if (!file.is_open()) {
			std::cerr << "could not open: " << output << " for writing!" << std::endl;
			return 1;
		} else {
			core.dump(file);
		}
		file.close();
	}
	return 0;
}
