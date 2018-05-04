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

#include "Core.h"
#include <iostream>
#include <fstream>


void usage(const std::string& name) {
	std::cerr << name << ": path-to-installation-image [output-image-path]" << std::endl;
}
int main(int argc, char** argv) {
	int exitCode = 0;
	std::string in, out;
	switch (argc) {
		case 3:
			out = argv[2];
		case 2:
			in = argv[1];
			break;
		default:
			usage(argv[0]);
			return 1;
	}
	iris::Core core;
	core.init();
	std::ifstream input(in.c_str(), std::ios::binary);
	if (input.is_open()) {
		core.install(input);
		core.execute();
		core.shutdown();
		if (!out.empty()) {
			std::ofstream file(out.c_str(), std::ios::binary);
			if (!file.is_open()) {
				std::cerr << "could not open: " << out << " for writing!" << std::endl;
				exitCode = 1;
			} else {
				core.dump(file);
			}
			file.close();
		}
	} else {
		std::cerr << "Could not open: " << in << " for reading!" << std::endl;
		exitCode = 1;
	}
	input.close();
	return exitCode;
}
