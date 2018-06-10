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
#include <map>
namespace iris {
    /* 
     * uses plain text for all purposes
     * one byte for the section ( up to 255 sections can be defined!)
     * one byte is ignored ( must be zero )
     * 16-bits define an address
     * 32-bits define the value ( this will be automatically truncated if the
     *   need arises)
     */
    class LinkerEntry {
        public:
            using RawLinkerEntry = uint64_t; // each entry is at most 64 bits wide
            enum class Kind : iris::byte {
                RegisterValue,
                MemorySpace,
                Instruction,
                LabelEntry,
                IndirectInstruction,
                IndirectMemory,
            };
            LinkerEntry(Kind kind, Address addr, RawInstruction val) noexcept : _kind(kind), _address(addr), _value(val) { };
            LinkerEntry(RawLinkerEntry entry) noexcept;
            Kind getKind() const noexcept { return _kind; }
            Address getAddress() const noexcept { return _address; }
            RawInstruction getValue() const noexcept { return _value; }
            void install(iris::Core& c);
        private:
            Kind _kind;
            Address _address;
            RawInstruction _value;
    };

    LinkerEntry::LinkerEntry(LinkerEntry::RawLinkerEntry entry) noexcept 
        : _kind(LinkerEntry::Kind(entry & 0xFF)),
        _address(decodeBits<decltype(entry), Address, 0x00000000FFFF0000, 16>(entry)),
        _value(decodeBits<decltype(entry), RawInstruction, 0xFFFFFFFF00000000, 32>(entry)) { }


} // end namespace iris


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
    // read in plain text instead of a binary encoding, makes things much easier
    // to parse. Especially since forth doesn't use binary encodings and the
    // design of it strongly discourages it. So let's be super simple and do it
    // using plain text instead.
    using Address = iris::Address;
    using LabelIndex = iris::RawInstruction;
    using Kind = iris::LinkerEntry::Kind;
    for (auto const & path : files) {
        std::map<LabelIndex, Address> _labelMappings;
        std::list<iris::LinkerEntry> _delayedInstructions;
        std::ifstream in(path.c_str());
        while (in) {
            iris::LinkerEntry::RawLinkerEntry v = 0;
            in >> std::hex >> v;
            iris::LinkerEntry entry(v);
            switch (entry.getKind()) {
                case Kind::RegisterValue:
                    core.install(entry.getAddress(), Address(entry.getValue()), iris::Core::InstallToRegister());
                    break;
                case Kind::MemorySpace:
                    core.install(entry.getAddress(), Address(entry.getValue()), iris::Core::InstallToMemory());
                    break;
                case Kind::Instruction:
                    // only 32kb worth of instructions allowed
                    core.install(entry.getAddress(), Address(entry.getValue()), iris::Core::InstallToMemory());
                    core.install(entry.getAddress() + 1, Address(entry.getValue() >> 16), iris::Core::InstallToMemory());
                    break;
                case Kind::LabelEntry:
                     _labelMappings.emplace(entry.getValue(), entry.getAddress());
                     break;
                case Kind::IndirectInstruction:
                case Kind::IndirectMemory:
                     // resolve these later
                     _delayedInstructions.emplace_back(entry);
                     break;
                default:
					 std::cerr << "Illegal linker entry kind defined!" << std::endl;
					 return 1;
            }
        }
        if (in.bad()) {
            std::cerr << "A really bad error occurred while installing section entries, terminating..." << std::endl;
            std::cerr << "\tThe culprit file is: " << path << std::endl;
            return 1;
        }
        in.close();
        for (auto& e : _delayedInstructions) {
            if (auto kind = e.getKind() ; kind == Kind::IndirectInstruction) {
                auto upperHalf = (e.getValue() >> 16) & 0xFFFF;
                if (auto r = _labelMappings.find(upperHalf); r != _labelMappings.end()) {
                    auto address = e.getAddress();
                    auto lowerHalf = Address(e.getValue());
                    core.install(address, lowerHalf, iris::Core::InstallToMemory());
                    core.install(address + 1, r->second, iris::Core::InstallToMemory());
                } else {
					std::cerr << "Not all labels are defined for given indirect instructions!" << std::endl;
					std::cerr << "\t\t target index is: " << upperHalf << std::endl;
					return 1;
                }
            } else if (kind == Kind::IndirectMemory) {
                if (auto r = _labelMappings.find(e.getValue()); r != _labelMappings.end()) {
                    core.install(e.getAddress(), r->second, iris::Core::InstallToMemory());
                } else {
					std::cerr << "Not all labels are defined for the given indirect memory!" << std::endl;
					return 1;
                }
            } else {
				std::cerr << "Unsupported linker entry kind in the delay list!" << std::endl;
				return 1;
            }
        }
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
