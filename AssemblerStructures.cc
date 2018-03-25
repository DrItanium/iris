/**
 * @file
 * Implementation of iris assembler structures and related functions
 * @copyright
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
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


// IrisCoreAssembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
//#include "Base.h"
#include "Assembler.h"
#include "Problem.h"
//#include "IrisCore.h"
//#include "IrisClipsExtensions.h"
//#include "ClipsExtensions.h"
//#include "IrisCoreAssemblerStructures.h"
//#include "IrisCoreEncodingOperations.h"
#include "AssemblerStructures.h"

namespace iris {
	AssemblerState::AssemblerState(Address c, Address d, Address s) : _code(c), _data(d), _stack(s) { }
	AssemblerState::~AssemblerState() { }
	void AssemblerState::addData(EvaluationFunction fn, EvaluationStyle style) {
		std::visit([this, fn](auto&& value) {
					using T = std::decay_t<decltype(value)>;
					if constexpr (std::is_same_v<T, AssemblerState::NormalEvaluation>) {
						fn(*this);
					} else if constexpr (std::is_same_v<T, AssemblerState::DeferEvaluation>) {
						_evalLater.emplace_back(fn);
					} else {
						static_assert(AlwaysFalse<T>::value, "Unimplemented type!");
					}
				}, style);
	}
	void AssemblerState::addData(Data32 data, Section section) {
		std::visit([data, this](auto&& value) {
					using T = std::decay_t<decltype(value)>;
					if constexpr (std::is_same_v<T, CodeSection>) {
						_code.addData(data);
					} else if constexpr (std::is_same_v<T, DataSection>) {
						throw Problem("Cannot put a data32 into the data section!");
					} else if constexpr (std::is_same_v<T, StackSection>) {
						throw Problem("Cannot put a data32 into the stack section!");
					} else {
						static_assert(AlwaysFalse<T>::value, "Unimplemented section!");
					}
				}, section);
	}
	void AssemblerState::addData(Data16 data, Section section) {
		std::visit([data, this](auto&& value) {
					using T = std::decay_t<decltype(value)>;
					if constexpr (std::is_same_v<T, DataSection>) {
						_data.addData(data);
					} else if constexpr (std::is_same_v<T, StackSection>) {
						_stack.addData(data);
					} else if constexpr (std::is_same_v<T, CodeSection>) {
						throw Problem("Cannot put a data16 into the code section!");
					} else {
						static_assert(AlwaysFalse<T>::value, "Unimplemented section!");
					}
				}, section);
	}
	void AssemblerState::addLabel(const std::string& name, Section sec) {
		std::visit([name, this](auto&& value) {
						using T = std::decay_t<decltype(value)>;
						if constexpr (std::is_same_v<T, CodeSection>) {
							_code.addLabel(name);
						} else if constexpr (std::is_same_v<T, DataSection>) {
							_data.addLabel(name);
						} else if constexpr (std::is_same_v<T, StackSection>) {
							_stack.addLabel(name);
						} else {
							static_assert(AlwaysFalse<T>::value, "Unimplemented section!");
						}
					}, sec);
	}
	Address AssemblerState::getLabel(const std::string& name, Section sec) const {
		return std::visit([name, this](auto&& value) {
							using T = std::decay_t<decltype(value)>;
							if constexpr (std::is_same_v<T, CodeSection>) {
								return _code.getLabel(name);
							} else if constexpr (std::is_same_v<T, DataSection>) {
								return _data.getLabel(name);
							} else if constexpr (std::is_same_v<T, StackSection>) {
								return _stack.getLabel(name);
							} else {
								static_assert(AlwaysFalse<T>::value, "Unimplemented section!");
							}
						}, sec);
	}
	void AssemblerState::setAddress(Address value, Section section) noexcept {
		std::visit([address = value, this](auto&& value) { setAddress(address, value); }, section);
	}
	Address AssemblerState::getAddress(Section section) const noexcept {
		return std::visit([this](auto&& value) { return getAddress(value); }, section);
	}

	EvaluationFunction stack() noexcept { return [](auto& x) { x.setCurrentSection(sectionStack); }; }
	EvaluationFunction code() noexcept { return [](auto& x) { x.setCurrentSection(sectionCode); }; }
	EvaluationFunction data() noexcept { return [](auto& x) { x.setCurrentSection(sectionData); }; }
	EvaluationFunction org(Address addr) noexcept { return [addr](auto& x) { x.setAddress(addr); }; }
} // end namespace iris
