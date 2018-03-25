/**
 * @file
 * Data structures and methods related to the iris architecture's assembler
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


#ifndef IRIS_CORE_ASSEMBLER_STRUCTURES_H__
#define IRIS_CORE_ASSEMBLER_STRUCTURES_H__
#include "Types.h"
#include "Assembler.h"
#include "Problem.h"
#include "Core.h"

#include <tao/pegtl.hpp>
#include <tao/pegtl/analyze.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/abnf.hpp>
#include <tao/pegtl/parse.hpp>
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include <vector>
#include <variant>
#include <memory>
#include <list>
#include <functional>

namespace iris {
	Opcode stringToOpcode(const std::string& str) noexcept;
	std::string opcodeToString(Opcode op) noexcept;
	std::string registerIndexToString(RegisterIndex index) noexcept;
	RegisterIndex stringToRegisterIndex(const std::string& str) noexcept;
	std::string decodedOperationToString(const Core::DecodedInstruction&) noexcept;
	class AssemblerState;
	template<typename T>
	using Referenceable = std::shared_ptr<T>;
	using EvaluationFunction = std::function<void(AssemblerState&)>;
	using Data32 = std::variant<Core::DecodedInstruction, Referenceable<Core::DecodedInstruction>, RawInstruction, Referenceable<RawInstruction>>;
	using Data16 = std::variant<Integer, Address, Referenceable<Address>, Referenceable<Integer>>;

	struct CodeSection final { };
	struct DataSection final { };
	struct StackSection final { };
	using Section = std::variant<CodeSection, DataSection, StackSection>;
	using Section32 = std::variant<CodeSection>;
	using Section16 = std::variant<DataSection, StackSection>;
	constexpr CodeSection sectionCode = CodeSection();
	constexpr DataSection sectionData = DataSection();
	constexpr StackSection sectionStack = StackSection();
	using LabelMap = std::map<std::string, Address>;
	template<typename S>
	std::string getSectionName() noexcept {
		if constexpr (std::is_same_v<S, CodeSection>) {
			return "code";
		} else if constexpr (std::is_same_v<S, DataSection>) {
			return "data";
		} else if constexpr (std::is_same_v<S, StackSection>) {
			return "stack";
		} else {
			static_assert(AlwaysFalse<S>::value, "Illegal section!");
		}
	}
	template<typename T, typename S>
	class SectionState {
		public:
			using DataContainer = std::map<Address, T>;
		public:
			SectionState(Address addr = 0) : _address(addr) { }
			~SectionState() { }
			Address getAddress() const noexcept { return _address; }
			void setAddress(Address value) noexcept { _address = value; }
			void addLabel(const std::string& name) {
				if (auto r = _labels.find(name); r != _labels.end()) {
					std::stringstream ss;
					ss << "Label " << name << " was already found in " << getSectionName<S>() << " section!";
					auto str = ss.str();
					throw Problem(str);
				} 
				_labels.emplace(name, _address);
			}
			Address getLabel(const std::string& name) const {
				if (auto r = _labels.find(name); r == _labels.end()) {
					std::stringstream ss;
					ss << "Label " << name << " was not found in " << getSectionName<S>() << " section!";
					auto str = ss.str();
					throw Problem(str);
				} else {
					return r->second;
				}
			}
			void addData(T value) noexcept {
				_toInstall.emplace(_address, value);
				++_address;
			}
		private:
			Address _address;
			DataContainer _toInstall;
			LabelMap _labels;
	};
	class AssemblerState {
		public:
			struct DeferEvaluation final { };
			struct NormalEvaluation final { };
			using EvaluationStyle = std::variant<NormalEvaluation, DeferEvaluation>;
			static constexpr DeferEvaluation defer = DeferEvaluation();
			static constexpr NormalEvaluation normal = NormalEvaluation();
		public:
			AssemblerState(Address codeAddress = 0, Address dataAddress = 0, Address stackAddress = 0);
			~AssemblerState();
		public:
			void setAddress(Address addr) noexcept { setAddress(addr, _currentSection); }
			void setAddress(Address addr, CodeSection) noexcept { _code.setAddress(addr); }
			void setAddress(Address addr, DataSection) noexcept { _data.setAddress(addr); }
			void setAddress(Address addr, StackSection) noexcept { _stack.setAddress(addr); }
			void setAddress(Address value, Section section) noexcept;
			
			Address getAddress() const noexcept { return getAddress(_currentSection); }
			Address getAddress(CodeSection) const noexcept { return _code.getAddress(); }
			Address getAddress(DataSection) const noexcept { return _data.getAddress(); }
			Address getAddress(StackSection) const noexcept { return _stack.getAddress(); }
			Address getAddress(Section section) const noexcept;

			void addData(EvaluationFunction fn, EvaluationStyle style = normal); 

			void addData(Data32 data) { addData(data, _currentSection); }
			void addData(Data32 data, Section section);
			void addData(Data16 data) { addData(data, _currentSection); }
			void addData(Data16 data, Section section);
			void addLabel(const std::string& name) { addLabel(name, _currentSection); }
			void addLabel(const std::string& name, Section section);
			Address getLabel(const std::string& name) const { return getLabel(name, _currentSection); }
			Address getLabel(const std::string& name, Section section) const;
			void setCurrentSection(Section s) noexcept { _currentSection = s; }
			Section getCurrentSection() const noexcept { return _currentSection; }
		private:
			Section _currentSection = sectionCode;
			SectionState<Data32, CodeSection> _code;
			SectionState<Data16, DataSection> _data;
			SectionState<Data16, StackSection> _stack;
			std::list<EvaluationFunction> _evalLater;
	};
	/**
	 * Change the address of the current section
	 * @param addr where to set the address in the current section to
	 * @return function to perform the assignment
	 */
	EvaluationFunction org(Address addr) noexcept;
	/**
	 * Change the current section to the code section
	 * @return function to perform the change
	 */
	EvaluationFunction code() noexcept;
	/**
	 * Change the current section to the data section
	 * @return function to perform the change
	 */
	EvaluationFunction data() noexcept;
	/**
	 * Change the current section to the stack section
	 * @return function to perform the change
	 */
	EvaluationFunction stack() noexcept;
} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_STRUCTURES_H__
