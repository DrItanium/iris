/**
 * @file 
 * Generate the clp library file from opcode data
 * @copyright 
 * iris
 * Copyright (c) 2013-2020, Joshua Scoggins and Contributors
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

#include "opcodes.h"
#include "register.h"
#include <iostream>
#include <string>
#include <functional>
#include <iomanip>
#include <list>

using Generator = std::function<void(std::ostream&)>;
Generator variable(const std::string& name) noexcept { return [name](auto& out) noexcept { out << '?' << name << ' '; }; }
Generator multifieldVariable(const std::string& name) noexcept { return [name](auto& out) noexcept { out << "$?" << name << ' '; }; }
Generator symbol(const std::string& symbol) noexcept { return [symbol](auto& out) noexcept { out << symbol << ' '; }; }
Generator string(const std::string& str) noexcept { return [str](auto& out) noexcept { out << std::quoted(str) << ' '; }; }
template<typename T, std::enable_if_t<std::is_integral_v<T>, int> = 0> 
Generator integer(T v) noexcept { 
    return [v](auto& out) noexcept { out << v << ' '; }; 
}

template<typename T, std::enable_if_t<std::is_floating_point_v<T>, int> = 0>
Generator floating(T v) noexcept {
    return [v](auto& out) noexcept { out << v << ' '; };
}

template<typename ... Args>
Generator expression(Args&& ... args) noexcept {
    return [args...](auto& out) { 
        out << "(";
        (std::invoke(args, out), ...);
        out << ") ";
    };
}
Generator 
pair(Generator first, Generator second) noexcept {
    return expression(first, second);
}
Generator expression() noexcept { 
    return [](auto& out) noexcept { out << "() " << std::endl; }; 
}
Generator 
comment(const std::string& line) noexcept {
    return [line](std::ostream& out) noexcept { out << "; " << line << std::endl; };
}

Generator
globalDeclaration(const std::string& name, const std::string& value) noexcept {
    return [name, value](std::ostream& out) noexcept { out << "?*" << name << "* = " << value << std::endl; };
}
template<typename ... Args>
Generator
defglobal(const std::string& moduleName, Args&& ... decls) noexcept {
    return expression(symbol("defglobal"),
            symbol(moduleName),
            decls...);
}
Generator nil() noexcept {
    return [](auto&) { };
}
Generator newline() noexcept {
    return [](auto& out) { out << std::endl; };
}
template<typename ... Args>
Generator
defmethod(const std::string& name, Generator args, Args&& ... body) noexcept {
    return expression(symbol("defmethod"), symbol(name), newline(),
            args, newline(),
            std::forward<Args>(body)...);
}
template<typename ... Rest>
Generator
methodArgument(Generator name, Generator first, Rest&& ... rest) noexcept {
    return expression(name, first, std::forward<Rest>(rest)...);
}

Generator
methodArgument(Generator name) noexcept {
    return name;
}

template<typename ... Args>
Generator
deffunction(const std::string& name, const std::string& description, Generator args, Args&&... body) noexcept {
    return expression(symbol("deffunction"),
            symbol(name), newline(),
            description.empty() ? nil() : string(description), newline(),
            args, newline(),
            std::forward<Args>(body)...);
}

template<typename ... Args>
Generator
deffunction(const std::string& name, Generator args, Args&& ... body) noexcept {
    return deffunction(name, "", args, std::forward<Args>(body)...);
}


template<typename ... Args>
Generator
commentedLines(Args&& ... args) noexcept {
    return [&args...](std::ostream& out) { (std::invoke(comment(args), out),...); };
}

template<typename ... Args>
Generator
createMultifield(Args&& ... args) noexcept {
    return expression(symbol("create$"), std::forward<Args>(args)...);
}

template<typename ... Args>
Generator
doFunction(const std::string& operation, Args&& ... args) noexcept {
    return expression(symbol(operation),
            std::forward<Args>(args)...);
}

int main(int, char**) {
    // emit the license out first
    commentedLines("@file",
            "Generate the clp library file from opcode data",
            "@copyright ",
            "iris",
            "Copyright (c) 2013-2020, Joshua Scoggins and Contributors",
            "All rights reserved.",
            "",
            "Redistribution and use in source and binary forms, with or without",
            "modification, are permitted provided that the following conditions are met:",
            "    * Redistributions of source code must retain the above copyright",
            "      notice, this list of conditions and the following disclaimer.",
            "    * Redistributions in binary form must reproduce the above copyright",
            "      notice, this list of conditions and the following disclaimer in the",
            "      documentation and/or other materials provided with the distribution.",
            "",
            "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND",
            "ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED",
            "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE",
            "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR",
            "ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES",
            "(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;",
            "LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND",
            "ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT",
            "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS",
            "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.",
            "",
            "Code generation routines",
            "")(std::cout);
    auto opcodeVar = variable("opcode");
    auto arg0Var = variable("arg0");
    auto arg1Var = variable("arg1");
    auto arg2Var = variable("arg2");
    auto imm16Var = variable("imm16");
    auto intType = symbol("INTEGER");
    auto first = variable("first"),
         second = variable("second"),
         third = variable("third"),
         fourth = variable("fourth"),
         rest = multifieldVariable("rest");

    return 0;
}
