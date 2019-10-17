/**
 * @copyright 
 * iris
 * Copyright (c) 2013-2019, Joshua Scoggins and Contributors
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
#ifndef IRIS_EXCEPTIONS_H__
#define IRIS_EXCEPTIONS_H__
#include <exception>
#include <sstream>
#include "types.h"
namespace iris {

template<typename ... Args>
void print(std::ostream& os, Args&& ... args) noexcept {
    (os <<  ... << args) << std::endl;
}
class Exception : public std::exception {
    public:
        template<typename ... Args>
        Exception(Args&& ... args) noexcept {
            std::ostringstream os;
            print(os, args...);
            _msg = os.str();
        }
        virtual ~Exception() = default;
        const char* what() const noexcept override final;
        auto message() const { return _msg; }
    private:
        std::string _msg;
};

class UnimplementedOperationException : public Exception {
    public:
        UnimplementedOperationException() noexcept : Exception("Unimplemented operation") { }
        ~UnimplementedOperationException() override = default;
};
class BadOperationException : public Exception {
    public:
        BadOperationException() noexcept : Exception("Bad operation") { }
        ~BadOperationException() override = default;
};
class ErrorInstructionException : public Exception {
    public:
        ErrorInstructionException() noexcept : Exception("Error instruction raised!") { }
        ~ErrorInstructionException() override = default;
};
class DivideByZeroException : public Exception {
    public:
        DivideByZeroException() noexcept : Exception("Divide by zero!") { }
        ~DivideByZeroException() override = default;
};
class MemoryLoadException : public Exception {
    public:
        template<typename ... Args>
        MemoryLoadException(Args&& ... args) noexcept : Exception("MemoryLoadException: ", std::forward<Args>(args)...) { }
        ~MemoryLoadException() override = default;
};
class MemoryStoreException : public Exception {
    public:
        template<typename ... Args>
        MemoryStoreException(Args&& ... args) noexcept : Exception("MemoryStoreException: ", std::forward<Args>(args)...) { }
        ~MemoryStoreException() override = default;
};
class MMIOException : public Exception {
    public:
        template<typename ... Args>
        MMIOException(Args&& ... args) noexcept : Exception("MMIOException: ", std::forward<Args>(args)...) { }
        ~MMIOException() override = default;
};
} // end namespace iris

#endif // end IRIS_EXCEPTIONS_H__
