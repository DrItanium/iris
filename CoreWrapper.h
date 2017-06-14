/*
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


// Common CLIPS <-> C++ core wrapper
#ifndef CORE_WRAPPER_H__
#define CORE_WRAPPER_H__

#include "ClipsExtensions.h"
#include <map>

namespace syn {


// Paste these default core operations in
#define __DEFAULT_CORE_OPERATIONS__ \
        Initialize, \
        Shutdown, \
        Run, \
        Cycle

#define __DEFAULT_ERROR_STATE__ Count

#define __DEFAULT_CORE_OPERATIONS_EXEC__(TYPE) \
    case TYPE :: Initialize: initialize(); break; \
    case TYPE :: Shutdown: shutdown(); break; \
    case TYPE :: Run: run(); break; \
    case TYPE :: Cycle: CVSetBoolean(ret, cycle()); break

template<typename T>
bool badCallArgument(void* env, CLIPSValue* ret, int code, const std::string& msg) noexcept {
    static bool init = true;
    static std::string funcErrorPrefix;
    if (init) {
        funcErrorPrefix = std::get<2>(syn::retrieveFunctionNames<T>("call"));
        init = false;
    }
    CVSetBoolean(ret, false);
    return syn::errorMessage(env, "CALL", code, funcErrorPrefix, msg);
}




inline bool setClipsBoolean(CLIPSValue* ret) noexcept {
    CVSetBoolean(ret, true);
    return true;
}

template<typename T>
class CoreWrapper : public syn::ExternalAddressWrapper<T> {
    public:
        using Parent = syn::ExternalAddressWrapper<T>;
        using Self = CoreWrapper<T>;
    public:
        static inline bool badCallArgument(void* env, CLIPSValue* ret, int code, const std::string& msg) noexcept {
            return syn::badCallArgument<T>(env, ret, code, msg);
        }
        template<int code>
        static inline bool badCallArgument(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return badCallArgument(env, ret, code, msg);
        }
        static inline bool callErrorCode2(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return badCallArgument<2>(env, ret, msg);
        }

        static inline bool callErrorCode3(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return badCallArgument<3>(env, ret, msg);
        }

        static inline bool callErrorCode4(void* env, CLIPSValue* ret, const std::string& msg) noexcept {
            return badCallArgument<4>(env, ret, msg);
        }
        static bool callErrorMessage(void* env, CLIPSValue* ret, int code, const std::string& subOp, const std::string& rest) {
            std::stringstream stm;
            stm << " " << subOp << ": " << rest << std::endl;
            auto msg = stm.str();
            return badCallArgument(env, ret, code, msg);
        }

        static inline bool callErrorMessageCode3(void* env, CLIPSValue* ret, const std::string& subOp, const std::string& rest) noexcept {
            return callErrorMessage(env, ret, 3, subOp, rest);
        }

        using GenericTryGetFromCallFunction = std::function<bool(void*, const std::string&, int, CLIPSValue*)>;
        static bool tryGetArgumentAsGenericFromCall(void* env, CLIPSValue* ret, int pos, GenericTryGetFromCallFunction fn) noexcept {
            static bool init = true;
            static std::string funcStr;
            if (init) {
                init = false;
                funcStr = std::get<1>(syn::retrieveFunctionNames<T>("call"));
            }
            return fn(env, funcStr, pos, ret);
        }

        static inline bool tryGetArgumentAsSymbolFromCall(void* env, CLIPSValue* ret, int pos) noexcept {
            return tryGetArgumentAsGenericFromCall(env, ret, pos, tryGetArgumentAsSymbol);
        }

        static inline bool tryGetArgumentAsIntegerFromCall(void* env, CLIPSValue* ret, int pos) noexcept {
            return tryGetArgumentAsGenericFromCall(env, ret, pos, tryGetArgumentAsInteger);
        }

        static bool tryExtractInteger(void* env, CLIPSValue* ret, CLIPSValue* storage, int pos, int errorCode, const std::string& msg) noexcept {
            if (!tryGetArgumentAsIntegerFromCall(env, storage, pos)) {
                return badCallArgument(env, ret, errorCode, msg);
            }
            return true;
        }

        static inline bool tryExtractIntegerErrorCode3(void* env, CLIPSValue* ret, CLIPSValue* storage, int pos, const std::string& msg) noexcept {
            return tryExtractInteger(env, ret, storage, pos, 3, msg);
        }

        static bool tryExtractFunctionName(void* env, CLIPSValue* ret, CLIPSValue* storage) noexcept {
            if (!tryGetArgumentAsSymbolFromCall(env, storage, 2)) {
                return badCallArgument(env, ret, 2, "expected a function name to call!");
            }
            return true;
        }

        static bool callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
            static bool init = true;
            static std::string funcErrorPrefix;
            if (init) {
                init = false;
                auto functions = syn::retrieveFunctionNames<T>("call");
                funcErrorPrefix = std::get<2>(functions);
            }
            if (!syn::isExternalAddress(value)) {
                return syn::errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
            }
            auto ptr = static_cast<Self*>(EnvDOPToExternalAddress(value));
            return ptr->get()->handleOperation(env, ret);
        }
        static void registerWithEnvironment(void* env, const char* title) {
            Parent::registerWithEnvironment(env, title, callFunction);
        }

        static void registerWithEnvironment(void* env) {
            static bool init = true;
            static std::string func;
            if (init) {
                init = false;
                func = Self::getType();
            }
            registerWithEnvironment(env, func.c_str());
        }
   public:
        CoreWrapper(T* core) : Parent(core) { }
        CoreWrapper() : Parent(new T()) { }
        virtual ~CoreWrapper() { }
};





} // end namespace syn
#endif // end CORE_WRAPPER_H__
