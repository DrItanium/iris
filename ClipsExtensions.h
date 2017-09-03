/**
 * @file
 * Types, functions, classes, and concepts for making it far easier to
 * interface with CLIPS from c++.
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


#ifndef __SYN_CLIPS_H
#define __SYN_CLIPS_H
#include <map>
#include <memory>
#include <sstream>
#include <iostream>
#include <typeinfo>
extern "C" {
	#include "clips.h"
}

namespace syn {
/// Wrapper over the CLIPS data objet type
using DataObject = DATA_OBJECT;
/// Wrapper over the CLIPS data object pointer type
using DataObjectPtr = DATA_OBJECT_PTR;

/**
 * Install extended user functions to make life easier.
 * @param theEnv the environment to install the extended user functions into
 */
void installExtensions(void* theEnv);

/**
 * A wrapper enum for interfacing with CLIPS' constants
 */
enum class MayaType {
    Integer = INTEGER,
    Float = FLOAT,
    ExternalAddress = EXTERNAL_ADDRESS,
    Symbol = SYMBOL,
    String = STRING,
    Lexeme = SYMBOL_OR_STRING,
    Multifield = MULTIFIELD,
};

/**
 * retrieves the argument count of the function call originating in CLIPS.
 * @param env the environment where the function call took place
 * @return the number of arguments that were passed
 */
int getArgCount(void* env) noexcept;
/**
 * performs a check to see if the number of arguments equals the expected count
 * @param env the environment to check
 * @param compare the expected number of args
 * @return true if compare equals the actual argument count
 */
bool hasCorrectArgCount(void* env, int compare) noexcept;

/**
 * Return true if the given dataObjetPtr is tagged as an ExternalAddress type
 * @param value the dataObjectPtr to check
 * @return true if the given dataObjectPtr contains an external address
 */
bool isExternalAddress(DataObjectPtr value) noexcept;

CLIPSInteger extractLong(void* env, DataObjectPtr value) noexcept;
CLIPSInteger extractLong(void* env, DataObject& value) noexcept;
template<typename Ret>
Ret extractLong(void* env, DataObjectPtr value) noexcept {
    return static_cast<Ret>(extractLong(env, value));
}

template<typename Ret>
Ret extractLong(void* env, DataObject& value) noexcept {
    return static_cast<Ret>(extractLong(env, value));
}

const char* extractLexeme(void* env, DataObjectPtr value) noexcept;
const char* extractLexeme(void* env, DataObject& value) noexcept;

bool checkThenGetArgument(void* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsInteger(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsSymbol(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsString(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;

/**
 * Wrapper method for setting a clips value to a boolean value. The boolean
 * value is also returned as a way to set the ret pointer and return a boolean
 * value from a function.
 * @param ret the area to store the boolean into
 * @param value the boolean value itself (defaults to true)
 * @return the input argument 'value'
 */
inline bool setClipsBoolean(CLIPSValue* ret, bool value = true) noexcept {
    CVSetBoolean(ret, value);
    return value;
}
/**
 * Check and see if the given CLIPS argument is of a given type and extract it
 * if it is.
 * @param env the environment to perform the check on
 * @param function the user defined function where this check is taking place
 * @param position the one-indexed position of the argument
 * @param type the type that is desired
 * @param saveTo the data object to where the argument will be stored to on
 * successful find
 * @return true if the given argument is of the correct type.
 */
bool checkThenGetArgument(void* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept;

/**
 * Output an error message through clips
 * @param env the environment where the error happened
 * @param idClass the error class
 * @param idIndex the error index
 * @param msgPrefix the prefix to add
 * @param msg the message to display
 * @return bool signifying if successful error output occurred
 */
bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;

/**
 * Common implementation for printing out an external address from within
 * CLIPS. Unless you've got really specific or odd requirements, it is
 * suggested that this be used as a base.
 * @param env the environment that called this function
 * @param logicalName the io router to output to
 * @param theValue the raw value that is printed (well it's address)
 * @param func The type of the given externalAddressType
 * @param majorType Used for appending Wrapper, etc to the output name
 */
void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType);


}
#endif
