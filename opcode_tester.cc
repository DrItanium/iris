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

#include "iris.h"
#include "opcodes.h"
#include <iostream>

bool codeTests(iris::Core& c) {
    std::cout << "Code writing tests" << std::endl;
    for (iris::Address i = 0; i < static_cast<iris::Address>(iris::Opcodes::Count); ++i) {
        c.storeCode<iris::Address, iris::DoubleWord>(i, static_cast<iris::DoubleWord>(i));
        if (auto result = c.loadCode(i, 0); result != static_cast<iris::DoubleWord>(i)) {
            std::cout << "Write to code memory failed" << std::endl;
            std::cout << "\tGot: " << std::hex << result << std::endl;
            std::cout << "\tExpected: " << std::hex << static_cast<iris::DoubleWord>(i) << std::endl;
            return true;
        }
    }
    return false;

}
bool dataTests(iris::Core& c) {
    std::cout << "Data writing tests" << std::endl;
    std::cout << "\t1. Write and readback from data memory" << std::endl;
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        iris::Word valueToWrite(~i);
        // data write
        c.storeData<iris::Address, iris::Word>(i, valueToWrite);
        if (auto result = c.loadData<iris::Address>(i); result != valueToWrite) {
            std::cout << "Write to data memory failed" << std::endl;
            std::cout << "\tGot: " << std::hex << result << std::endl;
            std::cout << "\tExpected: " << std::hex << valueToWrite<< std::endl;
            return true;
        }
    }
    return false;
}
bool stackTests(iris::Core& c) {
    std::cout << "Stack memory tests" << std::endl;
    std::cout << "\t1. Write and readback from stack memory" << std::endl;
    for (iris::DoubleWord i = 0; i < 0x10000; ++i) {
        iris::Word valueToWrite(~(i + 12));
        // data write
        c.storeStack<iris::Address, iris::Word>(i, valueToWrite);
        if (auto result = c.loadStack<iris::Address>(i); result != valueToWrite) {
            std::cout << "Write to data memory failed" << std::endl;
            std::cout << "\tGot: " << std::hex << result << std::endl;
            std::cout << "\tExpected: " << std::hex << valueToWrite<< std::endl;
            return true;
        }
    }
    return false;
}
int main(int, char* []) {
    iris::Core c;
    if (codeTests(c)) {
        return 1;
    } 
    if (dataTests(c)) {
        return 1;
    }
    if (stackTests(c)) {
        return 1;
    }


    std::cout << "All tests passed!" << std::endl;
    return 0;
}
