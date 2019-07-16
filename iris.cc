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

namespace iris {
struct InstructionLogic {
    virtual ~InstructionLogic() = default;
    virtual void invoke() noexcept = 0;
    inline void operator()() noexcept { invoke(); }
};
using SourceStorage = std::variant<Word, SourceRegister>;
struct ThreeArgumentFormat : InstructionLogic {
    public:
        ThreeArgumentFormat(DestinationRegister& d, SourceRegister& s0, SourceStorage s1) : _dest(d), _src0(s0), _src1(s1) { }
        virtual ~ThreeArgumentFormat() = default;
    protected:
        DestinationRegister _dest;
        SourceRegister _src0;
        SourceStorage _src1;
};
struct TwoArgumentFormat : InstructionLogic {
    public:
        TwoArgumentFormat(DestinationRegister& d, SourceStorage s0) : _dest(d), _src0(s0) { }
        virtual ~TwoArgumentFormat() = default;
    protected:
        DestinationRegister _dest;
        SourceStorage _src0;
};
struct OneArgumentFormat : InstructionLogic {
    public:
        using Storage = std::variant<Word, DestinationRegister, SourceRegister>;
    public: 
        OneArgumentFormat(Storage storage) : _storage(storage) { }
        virtual ~OneArgumentFormat() = default;
    protected:
        Storage _storage;
};
}
