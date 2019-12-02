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
#ifndef IRIS_ARDUINO_CORE_H__
#define IRIS_ARDUINO_CORE_H__
#include <types.h>
#include <iris.h>
#include <register.h>

namespace iris {

class ArduinoCore : public Core {
    public:
        ArduinoCore() noexcept;
        ~ArduinoCore() override = default;

        Ordinal getTerminateCell() const noexcept override { return _terminateCell; }
        Ordinal getIP() const noexcept override { return _ip; }
        void setIP(Ordinal value) noexcept override { _ip = value; }
        void setIP(Integer value) noexcept override { _ip = value; }
        void resetExecutionStatus() noexcept override { _executing = true; }
        bool getExecutingStatus() const noexcept override { return _executing; }
        void setTerminateCell(Ordinal value) noexcept override { _terminateCell = value; }
        void putDoubleRegister(RegisterIndex lower, RegisterIndex upper, LongOrdinal value) noexcept override;
        void putDoubleRegister(RegisterIndex lower, LongOrdinal value) noexcept override;
        LongOrdinal retrieveDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept override;
        LongOrdinal retrieveDoubleRegister(RegisterIndex lower) const noexcept override;
        void stopExecution() noexcept override { _executing = false; }
        void putRegister(RegisterIndex lower, Ordinal value) noexcept override;
        void putRegister(RegisterIndex lower, Integer value) noexcept override;
        Ordinal retrieveRegister(RegisterIndex ind, RequestOrdinal) const noexcept override;
        Integer retrieveRegister(RegisterIndex ind, RequestInteger) const noexcept override;
    protected:
        void advanceIP() noexcept override { ++_ip; }
        void doNotAdvanceIP() noexcept override { _advanceIP = false; }
        void allowAdvanceIP() noexcept override { _advanceIP = true; }
        bool shouldAdvanceIP() const noexcept override { return _advanceIP; }
    protected:
        LongOrdinal loadFromCodeMemory(Address addr) override;
        Ordinal loadFromDataMemory(Address addr) override;
        Ordinal loadFromStackMemory(Address addr) override;
        Ordinal loadFromIOMemory(Address addr) override;
        void storeToCodeMemory(Address addr, LongOrdinal value) override;
        void storeToDataMemory(Address addr, Ordinal value) override;
        void storeToStackMemory(Address addr, Ordinal value) override;
        void storeToIOMemory(Address addr, Ordinal value) override;
        void raiseErrorInstruction() override;
        void raiseDivideByZero() override;
        void cycleHandler() override;
        void raiseBadOperation() override;
    private:
        Ordinal _terminateCell = 0;
        Ordinal _ip = 0;
        bool _executing = false;
        bool _advanceIP = true;
};

} // end namespace iris

#endif // end IRIS_ARDUINO_CORE_H__
