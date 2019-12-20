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
#ifndef IRIS_MEMCORE_H__
#define IRIS_MEMCORE_H__
#include <array>
#include <functional>
#include <map>
#include <memory>
#include <list>
#include <iris.h>
#include <optional>
#include "IODevices.h"
#include "mem_bank.h"
#include "register.h"

namespace iris {
class MemoryMappedDevice {
    public:
        using Ptr = std::shared_ptr<MemoryMappedDevice>;
    public:
        MemoryMappedDevice() = default;
        virtual ~MemoryMappedDevice() = default;
        virtual bool respondsTo(Address address) const noexcept = 0;
        virtual Ordinal loadFull(Address address) noexcept = 0;
        virtual QuarterOrdinal loadQuarter(Address address) noexcept = 0;
        virtual HalfOrdinal loadHalf(Address address) noexcept = 0;
        virtual void store(Address address, Ordinal value) noexcept = 0;
        virtual void storeHalf(Address address, HalfOrdinal value) noexcept = 0;
        virtual void storeQuarter(Address address, QuarterOrdinal value) noexcept = 0;
};
class CaptiveMemoryMappedDevice : public MemoryMappedDevice {
    public:
        CaptiveMemoryMappedDevice(const MemoryMappedDevice::Ptr& captive) : _captive(captive) { }
        ~CaptiveMemoryMappedDevice() override = default;
        constexpr bool isMapped() const noexcept { return _mapped; }
        void map() noexcept { _mapped = true; }
        void unmap() noexcept { _mapped = false; }
        bool respondsTo(Address a) const noexcept override { 
            return _mapped && _captive->respondsTo(a); 
        }
        Ordinal loadFull(Address address) noexcept override { 
            return _captive->loadFull(address); 
        }
        QuarterOrdinal loadQuarter(Address address) noexcept override { 
            return _captive->loadQuarter(address); 
        }
        HalfOrdinal loadHalf(Address address) noexcept override { 
            return _captive->loadHalf(address); 
        }
        void store(Address address, Ordinal value) noexcept override {
            return _captive->store(address, value);
        }
        void storeQuarter(Address address, QuarterOrdinal value) noexcept override {
            return _captive->storeQuarter(address, value);
        }
        void storeHalf(Address address, HalfOrdinal value) noexcept override {
            return _captive->storeHalf(address, value);
        }
    private:
        bool _mapped = false;
        MemoryMappedDevice::Ptr _captive;

};
/** 
 * Use ram to denote the different memory spaces
 */
class InMemoryCore : public Core {
    public:
        InMemoryCore() noexcept;
        ~InMemoryCore() override = default;
        Ordinal getTerminateCell() const noexcept override { return _terminateCell; }
        Ordinal getIP() const noexcept override { return _ip.get<Word>(); }
        void setIP(Ordinal value) noexcept override { _ip.put(value); }
        void resetExecutionStatus() noexcept override { _executing = true; }
        bool getExecutingStatus() const noexcept override { return _executing; }
        void setTerminateCell(Ordinal value) noexcept override { _terminateCell = value; }
        void putRegister(RegisterIndex lower, Ordinal value) noexcept override {
            _regs[std::to_integer<Byte>(lower)].put(value);
        }
        void putRegister(RegisterIndex lower, Integer value) noexcept override {
            _regs[std::to_integer<Byte>(lower)].put(value);
        }
        void putRegister(RegisterIndex lower, bool value) noexcept override {
            _regs[std::to_integer<Byte>(lower)].put(value);
        }
        Ordinal retrieveRegister(RegisterIndex ind, RequestOrdinal) const noexcept override {
            return _regs[std::to_integer<Byte>(ind)].get<Ordinal>();
        }
        Integer retrieveRegister(RegisterIndex ind, RequestInteger) const noexcept override {
            return _regs[std::to_integer<Byte>(ind)].get<Integer>();
        }
        bool retrieveRegister(RegisterIndex ind, RequestBoolean) const noexcept override {
            return _regs[std::to_integer<Byte>(ind)].get<bool>();
        }
        void setIP(Integer value) noexcept override { _ip.put(value); }
        void stopExecution() noexcept override { _executing = false; }
        void mapDevice(MemoryMappedDevice::Ptr device) noexcept;
    protected:
        void advanceIP() noexcept override {
            ++_ip;
        }
        void doNotAdvanceIP() noexcept override {
            _advanceIP = false;
        }
        void allowAdvanceIP() noexcept override {
            _advanceIP = true;
        }
        bool shouldAdvanceIP() const noexcept override {
            return _advanceIP;
        }
    private:
        std::optional<CaptiveMemoryMappedDevice> findDevice(Address address) noexcept;
        static constexpr Address computeOrdinalAddress(Address addr) noexcept {
            Address mask = ~0b11;
            return addr & mask;
        }
        static constexpr Address computeHalfAddress(Address addr) noexcept {
            Address mask = ~0b1;
            return addr & mask;
        }
    protected:
        void raiseErrorInstruction() override;
        void raiseDivideByZero() override;
        void cycleHandler() override;
        void raiseBadOperation() override;
        Ordinal loadFromMemory(Address address) noexcept override;
        QuarterOrdinal loadQuarterFromMemory(Address address) noexcept override;
        HalfOrdinal loadHalfFromMemory(Address address) noexcept override;
        void storeToMemory(Address address, Ordinal value) noexcept override;
        void storeToMemory(Address address, HalfOrdinal value) noexcept override;
        void storeToMemory(Address address, QuarterOrdinal value) noexcept override;

    private:
        RegisterBank _regs;
        Register _ip;
        std::list<CaptiveMemoryMappedDevice> _memoryMap;
        bool _executing = false;
        bool _advanceIP = true;
        Word _terminateCell = 0;

};
} // end namespace iris

#endif // end IRIS_MEMCORE_H__
