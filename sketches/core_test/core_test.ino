#include "libbonuspin.h"
#include "iris.h"
#include "arduino_core.h"

iris::ArduinoCore core;
namespace iris {
LongOrdinal 
ArduinoCore::loadFromCodeMemory(Address addr) {
    return 0;
}
Ordinal 
ArduinoCore::loadFromDataMemory(Address addr) {
    return 0;
}
Ordinal 
ArduinoCore::loadFromStackMemory(Address addr) {
    return 0;
}
Ordinal 
ArduinoCore::loadFromIOMemory(Address addr) {
    return 0;
}
void 
ArduinoCore::storeToCodeMemory(Address addr, LongOrdinal value) {
}

void 
ArduinoCore::storeToDataMemory(Address addr, Ordinal value) {

}
void 
ArduinoCore::storeToStackMemory(Address addr, Ordinal value) {

}
void 
ArduinoCore::storeToIOMemory(Address addr, Ordinal value) {
}
void 
ArduinoCore::raiseErrorInstruction() {
}
void 
ArduinoCore::raiseDivideByZero() {
}
void 
ArduinoCore::cycleHandler() {
}

void 
ArduinoCore::raiseBadOperation() {
}

void 
ArduinoCore::putDoubleRegister(RegisterIndex lower, LongOrdinal value) noexcept {
}

LongOrdinal 
ArduinoCore::retrieveDoubleRegister(RegisterIndex lower, RegisterIndex upper) const noexcept {
    return 0;
}
LongOrdinal 
ArduinoCore::retrieveDoubleRegister(RegisterIndex lower) const noexcept {
    return 0;
}

void 
ArduinoCore::putDoubleRegister(RegisterIndex lower, RegisterIndex upper, LongOrdinal value) noexcept {
}
}
void setup() {

}

void loop() {

}
