// Currently targeting the Adafruit Grand Central M4 as I want more built in pins
// Previously targeted the Adafruit Metro M0 Express but it is slow
// Also have run on teh Adafruit Metro M4 Express which is fast but not enough pins
#include <SPI.h>
#include <SD.h>
#include <Adafruit_GFX.h>
#include <Adafruit_ST7735.h>
#include <Adafruit_seesaw.h>
#include <Adafruit_TFTShield18.h>
#include <libbitmanip.h>
#include <libbonuspin.h>
#include <Wire.h>
#include "iris.h"
#include "arduino_core.h"

constexpr auto SD_CS = 4;
constexpr auto TFT_CS = 10;
constexpr auto TFT_DC = 8;
constexpr auto TFT_RST = -1;
constexpr auto LowerFRAM = 5;
constexpr auto UpperFRAM = 6;
constexpr auto CodeSectionEnable = 5;
constexpr auto DataSectionEnable = 6;
constexpr auto StackSectionEnable = 6;
template<int pin>
using CableSelectHolder = bonuspin::DigitalPinHolder<pin, LOW, HIGH>;
iris::ArduinoCore core;
Adafruit_TFTShield18 ss;
Adafruit_ST7735 tft(TFT_CS, TFT_DC, TFT_RST);
// FM25V20A FRAM CHIPS 
namespace fram {
    
enum class Opcodes : uint8_t {
  WREN = 0b00000110,
  WRDI = 0b00000100,
  RDSR = 0b00000101,
  WRSR = 0b00000001,
  READ = 0b00000011,
  FSTRD = 0b00001011,
  WRITE = 0b00000010,
  SLEEP = 0b10111001,
  RDID = 0b10011111,
};

void sendOpcode(Opcodes opcode) {
  SPI.transfer(uint8_t(opcode));
}
void transferAddress(uint32_t address) {
  SPI.transfer(uint8_t(address >> 16));
  SPI.transfer(uint8_t(address >> 8));
  SPI.transfer(uint8_t(address));
}

void sendOpcodeAndAddress(Opcodes opcode, uint32_t address) noexcept {
    sendOpcode(opcode);
    transferAddress(address);
}

void beginFastReadOperation(uint32_t address) noexcept {
    sendOpcodeAndAddress(Opcodes::FSTRD, address);
    SPI.transfer(0x00); // dummy byte
}

template<int csPin>
uint8_t read8(uint32_t address) {
  CableSelectHolder<csPin> startup;
  sendOpcodeAndAddress(Opcodes::READ, address);
  return SPI.transfer(0x00);
}

inline uint16_t getSPI16() noexcept {
    return static_cast<uint16_t>(SPI.transfer(0x00)) | (static_cast<uint16_t>(SPI.transfer(0x00)) << 8);
}


template<int enablePin>
uint16_t read16(uint32_t address) {
  CableSelectHolder<enablePin> reader;
  beginFastReadOperation(address << 1);
  return getSPI16();
}
template<int enablePin>
uint32_t read32(uint32_t address) {
  CableSelectHolder<enablePin> reader;
  beginFastReadOperation(address << 2);
  auto lower = static_cast<uint32_t>(getSPI16());
  auto upper = static_cast<uint32_t>(getSPI16()) << 16;
  return lower | upper;
}

template<int csPin>
void enableWrites() {
  CableSelectHolder<csPin> startup;
  // send WREN before any write operation
  sendOpcode(Opcodes::WREN); 
}
template<int csPin>
void writeFram(uint32_t address, uint8_t value) { 
  enableWrites<csPin>();
  CableSelectHolder<csPin> startup;
  sendOpcodeAndAddress(Opcodes::WRITE, address);
  SPI.transfer(value);
}
inline void putSPI16(uint16_t value) noexcept {
    SPI.transfer(uint8_t(value));
    SPI.transfer(uint8_t(value >> 8));
}
template<int csPin>
void writeFram(uint32_t address, uint16_t value) {
  enableWrites<csPin>();
  CableSelectHolder<csPin> startup;
  sendOpcodeAndAddress(Opcodes::WRITE, address << 1);
  putSPI16(value);
}
template<int csPin>
void writeFram(uint32_t address, uint32_t value) {
  enableWrites<csPin>();
  CableSelectHolder<csPin> startup;
  sendOpcodeAndAddress(Opcodes::WRITE, address << 2);
  putSPI16(value);
  putSPI16(value >> 16);
}
constexpr auto max8BitAddress = 0x40000;
constexpr auto max16BitAddress = max8BitAddress >> 1;
constexpr auto max32BitAddress = max8BitAddress >> 2;
} // end namespace fram
// so there are two fram chips with 256 kbytes each,
// fram chip 1 is reserved for code memory
// fram chip 2 is split between data and stack
// the lower 128 kbytes of chip2 contains data
// the upper 128 kbytes of chip2 contains stack
//
// it may be simpler to think of the chips in terms of words:
// Chip 2 contains 128 kwords where 1 word = 2 bytes
// Chip 1 contains 64 kdwords where 1 dword = 2 words = 4 bytes
// thus chip 2 has two 64 kword sections
namespace iris {
LongOrdinal 
ArduinoCore::loadFromCodeMemory(Address addr) {
    return fram::read32<CodeSectionEnable>(addr);
}
Ordinal 
ArduinoCore::loadFromDataMemory(Address addr) {
    return fram::read16<DataSectionEnable>(addr);
}
Ordinal 
ArduinoCore::loadFromStackMemory(Address addr) {
    // the read routines disallow unaligned loads and stores
    //
    // iris addresses are 16-bit aligned so internally read16 will shift by one
    // to convert it to an 8-bit aligned address, thus we want to mark the
    // stack address as being the "upper" part of chip 2
    static constexpr uint32_t stackBaseAddress = 0b1'0000'0000'0000'0000;
    uint32_t internalAddress = stackBaseAddress | static_cast<uint32_t>(addr);
    return fram::read16<StackSectionEnable>(internalAddress);
    return 0;
}
Ordinal 
ArduinoCore::loadFromIOMemory(Address addr) {
    // io memory is special as it is really just a programmable section which
    // allows me to wire up all kinds of devices here
    return 0;
}
void 
ArduinoCore::storeToCodeMemory(Address addr, LongOrdinal value) {
    fram::writeFram<CodeSectionEnable>(addr, value);
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
ArduinoCore::raiseBadOperation() {
}

void 
ArduinoCore::cycleHandler() {
    // @todo figure out how to handle bad operations, divide by zero, error
    // instructions etc.
    cycle();
}

} // end namespace iris
constexpr iris::RegisterIndex framAddressRegisterIndex{20};
constexpr iris::RegisterIndex framValueRegisterLowerIndex{22};
constexpr iris::RegisterIndex framValueRegisterUpperIndex{23};
constexpr iris::RegisterIndex framReadbackRegisterLowerIndex{24};
constexpr iris::RegisterIndex framReadbackRegisterUpperIndex{25};
constexpr iris::RegisterIndex framCompareStorage{26};
constexpr iris::RegisterIndex numBytesRegisterIndex{4};

void failure() noexcept {
    while(true) {
        digitalWrite(LED_BUILTIN, HIGH);
        delay(1000);
        digitalWrite(LED_BUILTIN, LOW);
        delay(1000);
    }
}
void setup() {
    core.setRegisterValue(framAddressRegisterIndex,       iris::Ordinal(0));
    core.setRegisterValue(framValueRegisterLowerIndex,    iris::Ordinal(0));
    core.setRegisterValue(framValueRegisterUpperIndex,    iris::Ordinal(0));
    core.setRegisterValue(framReadbackRegisterLowerIndex, iris::Ordinal(0));
    core.setRegisterValue(framReadbackRegisterUpperIndex, iris::Ordinal(0));
    core.setRegisterValue(framCompareStorage, iris::Ordinal(0));
    Serial.begin(9600);
    pinMode(TFT_CS, OUTPUT);
    digitalWrite(TFT_CS, HIGH);
    pinMode(SD_CS, OUTPUT);
    digitalWrite(SD_CS, HIGH);
    pinMode(UpperFRAM, OUTPUT);
    pinMode(LowerFRAM, OUTPUT);
    digitalWrite(UpperFRAM, HIGH);
    digitalWrite(LowerFRAM, HIGH);
    pinMode(LED_BUILTIN, OUTPUT);
    digitalWrite(LED_BUILTIN, LOW);

    if (!ss.begin()) {
        Serial.println("seesaw could not be initialized");
        failure();
    }
    ss.setBacklight(TFTSHIELD_BACKLIGHT_OFF);

    ss.tftReset();

    tft.initR(INITR_BLACKTAB);

    ss.setBacklight(TFTSHIELD_BACKLIGHT_ON);

    delay(101);
    tft.fillScreen(ST77XX_BLACK);
}


void loop() {
    tft.setTextColor(ST77XX_WHITE, ST77XX_BLACK);
    tft.setCursor(0, 0);
    tft.print("AD: ");
    tft.print(core.getRegisterValue<iris::Ordinal>(framAddressRegisterIndex), HEX);
    tft.setCursor(0, 10);
    tft.print("VA: ");
    tft.print(core.getDoubleRegisterValue(framValueRegisterLowerIndex), HEX);
    core.storeCode<iris::Address, iris::LongOrdinal>(
            core.getRegisterValue<iris::Ordinal>(framAddressRegisterIndex),
            core.getDoubleRegisterValue(framValueRegisterLowerIndex));
    core.setDoubleRegisterValue(framReadbackRegisterLowerIndex, 
            core.loadCode<iris::Address>(
                core.getRegisterValue<iris::Ordinal>(framAddressRegisterIndex)));
    tft.setCursor(0, 20);
    tft.print("RB: ");
    tft.print(core.getDoubleRegisterValue(framReadbackRegisterLowerIndex), HEX);
    tft.setCursor(0, 30);
    tft.print("WD: ");
    tft.print(core.getRegisterValue<iris::Ordinal>(numBytesRegisterIndex), HEX);
    tft.setCursor(0, 40);
    tft.print("STATUS: ");
    core.setRegisterValue(framCompareStorage,
        core.getDoubleRegisterValue(framReadbackRegisterLowerIndex) !=
        core.getDoubleRegisterValue(framValueRegisterLowerIndex));
    if (core.getRegisterValue<bool>(framCompareStorage)) {
        tft.print("FAIL!");
        failure();
    } else {
        tft.print("OK");
    }
    core.setRegisterValue(framAddressRegisterIndex,
            iris::Ordinal(core.getRegisterValue<iris::Ordinal>(framAddressRegisterIndex) + 1));
    core.setDoubleRegisterValue(framValueRegisterLowerIndex, random(0x1FFFFFF));

    delay(1);
}
