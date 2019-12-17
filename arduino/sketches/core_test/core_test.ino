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

iris::ArduinoCore core;
constexpr auto SD_CS = 4;
constexpr auto TFT_CS = 10;
constexpr auto TFT_DC = 8;
constexpr auto TFT_RST = -1;
Adafruit_TFTShield18 ss;
Adafruit_ST7735 tft(TFT_CS, TFT_DC, TFT_RST);
constexpr auto LowerFRAM = 5;
constexpr auto UpperFRAM = 6;
template<int pin>
using CableSelectHolder = bonuspin::DigitalPinHolder<pin, LOW, HIGH>;
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

template<int csPin>
uint8_t read8(uint32_t address) {
  CableSelectHolder<csPin> startup;
  sendOpcode(Opcodes::READ);
  transferAddress(address);
  return SPI.transfer(0x00);
}

template<int enablePin>
uint16_t read16(uint32_t address) {
  CableSelectHolder<enablePin> reader;
  sendOpcode(Opcodes::READ);
  transferAddress(address << 1);
  auto lower = static_cast<uint16_t>(SPI.transfer(0x00));
  auto upper = static_cast<uint16_t>(SPI.transfer(0x00)) << 8;
  return lower | upper;
}
template<int enablePin>
uint32_t read32(uint32_t address) {
  CableSelectHolder<enablePin> reader;
  sendOpcode(Opcodes::READ);
  transferAddress(address << 2);
  auto lowest = static_cast<uint32_t>(SPI.transfer(0x00));
  auto lower = static_cast<uint32_t>(SPI.transfer(0x00)) << 8;
  auto higher = static_cast<uint32_t>(SPI.transfer(0x00)) << 16;
  auto highest = static_cast<uint32_t>(SPI.transfer(0x00)) << 24;
  return lowest | lower | higher | highest;
}

template<int enablePin>
uint64_t read64(uint64_t address) {
  CableSelectHolder<enablePin> reader;
  sendOpcode(Opcodes::READ);
  transferAddress(address << 3);
  auto a = static_cast<uint64_t>(SPI.transfer(0x00));
  auto b = static_cast<uint64_t>(SPI.transfer(0x00)) << 8;
  auto c = static_cast<uint64_t>(SPI.transfer(0x00)) << 16;
  auto d = static_cast<uint64_t>(SPI.transfer(0x00)) << 24;
  auto e = static_cast<uint64_t>(SPI.transfer(0x00)) << 32;
  auto f = static_cast<uint64_t>(SPI.transfer(0x00)) << 40;
  auto g = static_cast<uint64_t>(SPI.transfer(0x00)) << 48;
  auto h = static_cast<uint64_t>(SPI.transfer(0x00)) << 56;
  return a | b | c | d | e | f | g | h;
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
  // send WREN before any write operation
  sendOpcode(Opcodes::WRITE);
  transferAddress(address);
  SPI.transfer(value);
}
template<int csPin>
void writeFram(uint32_t address, uint16_t value) {
  enableWrites<csPin>();
  CableSelectHolder<csPin> startup;
  sendOpcode(Opcodes::WRITE);
  transferAddress(address << 1);
  SPI.transfer(uint8_t(value));
  SPI.transfer(uint8_t(value >> 8));
}
template<int csPin>
void writeFram(uint32_t address, uint32_t value) {
  enableWrites<csPin>();
  CableSelectHolder<csPin> startup;
  sendOpcode(Opcodes::WRITE);
  transferAddress(address << 2);
  SPI.transfer(uint8_t(value));
  SPI.transfer(uint8_t(value >> 8));
  SPI.transfer(uint8_t(value >> 16));
  SPI.transfer(uint8_t(value >> 24));
}
template<int csPin>
void writeFram(uint32_t address, uint64_t value) {
  enableWrites<csPin>();
  CableSelectHolder<csPin> startup;
  sendOpcode(Opcodes::WRITE);
  transferAddress(address << 3);
  SPI.transfer(uint8_t(value));
  SPI.transfer(uint8_t(value >> 8));
  SPI.transfer(uint8_t(value >> 16));
  SPI.transfer(uint8_t(value >> 24));
  SPI.transfer(uint8_t(value >> 36));
  SPI.transfer(uint8_t(value >> 40));
  SPI.transfer(uint8_t(value >> 48));
  SPI.transfer(uint8_t(value >> 56));
  
}
constexpr auto max8BitAddress = 0x40000;
constexpr auto max16BitAddress = max8BitAddress >> 1;
constexpr auto max32BitAddress = max8BitAddress >> 2;
constexpr auto max64BitAddress = max8BitAddress >> 3;
} // end namespace fram
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
ArduinoCore::raiseBadOperation() {
}

} // end namespace iris
void setup() {
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
        while(true) {
            digitalWrite(LED_BUILTIN, HIGH);
            delay(1000);
            digitalWrite(LED_BUILTIN, LOW);
            delay(1000);
        }
    }
    ss.setBacklight(TFTSHIELD_BACKLIGHT_OFF);

    ss.tftReset();

    tft.initR(INITR_BLACKTAB);

    ss.setBacklight(TFTSHIELD_BACKLIGHT_ON);

    delay(101);
    tft.fillScreen(ST77XX_BLACK);
}

uint32_t framAddress = 0;
uint32_t framValue = 0;
uint32_t readback = 0;
uint8_t numBytes = 4;

void loop() {
    tft.setTextColor(ST77XX_WHITE, ST77XX_BLACK);
    tft.setCursor(0, 0);
    tft.print("AD: ");
    tft.print(framAddress, HEX);
    tft.setCursor(0, 10);
    tft.print("VA: ");
    tft.print(framValue, HEX);
    auto localAddress = framAddress & 0xFFFF; // most significant bit must be clipped
    if (framAddress & 0x10000) {
        fram::writeFram<UpperFRAM>(localAddress, framValue);
        readback = fram::read32<UpperFRAM>(localAddress);
    } else {
        fram::writeFram<LowerFRAM>(localAddress, framValue);
        readback = fram::read32<LowerFRAM>(localAddress);
    }
    tft.setCursor(0, 20);
    tft.print("RB: ");
    tft.print(readback, HEX);
    tft.setCursor(0, 30);
    tft.print("WD: ");
    tft.print(numBytes);
    tft.setCursor(0, 40);
    tft.print("STATUS: ");
    if (readback != framValue) {
        tft.print("FAIL!");
        while(true);
    } else {
        tft.print("OK");
    }

    framAddress++;
    framAddress &= (0x7FFFF >> 2);  

    framValue = random(0x1FFFFFF);  
    delay(10);
}
