 #include <SPI.h>
#include <libbitmanip.h>
#include <libbonuspin.h>
#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define OLED_RESET 9
Adafruit_SSD1306 display(OLED_RESET);
template<int pin>
using CableSelectHolder = bonuspin::DigitalPinHolder<pin, LOW, HIGH>;

enum class FRAMOpcodes : uint8_t {
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


//SRAM opcodes
// pins associated with the different spi memory chips
// two spi chips are necessary to hold code space so we have to do some 
// math to figure out which chip to select
constexpr auto csPin = 10;
constexpr auto soPin = 12;
constexpr auto sckPin = 13;
constexpr auto siPin = 11;
constexpr auto delayAmount = 30;

void sendOpcode(FRAMOpcodes opcode) {
  SPI.transfer(uint8_t(opcode));
}
void transferAddress(uint32_t address) {
  SPI.transfer(uint8_t(address >> 16));
  SPI.transfer(uint8_t(address >> 8));
  SPI.transfer(uint8_t(address));
}


uint8_t read8(uint32_t address) {
  CableSelectHolder<csPin> startup;
  sendOpcode(FRAMOpcodes::READ);
  transferAddress(address);
  return SPI.transfer(0x00);
}

uint16_t read16(uint32_t address) {
  CableSelectHolder<csPin> reader;
  sendOpcode(FRAMOpcodes::READ);
  transferAddress(address << 1);
  auto lower = static_cast<uint16_t>(SPI.transfer(0x00));
  auto upper = static_cast<uint16_t>(SPI.transfer(0x00)) << 8;
  return lower | upper;
}

uint32_t read32(uint32_t address) {
  CableSelectHolder<csPin> reader;
  sendOpcode(FRAMOpcodes::READ);
  transferAddress(address << 2);
  auto lowest = static_cast<uint32_t>(SPI.transfer(0x00));
  auto lower = static_cast<uint32_t>(SPI.transfer(0x00)) << 8;
  auto higher = static_cast<uint32_t>(SPI.transfer(0x00)) << 16;
  auto highest = static_cast<uint32_t>(SPI.transfer(0x00)) << 24;
  return lowest | lower | higher | highest;
}

uint64_t read64(uint64_t address) {
  CableSelectHolder<csPin> reader;
  sendOpcode(FRAMOpcodes::READ);
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
void enableWrites() {
  CableSelectHolder<csPin> startup;
  // send WREN before any write operation
  sendOpcode(FRAMOpcodes::WREN); 
}
void writeFram(uint32_t address, uint8_t value) { 
  enableWrites();
  CableSelectHolder<csPin> startup;
  // send WREN before any write operation
  sendOpcode(FRAMOpcodes::WRITE);
  transferAddress(address);
  SPI.transfer(value);
}

void writeFram(uint32_t address, uint16_t value) {
  enableWrites();
  CableSelectHolder<csPin> startup;
  sendOpcode(FRAMOpcodes::WRITE);
  transferAddress(address << 1);
  SPI.transfer(uint8_t(value));
  SPI.transfer(uint8_t(value >> 8));
}

void writeFram(uint32_t address, uint32_t value) {
  enableWrites();
  CableSelectHolder<csPin> startup;
  sendOpcode(FRAMOpcodes::WRITE);
  transferAddress(address << 1);
  SPI.transfer(uint8_t(value));
  SPI.transfer(uint8_t(value >> 8));
  SPI.transfer(uint8_t(value >> 16));
  SPI.transfer(uint8_t(value >> 24));
}

void writeFram(uint32_t address, uint64_t value) {
  enableWrites();
  CableSelectHolder<csPin> startup;
  sendOpcode(FRAMOpcodes::WRITE);
  transferAddress(address << 1);
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

void setup(void) {   

  pinMode(csPin, OUTPUT);
  SPI.begin();
  display.begin(SSD1306_SWITCHCAPVCC, 0x3D);  // initialize with the I2C addr 0x3D (for the 128x64)
  display.display();
  delay(2000);
  display.clearDisplay();
  for (uint32_t i = 0; i < max64BitAddress; ++i) {
    display.clearDisplay();
    display.setTextSize(1);
    display.setTextColor(WHITE);
    display.setCursor(0,0);
    display.print("Address: ");    
    display.println(i, HEX);
    display.print("Value: ");
    auto value = read64(i);
    display.print(static_cast<uint32_t>(value >> 32), HEX);
    display.println(static_cast<uint32_t>(value), HEX);
    
    display.display();
    delay(10);
  }
}
 
void loop() {

}
