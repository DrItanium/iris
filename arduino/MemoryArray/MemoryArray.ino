#include <libbonuspin.h>

/*
   Used the following components and wire routing:
   (1) Arduino Uno (adafruit metro mini)
   (2) Microchip 23LC1024 * 4
   (3) 10K Resistor * 5
 */
#include <SPI.h>


enum class SRAMOpcodes : uint8_t {
  RDSR = 0x05,
  RDMR = RDSR,
  WRSR = 0x01,
  WRMR = WRSR,
  READ = 0x03,
  WRITE = 0x02,
  EDIO = 0x3B,
  EQIO = 0x38,
  RSTIO = 0xFF,
};
using Word = uint16_t;
using RawAddress = uint32_t;
using IrisAddress = uint16_t;
using Register = uint16_t;
Register registers[256];
//SRAM opcodes
// pins associated with the different spi memory chips
// two spi chips are necessary to hold code space so we have to do some 
// math to figure out which chip to select
constexpr auto CodeSectionLowerHalf = 5;
constexpr auto CodeSectionUpperHalf = 6;
constexpr auto DataSection = 7;
constexpr auto StackSection = 8;
constexpr RawAddress computeWordAddress(IrisAddress addr) noexcept {
    return static_cast<RawAddress>(addr) << 1;
}
constexpr RawAddress computeDoubleWordAddress(IrisAddress addr) noexcept {
    return static_cast<RawAddress>(addr) << 2;
}
constexpr int getCodePin(IrisAddress addr) noexcept {
  return (addr & 0x8000) ? CodeSectionUpperHalf : CodeSectionLowerHalf;  
}

constexpr RawAddress localCodeAddress(IrisAddress addr) noexcept {
  // strip the most significant digit of the address out and then shift by two
  return computeDoubleWordAddress(addr & 0x7FFF);  
}
template<int pin>
using CableSelectHolder = bonuspin::DigitalPinHolder<pin, LOW, HIGH>;
void sendOpcode(SRAMOpcodes opcode) {
  SPI.transfer(uint8_t(opcode));
}
void transferAddress(uint32_t address) {
  SPI.transfer(uint8_t(address >> 16));
  SPI.transfer(uint8_t(address >> 8));
  SPI.transfer(uint8_t(address));
}


template<int pin>
uint8_t read8(uint32_t address) {
  CableSelectHolder<pin> holder;
  sendOpcode(SRAMOpcodes::READ);
  transferAddress(address);
  return SPI.transfer(0x00);
}
template<int pin>
void write8(uint32_t address, uint8_t value) {
  CableSelectHolder<pin> holder;
  sendOpcode(SRAMOpcodes::WRITE);
  transferAddress(address);
  SPI.transfer(value);
}
template<int pin>
void writeUint16(uint32_t address, uint16_t value) {
  auto actualAddress = address << 1;
  write8<pin>(actualAddress, uint8_t(value));
  write8<pin>(actualAddress+1, uint8_t(value >> 8));
}
template<int pin>
uint16_t readUint16(uint32_t address) {
  auto actualAddress = address << 1;
  uint16_t lower = read8<pin>(actualAddress);
  uint16_t upper = read8<pin>(actualAddress+1);
  return lower | (upper << 8);
}
template<int pin>
uint32_t readUint32(uint32_t address) {
  auto actualAddress = address << 1;
  uint32_t lower = readUint16<pin>(actualAddress);
  uint32_t upper = readUint16<pin>(actualAddress+1);
  return lower | (upper << 16);    
}
template<int pin>
void writeUint32(uint32_t address, uint32_t value) {
  auto actualAddress = address << 1;
  writeUint16<pin>(actualAddress, uint16_t(value));
  writeUint16<pin>(actualAddress+1, uint16_t(value >> 16));
}


template<int pin>
void performTests() {
  Serial.print("Testing pin: ");
  Serial.println(pin);
  Serial.println("Performing 16-bit writes, errors will be displayed!");
  for (uint32_t i=0; i< 0x10000; ++i) {    
    auto value = uint16_t(i);
    uint32_t address = i;
    writeUint16<pin>(address, value);
    auto readValue = readUint16<pin>(address);
    if (readValue != value) {
      Serial.print("Going to write '0x");
      Serial.print(value,HEX);
      Serial.print("' to addresses '0x");
      Serial.print(address << 1, HEX);
      Serial.print("' and '0x");
      Serial.print((address << 1) + 1, HEX);
      Serial.print("' - Read back: '0x");
      Serial.print(readValue, HEX);
      Serial.println("' - MISMATCH!"); 
      return;
    }
  }
  Serial.println("Performing 32-bit writes, errors will be displayed!");
  for (uint32_t i=0; i< (0x10000 >> 1); ++i) {    
    auto value = uint32_t(i);
    uint32_t address = i;
    writeUint32<pin>(address, value);
    auto readValue = readUint32<pin>(address);
    if (readValue != value) {
      Serial.print("Going to write '0x");
      Serial.print(value,HEX);
      Serial.print("' to addresses '0x");
      Serial.print(address << 2, HEX);
      Serial.print("', '0x");
      Serial.print((address << 2) + 1, HEX);
      Serial.print("', '0x");
      Serial.print((address << 2) + 2, HEX);
      Serial.print("', and '0x");
      Serial.print((address << 2) + 3, HEX);      
      Serial.print("' - Read back: '0x");
      Serial.print(readValue, HEX);
      Serial.println("' - MISMATCH!"); 
      return;
    }
  }
  Serial.print("Done testing pin ");
  Serial.println(pin);
}

void wakeDevice(int pin) {
  digitalWrite(pin, LOW);
}
void setup(void) { 
  Serial.begin(115200);
  pinMode(CodeSectionLowerHalf, OUTPUT);
  pinMode(CodeSectionUpperHalf, OUTPUT);
  pinMode(DataSection, OUTPUT);
  pinMode(StackSection, OUTPUT);
  SPI.begin();
  for (int i = 0; i < 256; ++i) {
    registers[i] = i;
  }
  performTests<CodeSectionLowerHalf>();
 // performTests<PinCS1>();

  performTests<CodeSectionUpperHalf>();
  performTests<DataSection>();
  performTests<StackSection>();  
}
 
void loop() {
  for (int i =0 ;i < 256; ++i) {
    for (int j = 0; j < 256; ++j) {
      for (int k = 0; k < 256; ++k) {
          registers[i] = registers[j] + registers[k];
      }
    }
  }
}
