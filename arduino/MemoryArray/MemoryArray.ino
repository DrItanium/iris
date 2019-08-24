 #include <SPI.h>
#include <libbitmanip.h>
#include <libbonuspin.h>
// use the mega 2560
using ShiftRegisterLEDArray = bonuspin::SN74HC595<10, 9, A0>;
ShiftRegisterLEDArray ledArray;


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
using DoubleWord = uint32_t;
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
constexpr auto delayAmount = 30;


bool serialActive = false;
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


void writeToCodeSection(IrisAddress address, DoubleWord value) {
    auto localAddress = localCodeAddress(address);
    if (getCodePin(address) == CodeSectionLowerHalf) {
      writeUint32<CodeSectionLowerHalf>(localAddress, value);
    } else {
      writeUint32<CodeSectionUpperHalf>(localAddress, value);
    }
}
DoubleWord readFromCodeSection(IrisAddress address) {
  auto localAddress = localCodeAddress(address);
  if (getCodePin(address) == CodeSectionLowerHalf) {
    return readUint32<CodeSectionLowerHalf>(localAddress);
  } else {
    return readUint32<CodeSectionUpperHalf>(localAddress);
  }
}

void writeToDataSection(IrisAddress address, Word value) {
  writeUint16<DataSection>(address, value);
}
Word readFromDataSection(IrisAddress address) {
  return readUint16<DataSection>(address);
}
void writeToStackSection(IrisAddress address, Word value) {
  writeUint16<StackSection>(address, value);
}
Word readFromStackSection(IrisAddress address) {
  return readUint16<StackSection>(address);
}

void pushValue(Register& sp, Word value) {
  ++sp;
  writeToStackSection(sp, value);
}
Word popValue(Register& sp) {
  auto value = readFromStackSection(sp);
  --sp;
  return value;
}

// arduino preprocessing cocks this signature up if it is not on a single line...
template<typename R, typename W, typename T> void performStorageTests(R rf, W wf, T mask) {
  if (serialActive) {
   Serial.println("\tWrite to the entire contents of memory!");
  }
   for (RawAddress addr = 0; addr < 0x10000; ++addr) {
      IrisAddress localAddress = addr;
      T value = T(addr) | mask;
      wf(localAddress, value);
      auto readback = rf(localAddress);
      ledArray << readback;
      if (value != readback) {
        if (serialActive) {
          Serial.print("FAIL: Wrote '0x");
          Serial.print(value, HEX);
          Serial.print("' to '0x");
          Serial.print(localAddress, HEX);
          Serial.print("' and read back '0x");
          Serial.println(readback, HEX);     
        }   
      }    
  }
  if (serialActive) {
    Serial.println("\tDone");
  }
}

void performCodeStorageTests() {
  if (serialActive)
  Serial.println("Performing code storage tests, this will be noisy!");
  performStorageTests(readFromCodeSection, writeToCodeSection, DoubleWord(random(0x99, 0x12345678)));  
  if (serialActive)
  Serial.println("Done with code write test!");
}

void performDataStorageTests() {
  if (serialActive) {
  Serial.println("Performing data storage tests, this could be noisy!");
  }
  performStorageTests(readFromDataSection, writeToDataSection, Word(random(0x99, 0x1234)));  
  if (serialActive)
  Serial.println("Done with data storage tests");
}


void performStackStorageTests() {
  if (serialActive) 
  Serial.println("Performing stack storage tests, this could be noisy!");
  performStorageTests(readFromStackSection, writeToStackSection, Word(random(0x99, 0x1234)));  
  if (serialActive) {
  Register& sp = registers[255];
  pushValue(sp, 0xFDED);
  if (popValue(sp) != 0xFDED) {
    Serial.println("\tPush and pop stack test fail!");
  } else {
    Serial.println("\tPush and pop stack test success!");
  }
  Serial.println("Done with stack storage tests");
  }
}

void setupCodeMemory() {
  pinMode(CodeSectionLowerHalf, OUTPUT);
  pinMode(CodeSectionUpperHalf, OUTPUT);
}
void setupStackMemory() {
  pinMode(StackSection, OUTPUT);
}
void setupDataMemory() {
  pinMode(DataSection, OUTPUT);
}


void wakeDevice(int pin) {
  digitalWrite(pin, LOW);
}
void setup(void) { 
  Serial.begin(115200);
  serialActive = true;
  ledArray << uint16_t(0);
  
   for (uint16_t numberToDisplay = 1; numberToDisplay < 0x4000; numberToDisplay <<= 1) {
    // take the latchPin low so 
    // the LEDs don't change while you're sending in bits:    
    ledArray << numberToDisplay;    
    delay(delayAmount);    
  }
  for (uint32_t numberToDisplay = 0x4000; numberToDisplay != 0; numberToDisplay >>= 1) {
    // take the latchPin low so 
    // the LEDs don't change while you're sending in bits:    
    ledArray << numberToDisplay;    
    delay(delayAmount);    
  }
  setupCodeMemory();
  setupDataMemory();
  setupStackMemory();
  SPI.begin();
  for (int i = 0; i < 256; ++i) {
    registers[i] = i;
  }
  performCodeStorageTests();
  performDataStorageTests();
  performStackStorageTests();
}
 
void loop() {

}
