 #include <SPI.h>
#include <libbonuspin.h>

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

constexpr auto DataSection = 7;

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

void writeToDataSection(IrisAddress address, Word value) {
  writeUint16<DataSection>(address, value);
}
Word readFromDataSection(IrisAddress address) {
  return readUint16<DataSection>(address);
}

// arduino preprocessing cocks this signature up if it is not on a single line...
template<typename R, typename W, typename T> void performStorageTests(R rf, W wf, T mask) {
   for (RawAddress addr = 0; addr < 0x10000; ++addr) {
      IrisAddress localAddress = addr;
      T value = T(addr) | mask;
      wf(localAddress, value);      
      if (value != rf(localAddress)) {
          Serial.print("![0x");
          Serial.print(localAddress, HEX);
          Serial.print("] = 0x");
          Serial.println(value, HEX);          
      }    
  }
}

void setup(void) { 
  Serial.begin(115200);
  pinMode(DataSection, OUTPUT); 
  SPI.begin();
  Serial.println("Tests");
  performStorageTests(readFromDataSection, writeToDataSection, Word(0xFFFF));  
  Serial.println("Done");  
}
 
void loop() {

}
