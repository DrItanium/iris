// adding a Adafruit Sharp Display
#include <SPI.h>
#include <libbitmanip.h>
#include <libbonuspin.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SharpMem.h>

Adafruit_SharpMem display(13, 11, 10, 144, 168);
template<int pin>
using CableSelectHolder = bonuspin::DigitalPinHolder<pin, LOW, HIGH>;

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



class Register {
public:
  explicit constexpr Register(Word initialValue = 0) : _contents(initialValue) { }
  ~Register() = default;
  constexpr Word getContents() const noexcept { return _contents; }
  void setContents(Word value) noexcept { _contents = value; }
  Register& operator=(Word value) {
    _contents = value;
    return *this;
  }
  Register& operator++() {
    ++_contents;
    return *this;
  }
  Register& operator--() {
    --_contents;
    return *this;
  }
  constexpr Register operator~() const noexcept { return Register(~getContents()); }
  constexpr Register operator+(const Register& b) const noexcept { return Register(getContents() + b.getContents()); }
  constexpr Register operator-(const Register& b) const noexcept { return Register(getContents() - b.getContents()); }
  constexpr Register operator*(const Register& b) const noexcept { return Register(getContents() * b.getContents()); }
  constexpr Register operator/(const Register& b) const noexcept { return Register(getContents() / b.getContents()); }
  constexpr Register operator%(const Register& b) const noexcept { return Register(getContents() % b.getContents()); }
  constexpr Register operator&(const Register& b) const noexcept { return Register(getContents() & b.getContents()); }
  constexpr Register operator|(const Register& b) const noexcept { return Register(getContents() | b.getContents()); }
  constexpr bool operator==(const Register& b) const noexcept { return getContents() == b.getContents(); }
  constexpr bool operator!=(const Register& b) const noexcept { return getContents() != b.getContents(); }
  constexpr bool operator<(const Register& b) const noexcept { return getContents() < b.getContents(); }
  constexpr bool operator<=(const Register& b) const noexcept { return getContents() <= b.getContents(); }
  constexpr bool operator>(const Register& b) const noexcept { return getContents() > b.getContents(); }
  constexpr bool operator>=(const Register& b) const noexcept { return getContents() >= b.getContents(); }
  explicit constexpr operator Word() const noexcept { return getContents(); }
private:
  Word _contents;
    
};
constexpr auto numRegisters = 256;
Register registers[numRegisters];
Register ip;

void setupRegisters() {
  ip = 0;
  for (auto i = 0; i < numRegisters; ++i) {
    registers[i] = 0;
  }
}
//SRAM opcodes
// pins associated with the different spi memory chips
// two spi chips are necessary to hold code space so we have to do some 
// math to figure out which chip to select
constexpr auto CodeSectionLowerHalf = 0;
constexpr auto CodeSectionUpperHalf = 1;
constexpr auto DataSection = 2;
constexpr auto StackSection = 3;
constexpr auto delayAmount = 30;

constexpr auto HC138_Enable = 8;
constexpr auto HC138_SelA = 7;
constexpr auto HC138_SelB = 6;
constexpr auto HC138_SelC = 5;

bonuspin::HC138<HC138_SelA, HC138_SelB, HC138_SelC, HC138_Enable> demux;


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


void sendOpcode(SRAMOpcodes opcode) {
  SPI.transfer(uint8_t(opcode));
}
void transferAddress(uint32_t address) {
  SPI.transfer(uint8_t(address >> 16));
  SPI.transfer(uint8_t(address >> 8));
  SPI.transfer(uint8_t(address));
}


template<int memoryCell>
uint8_t read8(uint32_t address) {
  demux.enableLine(memoryCell);
  sendOpcode(SRAMOpcodes::READ);
  transferAddress(address);
  return SPI.transfer(0x00);
}
template<int memoryCell>
void write8(uint32_t address, uint8_t value) {
  demux.enableLine(memoryCell);  
  sendOpcode(SRAMOpcodes::WRITE);
  transferAddress(address);
  SPI.transfer(value);
}
template<int memoryCell>
void writeUint16(uint32_t address, uint16_t value) {
  auto actualAddress = address << 1;
  write8<memoryCell>(actualAddress, uint8_t(value));
  write8<memoryCell>(actualAddress+1, uint8_t(value >> 8));
}
template<int memoryCell>
uint16_t readUint16(uint32_t address) {
  auto actualAddress = address << 1;
  uint16_t lower = read8<memoryCell>(actualAddress);
  uint16_t upper = read8<memoryCell>(actualAddress+1);
  return lower | (upper << 8);
}
template<int memoryCell>
uint32_t readUint32(uint32_t address) {
  auto actualAddress = address << 1;
  uint32_t lower = readUint16<memoryCell>(actualAddress);
  uint32_t upper = readUint16<memoryCell>(actualAddress+1);
  return lower | (upper << 16);    
}
template<int memoryCell>
void writeUint32(uint32_t address, uint32_t value) {
  auto actualAddress = address << 1;
  writeUint16<memoryCell>(actualAddress, uint16_t(value));
  writeUint16<memoryCell>(actualAddress+1, uint16_t(value >> 16));
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
  writeToStackSection(sp.getContents(), value);
}

Word popValue(Register& sp) {
  auto value = readFromStackSection(sp.getContents());
  --sp;
  return value;
}

// arduino preprocessing cocks this signature up if it is not on a single line...
template<typename R, typename W, typename T> bool performStorageTests(R rf, W wf, T mask) {
   for (RawAddress addr = 0; addr < 0x10000; ++addr) {
      IrisAddress localAddress = addr;
      T value = T(addr) | mask;
      wf(localAddress, value);
      auto readback = rf(localAddress);
      if (value != readback) {
          display.print("FAIL: Wrote '0x");
          display.print(value, HEX);
          display.print("' to '0x");
          display.print(localAddress, HEX);
          display.print("' and read back '0x");
          display.println(readback, HEX);              
          display.refresh();
          delay(500);
          return false;
      }    
  }
  return true;
}

void startTest(const char* what) {
  display.clearDisplay();
  display.setCursor(0, 0);
  display.print("TESTING: ");
  display.println(what);
  display.refresh();
  delay(500);  
}
bool processTestResult(const char* msg, bool result) {
  display.print(msg);
  display.print(" Test Outcome: ");
  if (result) {
    display.println("SUCCESS!");
  } else {
    display.println("FAILURE!");
  }
  display.refresh();
  delay(500);
  return result;
}

bool performCodeStorageTests() {
  startTest("CODE");
  return processTestResult("Code Storage", performStorageTests(readFromCodeSection, writeToCodeSection, DoubleWord(random(0x99, 0x12345678))));  
}

bool performDataStorageTests() {
  startTest("DATA");
  return processTestResult("Data Storage", performStorageTests(readFromDataSection, writeToDataSection, Word(random(0x99, 0x1234))));
}


bool performStackStorageTests() {
  startTest("STACK");
  auto result = performStorageTests(readFromStackSection, writeToStackSection, Word(random(0x99, 0x1234)));
  if (!result) {
    display.println("STORAGE TEST: FAILURE!");
    display.refresh();
    delay(500);
    return false;
  }
  
  Register& sp = registers[255];
  pushValue(sp, 0xFDED);
  if (popValue(sp) != 0xFDED) {
    display.println("\t Register Test: FAILURE");
    display.refresh();
    delay(500);
    return false;
  
  } 
  display.println("SUCCESS!");
  display.refresh();
  delay(500);
  return true;  
}
int minorHalfSize;
constexpr auto BLACK = 0;
constexpr auto WHITE = 1;
void setup(void) { 
  //Serial.begin(115200);
  demux.setupPins();  
  SPI.begin();
  display.begin();
  minorHalfSize = min(display.width(), display.height()) / 2;
    display.clearDisplay();
  for (int i=0; i<minorHalfSize; i+=2) {
    display.drawRect(i, i, display.width()-2*i, display.height()-2*i, BLACK);
    display.refresh();
  }
  display.clearDisplay();
  display.setTextSize(1);
  display.setTextColor(BLACK);
  display.setCursor(0,0);
  setupRegisters();
  auto result = performCodeStorageTests() && performDataStorageTests() && performStackStorageTests();
  display.clearDisplay();

  display.setCursor(0,0);
  display.print("TESTS ");
  if (result) {
    display.print("PASSED");
  } else {
    display.print("FAILED");
  }
  display.println("!!!");
  display.refresh();
  delay(500);
}
 
void loop() {

}
