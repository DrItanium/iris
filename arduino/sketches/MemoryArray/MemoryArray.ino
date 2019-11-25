 #include <SPI.h>
#include <libbitmanip.h>
#include <libbonuspin.h>

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

namespace HC138 {
constexpr auto EnablePin = 8;
constexpr auto SelA = 7;
constexpr auto SelB = 6;
constexpr auto SelC = 5;
using Enabler = CableSelectHolder<EnablePin>;
void setup() {
  pinMode(SelA, OUTPUT);
  pinMode(SelB, OUTPUT);
  pinMode(SelC, OUTPUT);
  pinMode(EnablePin, OUTPUT);
  
  digitalWrite(SelA, HIGH);
  digitalWrite(SelB, HIGH);
  digitalWrite(SelC, HIGH);
  digitalWrite(EnablePin, HIGH);
}
void generateSignal(int a, int b, int c) {    
    Enabler e;
    digitalWrite(SelA, a);
    digitalWrite(SelB, b);
    digitalWrite(SelC, c);
}

template<int i>
void writeSignal() {

  switch (i) {
    case 0: 
      generateSignal(LOW, LOW, LOW);
      break;
    case 1: 
      generateSignal(HIGH, LOW, LOW);
      break;
    case 2: 
      generateSignal(LOW, HIGH, LOW);
      break;
    case 3: 
      generateSignal(HIGH, HIGH, LOW);
      break;
    case 4: 
      generateSignal(LOW, LOW, HIGH);
      break;
    case 5: 
      generateSignal(HIGH, LOW, HIGH);
      break;
    case 6: 
      generateSignal(LOW, HIGH, HIGH);
      break;
    case 7: 
      generateSignal(HIGH, HIGH, HIGH);
      break;
    default:
      writeSignal<i & 0x7>();
      break;
  }
}

}


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
  HC138::writeSignal<memoryCell>();
  sendOpcode(SRAMOpcodes::READ);
  transferAddress(address);
  return SPI.transfer(0x00);
}
template<int memoryCell>
void write8(uint32_t address, uint8_t value) {
  HC138::writeSignal<memoryCell>();
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
template<typename R, typename W, typename T> void performStorageTests(R rf, W wf, T mask) {
   Serial.println("\tWrite to the entire contents of memory!");
   for (RawAddress addr = 0; addr < 0x10000; ++addr) {
      IrisAddress localAddress = addr;
      T value = T(addr) | mask;
      wf(localAddress, value);
      auto readback = rf(localAddress);
      if (value != readback) {
          Serial.print("FAIL: Wrote '0x");
          Serial.print(value, HEX);
          Serial.print("' to '0x");
          Serial.print(localAddress, HEX);
          Serial.print("' and read back '0x");
          Serial.println(readback, HEX);              
      }    
  }
  Serial.println("\tDone");
}

void performCodeStorageTests() {
  Serial.println("Performing code storage tests, this will be noisy!");
  performStorageTests(readFromCodeSection, writeToCodeSection, DoubleWord(random(0x99, 0x12345678)));  
  Serial.println("Done with code write test!");
}

void performDataStorageTests() {
  Serial.println("Performing data storage tests, this could be noisy!");
  performStorageTests(readFromDataSection, writeToDataSection, Word(random(0x99, 0x1234)));  
  Serial.println("Done with data storage tests");
}


void performStackStorageTests() {
  Serial.println("Performing stack storage tests, this could be noisy!");
  performStorageTests(readFromStackSection, writeToStackSection, Word(random(0x99, 0x1234)));  
  Register& sp = registers[255];
  pushValue(sp, 0xFDED);
  if (popValue(sp) != 0xFDED) {
    Serial.println("\tPush and pop stack test fail!");
  } else {
    Serial.println("\tPush and pop stack test success!");
  }
  Serial.println("Done with stack storage tests");
}

void setup(void) { 
  Serial.begin(115200);
  
  HC138::setup();
  SPI.begin();
  setupRegisters();
  performCodeStorageTests();
  performDataStorageTests();
  performStackStorageTests();
}
 
void loop() {

}
