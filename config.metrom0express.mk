# extracted from adafruit arduino package files
CXX := arm-unknown-linux-gnueabi-g++
LD := ${CXX}
MCU_KIND := cortex-m0plus
ARM_FLAGS := -mcpu=${MCU_KIND} \
	-mthumb \
	-D__SAMD21G18A__ \
	-DARDUINO_SAMD_ZERO \
	-DARM_MATH_CM0PLUS \
	-DADAFRUIT_METRO_M0_EXPRESS
FFLAGS := -ffunction-sections \
	-fdata-sections \
	-fno-threadsafe-statics \
	-nostdlib \
	--param max-inline-insns-single=500 \
	-fno-rtti \
	-fno-exceptions

LD_FLAGS := -Wl,--cref \
	-Wl,--check-sections \
	-Wl,--gc-sections \
	-Wl,--unresolved-symbols=report-all \
	-Wl,--warn-common \
	-Wl,--warn-section-align
ARDUINO_ROOT := ${HOME}/.arduino15/
ADAFRUIT_VERSION := 1.5.7
ATMEL_CMSIS_VERSION := 1.2.0
CMSIS_ATMEL_SAMD_ROOT := ${ARDUINO_ROOT}/packages/arduino/tools/CMSIS-Atmel/${ATMEL_CMSIS_VERSION}
ADAFRUIT_SAMD_ROOT := ${ARDUINO_ROOT}/packages/adafruit/hardware/samd/${ADAFRUIT_VERSION}
INCLUDES := -I${ADAFRUIT_SAMD_ROOT}/variants/metro_m0 \
			-I${ADAFRUIT_SAMD_ROOT}/cores/arduino \
			-I${CMSIS_ATMEL_SAMD_ROOT}/CMSIS/Device/ATMEL

GENFLAGS := -Wall -Wextra -std=gnu++17 ${ARM_FLAGS}
OPTIMIZATION_FLAGS := -Os
CXXFLAGS := ${GENFLAGS} ${OPTIMIZATION_FLAGS} ${DEBUGGING_FLAGS} ${INCLUDES}
LDFLAGS := ${LIBS} ${OPTIMIZATION_FLAGS} ${ARM_FLAGS}
