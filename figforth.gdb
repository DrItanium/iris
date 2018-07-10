set args iris.img
catch throw
b iris::Core::cycle()
commands
set _enableDebugging=true
# printf "xsp: %x\n", _registers[4]._value.address
# printf "xrp: %x\n", _registers[5]._value.address
# printf "xtop: %x\n", _registers[6]._value.address
c
end
run
