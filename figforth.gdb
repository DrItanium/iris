set args iris.img
catch throw
b iris::Core::init() 
commands
set _enableDebugging=true
c
end
run
