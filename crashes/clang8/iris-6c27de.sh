# Crash reproducer for clang version 8.0.0 (tags/RELEASE_800/final)
# Driver args: "--driver-mode=g++" "-std=c++17" "-c" "iris.cc"
# Original command:  "/usr/lib64/llvm/8/bin/clang-8" "-cc1" "-triple" "x86_64-pc-linux-gnu" "-emit-obj" "-mrelax-all" "-disable-free" "-disable-llvm-verifier" "-discard-value-names" "-main-file-name" "iris.cc" "-mrelocation-model" "static" "-mthread-model" "posix" "-mdisable-fp-elim" "-fmath-errno" "-masm-verbose" "-mconstructor-aliases" "-munwind-tables" "-fuse-init-array" "-target-cpu" "x86-64" "-dwarf-column-info" "-debugger-tuning=gdb" "-coverage-notes-file" "/home/jscoggins/dev/iris/iris.gcno" "-resource-dir" "/usr/lib64/llvm/8/bin/../../../../lib/clang/8.0.0" "-internal-isystem" "/usr/lib/gcc/x86_64-pc-linux-gnu/7.3.0/include/g++-v7" "-internal-isystem" "/usr/lib/gcc/x86_64-pc-linux-gnu/7.3.0/include/g++-v7/x86_64-pc-linux-gnu" "-internal-isystem" "/usr/lib/gcc/x86_64-pc-linux-gnu/7.3.0/include/g++-v7/backward" "-internal-isystem" "/usr/local/include" "-internal-isystem" "/usr/lib64/llvm/8/bin/../../../../lib/clang/8.0.0/include" "-internal-externc-isystem" "/usr/include/x86_64-linux-gnu" "-internal-externc-isystem" "/include" "-internal-externc-isystem" "/usr/include" "-std=c++17" "-fdeprecated-macro" "-fdebug-compilation-dir" "/home/jscoggins/dev/iris" "-ferror-limit" "19" "-fmessage-length" "147" "-fobjc-runtime=gcc" "-fcxx-exceptions" "-fexceptions" "-fdiagnostics-show-option" "-fcolor-diagnostics" "-o" "iris.o" "-x" "c++" "iris.cc"
 "/usr/lib64/llvm/8/bin/clang-8" "-cc1" "-triple" "x86_64-pc-linux-gnu" "-emit-obj" "-mrelax-all" "-disable-free" "-disable-llvm-verifier" "-discard-value-names" "-main-file-name" "iris.cc" "-mrelocation-model" "static" "-mthread-model" "posix" "-mdisable-fp-elim" "-fmath-errno" "-masm-verbose" "-mconstructor-aliases" "-munwind-tables" "-fuse-init-array" "-target-cpu" "x86-64" "-dwarf-column-info" "-debugger-tuning=gdb" "-coverage-notes-file" "/home/jscoggins/dev/iris/iris.gcno" "-std=c++17" "-fdeprecated-macro" "-ferror-limit" "19" "-fmessage-length" "147" "-fobjc-runtime=gcc" "-fcxx-exceptions" "-fexceptions" "-fdiagnostics-show-option" "-fcolor-diagnostics" "-x" "c++" "iris-6c27de.cpp"
