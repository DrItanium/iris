#!/bin/bash

make operations.clp
rlwrap maya -f2 common.clp -f2 knowledge_desc.clp -f2 operations.clp
