
all: operations.clp

operations.clp: common.clp generator.clp reset_run_exit.clp
	@echo Making operations.clp
	@maya -f2 generator.clp -f2 reset_run_exit.clp > operations.clp

clean: 
	@echo Cleaning
	@rm -f operations.clp

.PHONY: shell all

knowledge_desc.clp: common.clp
parse_knowledge_desc.clp: common.clp knowledge_desc.clp
build_instruction_functions.clp: common.clp
build_instruction_description.clp: common.clp 
generator.clp: common.clp build_instruction_functions.clp parse_knowledge_desc.clp build_instruction_description.clp knowledge_desc.clp
