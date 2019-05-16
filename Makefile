

operations.clp: common.clp generator.clp knowledge_desc.clp reset_run_exit.clp
	@echo Making operations.clp
	@maya -f2 common.clp -f2 generator.clp -f2 knowledge_desc.clp -f2 reset_run_exit.clp > operations.clp

clean: 
	@echo Cleaning
	@rm -f operations.clp

.PHONY: shell
