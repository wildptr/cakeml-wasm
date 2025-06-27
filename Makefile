CAKE = ~/cake-x64-64/cake_no_rawcall

%.wasm: %.wat
	wat2wasm --enable-tail-call -o $@ $<

%.o: %.asm
	nasm -f elf64 -o $@ $<

%.sexp: %.cml
	$(CAKE) --explore --jump=false < $< > $@

%.sexp: %.cml_sexp
	CML_HEAP_SIZE=4000 $(CAKE) --explore --jump=false --sexp=true --skip_type_inference=true < $< > $@

%.S: %.cml
	$(CAKE) --jump=false < $< > $@

%.S: %.cml_sexp
	$(CAKE) --jump=false --sexp=true --skip_type_inference=true < $< > $@

%.stackLang.sexp: %.sexp
	./extract-stackLang.pl $< > $@

%.labLang.sexp: %.sexp
	./extract-labLang.pl $< > $@

%.nodata.wat: %.stackLang.sexp
	dune exe ./main.exe -- -wasm-tail-call $< > $@

%.notail.nodata.wat: %.stackLang.sexp
	dune exe ./main.exe -- $< > $@

%.wat: %.nodata.wat %.S
	./extract-data.pl $^ > $@

%.notail.wat: %.notail.nodata.wat %.S
	./extract-data.pl $^ > $@
