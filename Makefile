parser1:
	@ocamlc -c types.ml
	@ocamlc -c backend.ml
	@ocamlyacc parser1.mly
	@ocamlc -c parser1.mli
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c parser1.ml
	@ocamlc -c main.ml
	@ocamlc -o parser1 str.cma types.cmo backend.cmo lexer.cmo parser1.cmo main.cmo
run: parser1
	./parser1 sheet.csv x x input1 
	@echo
	./parser1 sheet.csv x x input
clean:
	@rm types.cmi
	@rm backend.cmi
	@rm types.cmo
	@rm backend.cmo
	@rm lexer.cmi
	@rm lexer.cmo
	@rm lexer.ml
	@rm main.cmi
	@rm main.cmo
	@rm parser1
	@rm parser1.cmi
	@rm parser1.cmo
	@rm parser1.ml
	@rm parser1.mli
