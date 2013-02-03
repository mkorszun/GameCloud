.PHONY: 

all:
	./rebar get-deps compile

clean:
	./rebar clean
	rm -rf apps/*/deps
	rm -rf deps/*/deps
	rm -rf apps/*/doc/*.html
	rm -rf apps/*/doc/*.png
	rm -rf apps/*/doc/*.css
	rm -rf apps/*/doc/edoc-info
	rm -rf deps/*/doc/*html
	rm -rf apps/*/logs
	rm -rf deps/*/logs
	find . -name "*~" -exec rm {} \;
	find . -name ".#*" -exec rm {} \;
	find . -name "erl_crash.dump" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

release:
	make clean
	make
	./rebar generate overlay_vars=${NODE}.config  

test:
	ct_run -pa apps/*/ebin -pa deps/*/ebin -dir apps/*/test/ -logdir tests 
