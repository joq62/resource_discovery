all:
	rm -rf  *~ */*~ src/*.beam tests/*.beam;
	rm -rf erl_cra*;
	rm -rf test_log_dir;
	rm -rf tests_ebin;
	rm -rf ebin;
	mkdir tests_ebin;
	erlc -I include -o tests_ebin tests/*.erl;
	mkdir ebin;
	erlc -I include -o ebin src/*.erl;
	rm -rf tests_ebin;
	rm -rf ebin;
	git add *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
clean:
	rm -rf  *~ */*~ src/*.beam tests/*.beam;
	rm -rf erl_cra*;
	rm -rf test_log_dir;
	rm -rf tests_ebin;
	rm -rf ebin;
eunit:
	rm -rf  *~ */*~ src/*.beam tests/*.beam;
	rm -rf erl_cra*;
	rm -rf test_log_dir;
	rm -rf tests_ebin;
	rm -rf ebin;
#	tests
	mkdir tests_ebin;
	erlc -I include -o tests_ebin tests/*.erl;
	cp tests/*.app tests_ebin;
#	dependencies
	erlc -I include -o tests_ebin ../common/src/*.erl;
#	service
	mkdir ebin;
	erlc -I include -o ebin src/*.erl;
	erl -pa ebin -pa tests_ebin\
	    -sname sd_test -run $(m) start -setcookie cookie_test
