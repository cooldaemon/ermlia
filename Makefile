## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations
## under the License.
ERMLIA_VSN=0.0.1

ifndef ROOT
	ROOT=$(shell pwd)
endif

COMMON_TEST=$(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(common_test)]).' -s init stop)
RUN_TEST_CMD=$(COMMON_TEST)/priv/bin/run_test

all: subdirs

subdirs:
	cd src; ERMLIA_VSN=$(ERMLIA_VSN) ROOT=$(ROOT) make
	cd deps/mochiweb/; make

test: test_do

test_compile: subdirs
	cd test; \
		ROOT=$(ROOT) COMMON_TEST=$(COMMON_TEST) make

test_do: test_compile
	${RUN_TEST_CMD} -dir . \
		-logdir test/log -cover test/ermlia.coverspec \
		-I$(ROOT)/include -pa $(ROOT)/ebin

test_single: test_compile
	${RUN_TEST_CMD} -suite $(SUITE) \
		-logdir test/log -cover test/ermlia.coverspec \
		-I$(ROOT)/include -pa $(ROOT)/ebin

docs:
	erl -noshell -run edoc_run application "'ermlia'" \
		'"."' '[{def,{vsn, "$(ERMLIA_VSN)"}}]'

dialyze:
	cd src; make dialyze

clean:	
	rm -rf *.beam erl_crash.dump log/* doc/*
	cd src; ROOT=$(ROOT) make clean
	cd test; ROOT=$(ROOT) make clean
	cd deps/mochiweb/; make clean

