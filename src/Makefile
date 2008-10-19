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
EMULATOR=beam
EBIN=$(ROOT)/ebin
INCLUDE=$(ROOT)/include
SCRIPTS=$(ROOT)/scripts

SOURCES = $(wildcard *.erl)
HEADERS = $(wildcard $(INCLUDE)/include/*.hrl)
OBJECTS = $(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

APP=$(EBIN)/ermlia.app
ELOG=$(EBIN)/elog.config
START_SH=$(SCRIPTS)/start

COMPILE_FLAGS += \
	+warn_unused_vars +nowarn_shadow_vars +warn_unused_import

ifndef debug_info
	COMPILE_FLAGS += +debug_info
endif

$(EBIN)/%.$(EMULATOR): %.erl
	erlc -pa $(EBIN) -W $(COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

all: $(OBJECTS) $(APP) $(ELOG) $(START_SH)

$(OBJECTS): $(HEADERS)

$(APP): ermlia.app.src $(ROOT)/Makefile
	sed -e 's;%VSN%;$(ERMLIA_VSN);' $< > $@

$(ELOG): elog.config.src $(ROOT)/Makefile
	sed -e 's;%LOGS_DIR%;$(ROOT)/log;' $< > $@

$(START_SH): start.src
	sed -e 's;%ROOT_DIR%;$(ROOT);' $< > $@
	chmod 755 $@

dialyze:
	dialyzer --succ_typings -c ${SOURCES}

clean:	
	rm -rf $(EBIN)/*.$(EMULATOR) $(APP) $(ELOG) $(EBIN)/erl_crash.dump


