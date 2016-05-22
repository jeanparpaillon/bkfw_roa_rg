# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

OTP_SRC=esl-erlang_17.1-2~debian~wheezy_amd64.deb
OTP_BASE_URL=https://packages.erlang-solutions.com/debian/pool
OTP_ARCHIVE=docker/$(OTP_SRC)

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar || echo ./rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  update-deps clean-common-test-data rebuild install dist docker-release \
  docker-image release

all: deps compile test

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

install:
	$(REBAR) generate
doc:
	$(REBAR) skip_deps=true doc

eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit

test: compile eunit

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src

shell: deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

pdf:
	pandoc README.md -o README.pdf

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	#- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile escript dialyzer test

RELNAME=bkfw
RELVERSION=$(shell ./version.sh)
RELDIR=$(RELNAME)-$(RELVERSION)
RELBIN=$(RELNAME)_$(RELVERSION).bin
dist:
	git archive --prefix=$(RELNAME)-$(RELVERSION)/ HEAD . | gzip -c - > $(RELNAME)-$(RELVERSION).source.tar.gz

IMAGE_ID=bkfw-build

docker-release: docker-image
	docker run -v `pwd`:/mnt $(IMAGE_ID) make -C /mnt release ORIG_UID=`id -u` ORIG_GRP=`id -g`

docker-image: $(OTP_ARCHIVE)
	@if ! $$(docker images | grep -q ^$(IMAGE_ID)); then \
	  echo "Building $(IMAGE_ID) docker image"; \
	  docker build --tag=$(IMAGE_ID) docker; \
	else \
	  echo "$(IMAGE_ID) docker image up-to-date"; \
	fi

$(OTP_ARCHIVE):
	curl --progress-bar  $(OTP_BASE_URL)/$(OTP_SRC) > $@


release:
	rm -rf $(RELDIR)
	rm -f $(RELDIR).zip
	mkdir -p $(RELDIR)
	dpkg-buildpackage -b
	cp ../bkfw_$(RELVERSION)_all.deb $(RELDIR)/$(RELBIN)
	cp README.md $(RELDIR)/README.md
	cp CHANGES.md $(RELDIR)/CHANGES.md
	mkdir -p $(RELDIR)/mibs
	cp mibs/*.mib $(RELDIR)/mibs
	cp /usr/share/mibs/ietf/SNMPv2-MIB $(RELDIR)/mibs
	zip -r $(RELDIR).zip $(RELDIR)
	-test -n "$(ORIG_UID)" && chown -R $(ORIG_UID).$(ORIG_GRP) .
