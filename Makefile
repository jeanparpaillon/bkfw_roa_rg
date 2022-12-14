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
PROJECT = bkfw
PROJECT_VERSION = 1.6.1

DEPS = \
	getopt \
	cereal \
	cowboy \
	jsx

dep_getopt = git https://github.com/jcomellas/getopt.git 838e67f
dep_cereal = git https://github.com/joewilliams/cereal.git 5933f1c
dep_jsx_commit = v2.8.0
dep_erlang_ale = git https://github.com/esl/erlang_ale.git v0.1.0
dep_lcd_app = git https://github.com/jeanparpaillon/erlang_pi_lcd.git next

include erlang.mk

OTP_SRC=esl-erlang_17.5.3-1~debian~jessie_amd64.deb
OTP_BASE_URL=https://packages.erlang-solutions.com/debian/pool
OTP_ARCHIVE=docker/$(OTP_SRC)

RELX_CONFIG = $(CURDIR)/rel/prod/relx.config

ERLC_MIB_OPTS = +'{group_check, false}' +no_defs

rel-dev:
	-rm -rf _rel/bkfw
	$(MAKE) RELX_CONFIG=$(CURDIR)/rel/dev/relx.config RELX_OPTS=-d

$(PROJECT).d:: 

assets: priv/.stamp-www

priv/.stamp-www: # priv/www/bower.json
	cd priv/www && bower update
	touch $@

clean:: clean-release

clean-release:
	-rm -rf $(PROJECT)-*

pdf:
	pandoc README.md -o README.pdf

RELDIR=$(PROJECT)-$(PROJECT_VERSION)
RELBIN=$(PROJECT)_$(PROJECT_VERSION).bin
RELSRC=$(RELDIR).source.tar.gz

docker-release: docker-image
	docker run -v $(HOME):$(HOME) -v /etc/resolv.conf:/etc/resolv.conf \
	  --user=$(USER) \
	  $(IMAGE_ID) make -C $(PWD) release

docker-image: # $(OTP_ARCHIVE)
	@if ! $$(docker images | grep -q ^$(IMAGE_ID)); then \
	  echo "Building $(IMAGE_ID) docker image"; \
	  docker build --tag=$(IMAGE_ID) docker; \
	else \
	  echo "$(IMAGE_ID) docker image up-to-date"; \
	fi

$(OTP_ARCHIVE):
	wget -O $@ $(OTP_BASE_URL)/$(OTP_SRC)

.PHONY: pdf dist docker-release docker-image release clean-release build-www rel-dev
