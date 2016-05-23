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
PROJECT_VERSION = 1.3.0

DEPS = \
	getopt \
	cereal \
	cowboy \
	jsx

dep_cereal = git https://github.com/joewilliams/cereal.git 5933f1c
dep_jsx_commit = v2.8.0

include erlang.mk

OTP_SRC=esl-erlang_17.1-2~debian~wheezy_amd64.deb
OTP_BASE_URL=https://packages.erlang-solutions.com/debian/pool
OTP_ARCHIVE=docker/$(OTP_SRC)

ERLC_MIB_OPTS = +'{group_check, false}' +no_defs

$(PROJECT).d:: build-www

build-www:
	cd priv/www && bower update

clean:: clean-release

clean-release:
	-rm -rf $(PROJECT)-*

pdf:
	pandoc README.md -o README.pdf

RELDIR=$(PROJECT)-$(PROJECT_VERSION)
RELBIN=$(PROJECT)_$(PROJECT_VERSION).bin
dist:
	git archive --prefix=$(RELDIR)/ HEAD . | gzip -c - > $(RELDIR).source.tar.gz

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
	cp ../bkfw_$(PROJECT_VERSION)_all.deb $(RELDIR)/$(RELBIN)
	cp README.md $(RELDIR)/README.md
	cp CHANGES.md $(RELDIR)/CHANGES.md
	mkdir -p $(RELDIR)/mibs
	cp mibs/*.mib $(RELDIR)/mibs
	cp /usr/share/mibs/ietf/SNMPv2-MIB $(RELDIR)/mibs
	zip -r $(RELDIR).zip $(RELDIR)
	-test -n "$(ORIG_UID)" && chown -R $(ORIG_UID).$(ORIG_GRP) .

.PHONY: pdf dist docker-release docker-image release clean-release build-www
