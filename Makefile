BUILD_DIR=/var/tmp/$(shell whoami)
NODE_MODULES=$(BUILD_DIR)/node_modules
NODE=$(BUILD_DIR)/node/bin/node
BSB=$(NODE_MODULES)/bs-platform
export PATH := $(BUILD_DIR)/node/bin:$(PATH)

.PHONY: all, clean
.SECONDARY: $(NODE), node_modules/

all: node_modules/
	@npm run build
	@echo -e "\nDone. After testing the program, consider calling \e[36;1mmake clean\e[0m to remove the dependencies that were temporarily downloaded to $(BUILD_DIR)."

node_modules/: $(NODE)
	@mkdir -p $(NODE_MODULES)
	@ln -sf $(NODE_MODULES) node_modules
	@npm cache clean --force 2>/dev/null
	@echo -e "\e[32mDownloading BuckleScript 2.1.0\e[0m"
	@npm install

$(NODE):
	@echo -e "\e[32mDownloading NodeJS v11.7.0\e[0m"
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR) && \
	  wget -O node.tar.gz http://nodejs.org/dist/v11.7.0/node-v11.7.0-linux-x64.tar.gz 2>/dev/null && \
	  tar -xf node.tar.gz && \
	  mv node-v11.7.0-linux-x64 node && \
	  rm node.tar.gz

clean:
	@rm -rf ~/.npm # delete npm local cache
	@rm -rf $(BUILD_DIR)
	@rm -rf lib/
	@rm -f node_modules
	@rm -f src/*.bs.js

sharfile:
	@rm -rf lib/
	@rm -f node_modules
	@rm -f src/*.bs.js
	@shar * > sharfile