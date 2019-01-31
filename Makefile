BUILD_DIR=/var/tmp/$(shell whoami)
NODE_MODULES=$(BUILD_DIR)/node_modules
NODE=$(BUILD_DIR)/node/bin/node
BSB=$(NODE_MODULES)/bs-platform
export PATH := $(BUILD_DIR)/node/bin:$(PATH)

all: $(BSB)
	@npm run build

$(BSB): $(NODE)
	@mkdir -p $(NODE_MODULES)
	@ln -sf $(NODE_MODULES) node_modules
	@npm cache clean --force 2>/dev/null
	@echo "Installing dependencies..."
	@npm install

$(NODE):
	@echo "Installing nodejs v11.7.0..."
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR) && \
	  wget -O node.tar.gz http://nodejs.org/dist/v11.7.0/node-v11.7.0-linux-x64.tar.gz 2>/dev/null && \
	  tar -xf node.tar.gz && \
	  mv node-v11.7.0-linux-x64 node && \
	  rm node.tar.gz

clean:
	@rm -rf ~/.npm # delete npm cache
	@rm -rf $(BUILD_DIR)
	@rm -f node_modules
	@rm -f src/*.bs.js

