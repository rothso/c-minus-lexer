TMP_DIR=/var/tmp
NODE_MODULES=$(TMP_DIR)/c-compiler-node_modules
NODE=$(TMP_DIR)/node/bin/node
BSB=$(NODE_MODULES)/bs-platform
export PATH := $(TMP_DIR)/node/bin:$(PATH)

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
	@cd $(TMP_DIR) && \
	  wget -O node.tar.gz http://nodejs.org/dist/v11.7.0/node-v11.7.0-linux-x64.tar.gz && \
	  tar -xf node.tar.gz && \
	  mv node-v11.7.0-linux-x64 node && \
	  rm node.tar.gz

clean:
	@rm -rf ~/.npm # delete npm cache
	@rm -rf $(NODE_MODULES)
	@rm -rf $(TMP_DIR)/node
	@rm -f src/*.bs.js

