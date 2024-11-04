# Makefile for running the neilyio.metal-detectors.server project

# Default port, which can be overridden when calling make
PORT ?= 5000

# Run the server with the default or specified port
run-server:
	clj -M:run-server $(PORT)

# Example of how to stop the server if you add functionality later
stop-server:
	@echo "Stopping server... (implement if necessary)"

# Shortcut to run tests if you have them configured in deps.edn
test:
	clj -M:test

test-data:
	python send_test_data.py

# Clean up any build artifacts (adjust if necessary)
clean:
	rm -rf target

# Help message to show all make commands
help:
	@echo "Available commands:"
	@echo "  make run-server [PORT=<port>]   - Start the server on the specified port (default: 5000)"
	@echo "  make stop-server                - Stop the server (implement if necessary)"
	@echo "  make test-data                  - Send random test to the UDP server"
	@echo "  make test                       - Run tests"
	@echo "  make clean                      - Clean up build artifacts"
	@echo "  make help                       - Show this help message"

.PHONY: run-server stop-server test clean help
