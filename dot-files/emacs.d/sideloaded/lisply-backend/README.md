# Emacs Lisply Backend - HTTP API for Emacs Lisp Evaluation

Emacs Lisply Backend provides a simple HTTP API that exposes Emacs Lisp evaluation capabilities. It allows external clients to interact with Emacs through standard HTTP requests. This project focuses solely on providing the Emacs backend service that can be containerized and deployed independently.


## Overview

GNU Emacs is a powerful text editor that can run as a daemon (server) and contains an interpreter for the Emacs Lisp language. This implementation exposes Emacs functionality via a simple HTTP API, allowing any client to:

1. Evaluate Emacs Lisp expressions
2. Access and manipulate buffers
3. Read and write files
4. Execute Emacs commands programmatically

This enables external tools, scripts, or services to harness the power of Emacs Lisp for text processing, file manipulation, and other operations in a containerized environment.

## Key Features

- **HTTP Server**: Exposes Emacs functionality through a simple HTTP API
- **JSON Responses**: Returns data in standardized JSON format
- **Lisp Evaluation**: Evaluate arbitrary Emacs Lisp code securely
- **Buffer Access**: List and manipulate Emacs buffers
- **File Operations**: Read from and write to files
- **Containerized**: Runs in an isolated Docker container

## Security Considerations

Because this implementation allows arbitrary Emacs Lisp code to be
evaluated against the running Emacs daemon, best practices are:

- Allow an LLM to connect only to a containerized version of emacs
  (handled automatically by default by [lisp-mcp
  project](https://github.com/gendl/lisp-mcp);
- Make sure not to mount any valuable directories to that container;
- Take steps to [limit RAM and CPU
  usage](https://docs.docker.com/engine/containers/resource_constraints/)
  of the container ([lisp-mcp
  project](https://github.com/gendl/lisp-mcp) is being actively
  updated to support these options as pass-through to the automated
  container startup).


## Installation

### Direct Installation in Emacs (use with caution if not using a container)

1. Install the required package:
   - simple-httpd: `M-x package-install RET simple-httpd RET`

2. Copy the source files to your Emacs load path.

3. Add to your `init.el`:
   ```elisp
   (require 'emacs-lisply-backend)
   ```

### Docker Container

Build and run the provided Docker container:

```bash
# Build the container
./docker/build.sh

# Run the container
./docker/run-container.sh
```

## API Usage

The Lisply backend exposes a simple HTTP API that allows clients to evaluate Emacs Lisp code and interact with Emacs. Clients can directly connect to the server on port 7080 (internal container port) or 7081 (default mapped host port).

### Example API Usage

Here's how to interact with the Lisply backend using curl:

```bash
# Check if the server is running
curl http://localhost:7081/lisply/ping-lisp

# Evaluate a simple expression
curl -X POST http://localhost:7081/lisply/lisp-eval \
  -H "Content-Type: application/json" \
  -d '{"code": "(+ 1 2 3)"}'

# Get a list of buffers
curl http://localhost:7081/lisply/buffers

# Read a file
curl -X POST http://localhost:7081/lisply/read-file \
  -H "Content-Type: application/json" \
  -d '{"path": "/projects/myfile.txt"}'
```

### Integration with Other Tools

This backend can be integrated with any client that can make HTTP requests. It provides the foundation for tools that need to interact with Emacs programmatically, including:

- LLM (Large Language Model) tools
- Development environments
- CI/CD pipelines
- Custom scripts and utilities

When integrating with tool frameworks that support the Model Context Protocol, you'll need to point them to this server's endpoint.

## API Endpoints

- `/lisply/ping-lisp` - Check if the server is available
- `/lisply/lisp-eval` - Evaluate Emacs Lisp code
- `/lisply/tools/list` - List available MCP tools
- `/lisply/specs` - Get MCP configuration specs
- `/lisply/buffers` - List all Emacs buffers
- `/lisply/current-buffer` - Get information about the current buffer
- `/lisply/buffer-content` - Get the content of a specific buffer
- `/lisply/read-file` - Read a file
- `/lisply/write-file` - Write to a file

## Development

### Project Structure

- `source/emacs-lisply-backend.el` - Main entry point
- `source/lisply-http-setup.el` - HTTP server configuration
- `source/lisply-endpoints.el` - MCP endpoint definitions

### Building the Container

The container build process:
1. Copies the entire skewed-emacs repository into the container
2. Runs the `./setup` script to configure the Emacs environment
3. Installs required packages using the on-demand installation mechanism in init.el
4. Configures the Lisply backend

```bash
./docker/build.sh -t your-tag -n your-image-name
```

To run the container after building:

```bash
./docker/run-container.sh -i your-image-name:your-tag -p 7081 -m /path/to/your/projects
```

Note: By default, the container exposes port 7080 internally but maps to port 7081 on the host to avoid potential conflicts.

## License

This project is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0), which is compatible with GNU Emacs' GPL-3.0 license. The AGPL-3.0 provides all the protections of GPL-3.0 plus an additional provision to ensure that modifications to the software when used over a network are also made available to users.

## Acknowledgments

Thank you to emacs developers and the MCP community.