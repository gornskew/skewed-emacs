# Emacs Lisply Backend - Model Context Protocol for LLMs to drive Emacs

This repository depends on the [lisp-mcp
project](https://github.com/gendl/lisp-mcp), which provides a generic
MCP server wrapper for backend Lisp-speaking services. This project
(emacs-lisply-backend) contains a compatible back-end server
implementation for Gnu Emacs and emacs lisp. It is named
"emacs-lisply-backend" to avoid confusion with other projects such as
[`mcp.el`](https://github.com/lizqwerscott/mcp.el) which aim to build
mcp _clients_ within emacs (another worthy goal).


## Overview

The Model Context Protocol (MCP) enables Large Language Models (LLMs)
to interact with external tools and services. Gnu Emacs is a text
editor which can also run as a daemon (server) and contains an
interpreter and compiler for the emacs-lisp language, a parenthesized
s-expression (Symbolic Expression) format. This implementation
provides a standard way for LLMs to evaluate emacs-lisp expressions,
via which it can perform various editing and file-manipulation
operations perhaps, with proper training, in a more efficient way than
the `grep` and `sed` techniques predominantly used by filesystem MCP
services today.

## Key Features

- **HTTP Server**: Exposes Emacs functionality through a simple HTTP API
- **Optional Standard I/O** Optionally operates through stdio instead of http
- **Lisp Evaluation**: Evaluate arbitrary Emacs Lisp code from the LLM

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
./build-container.sh

# Run the container
./run-container.sh
```

## Claude Desktop Configuration

First, Configure Claude Desktop: Edit your Claude Desktop
   configuration to invoke NodeJS with the mcp-wrapper.js script from
   the [lisp-mcp project](https://github.com/gendl/lisp-mcp) in the
   manner of the following examples (see the lisp-mcp [project
   README](https://github.com/gendl/lisp-mcp/README.md) for more
   details on use and configuration of mcp-wrapper.js.


### WSL with local NodeJS
   
   ```json
   {...
     "mcpServers":
	 {
      "gendl": {
	    "command": "wsl",
	    "args": [
		"node",
		"/path/to/lisp-eval-mcp/scripts/mcp-wrapper.js",
		"--debug",
		"--backend", "emacs",
		"--mount", "/path/to/your/projects/:/projects"
	    ]
		}
		
		...
		
			 }
   }
   
   ```

### Linux/Unix with local NodeJS
   
   ```json
   {...
     "mcpServers":
	 {
      "gendl": {
	    "command": "node",
	    "args": [
		"/path/to/lisp-eval-mcp/scripts/mcp-wrapper.js",
		"--debug",
		"--backend", "emacs",
		"--mount", "/path/to/your/projects/:/projects"
	    ]
		}
		
		...
		
			 }
   }
   
   ```

### NodeJS run through Docker on WSL

To run the Node.js wrapper in a Docker, you have mount your docker
socket to enable the wrapper to manage container lifecycle:

FLAG -- we need to pass the --backend and --debug on to the entrypoint
of the container, which will be something similar to
/projects/gendl-mcp/scripts/mcp-wrapper.js. Probably need to use `-e`
or otherwise adjust how we pass --debug etc below. The --mount should
also be passed on to the secondary emacs-mcp container started by the
nodejs script - the node container doesn't need any mounts (it will
just use stdio). But it does need the docker socket - that mount
should go directly to the container started here. 

   ```json
      {...
     "mcpServers":
	 {
      "gendl": {
	    "command": "wsl", 
	    "args": [
		"docker",
		"run",
		"-i",
	    "--mount", "/var/run/docker.sock:/var/run/docker.sock",		
		"-e", "BACKEND=emacs",
		"-e", "DEBUG_MODE=true",
		"-e", "MOUNTS=/path/to/your/projects/:/projects",
		"node", "/projects/path/to/lisp-mcp/scripts/mcp-wrapper.js"
	    ]
		}
		
		...
		
		 }
   }
   
   ```
   
### NodeJS run through Docker on Linux/Unix

   ```json
   {...
     "mcpServers":
	 {
      "emacs": {
	    "command": "docker",
	    "args": [
		"run",
		"-i",
	    "--mount", "/var/run/docker.sock:/var/run/docker.sock",		
		"-e", "BACKEND=emacs",
		"-e", "DEBUG_MODE=true",
		"-e", "MOUNTS=/path/to/your/projects/:/projects",
		"node", "/projects/path/to/lisp-mcp/scripts/mcp-wrapper.js"
	    ]
		}
		
		...
		
			 }
   }
   
   ```


3. Now restart Claude and look for a new MCP server "emacs" and some
   new MCP tools.  Claude can use the `lisp_eval` tool to execute
   Emacs Lisp code and interact with your Emacs environment.

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

```bash
./build-container.sh -t your-tag -n your-image-name
```

## License

This project is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0), which is compatible with GNU Emacs' GPL-3.0 license. The AGPL-3.0 provides all the protections of GPL-3.0 plus an additional provision to ensure that modifications to the software when used over a network are also made available to users.

## Acknowledgments

Thank you to emacs developers and the MCP community.