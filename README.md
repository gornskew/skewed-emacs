# Skewed Emacs: A Handy Setup for Gnu Emacs with Slime and AI

![Skewed Emacs Logo](img/skewed-1-t.png)

This is a comprehensive, opinionated configuration for Emacs and
Linux/Unix bash environments, optimized for Terminal-mode Emacs-based
Slime Common Lisp and Gendl development. Skewed-emacs is available in
a containerized form which avoids the need to touch your own config
files. Skewed-emacs also ships with a built-in [Lisply
backend](./dot-files/emacs.d/sideloaded/lisply-backend/README.md) for
exposing your emacs to an AI Agent as an MCP server.

## Features

- **Emacs Configuration:**
  - Ships with common useful packages pre-installed 
  - Includes [Slime](https://en.wikipedia.org/wiki/SLIME) setup for
    Common Lisp and Gendl development, with extensive customizations
  - AI client integration (Copilot, GPT, Ellama, etc)
  - MCP (Model Context Protocol) server backend for allowing AI agents
    to drive your emacs from AI agents
  - Org-mode, Magit, Doom color themes, theme functions

- **Skewed Windows:**
  - For using Emacs with modern keyboards, it is recommended to [map
    CapsLock to Control](windows-keybindings/README.md).
  - AutoHotkey (.ahk) configuration for Emacs-style keybindings across
    Microsoft Edge, Chroms, Claude, and other applications as per your
    specification. Just double click the file in File Explorer.

- **Docker Integration:**
  - Docker Compose for running skewed-emacs with other helpful
    containers. 
  - `docker/Dockerfile` and `docker./build` script for building and
    pushing the skewed-emacs container. Note the comment in `build`
    about whether to use `silex/emacs` or install our own emacs (our
    own emacs supports X11 but is 0.5GB larger).

## Containerized Runnings (recommended)

1. Clone this repo anywhere on your filesystem:
   ```bash
   git clone <repository-url> skewed-emacs
   cd skewed-emacs
   ./compose-dev up 
   ```

This starts three containers:
- **skewed-emacs**: Emacs with AI integrations (HTTP API on port 7081)  
- **gendl**: Common Lisp/Gendl system (HTTP API on port 9081, SWANK on port 4201)
- **lisply-mcp**: Node.js environment for Claude Code (MCP tools auto-configured)


2. See [the directions](./README-compose.md) for further details.


## Local Installation

This section is for setting up the .emacs.d and other so-called "dot
files" in your home directory. It does not download or install or run
any of the systems discussed in the Containerized Runnings section
above.

1. Clone this repo anywhere on your filesystem:
   ```bash
   git clone <repository-url> skewed-emacs
   ```

2. Run the setup script:
   ```bash
   cd skewed-emacs
   ./setup
   ```
   
   The setup script will create symlinks of the salient "dot-files"
   (hidden files starting with `.` pointing to the corresponding files
   in the cloned repo, for example:
   
    `~/.emacs.d -> ~/skewed-emacs/dot-files/emacs.d`
   
   If you already have any of these dot files existing (as links or
   actual files/directories), the existing files will be backed up
   with names appended with `-pre-skewed-emacs`.


# Optional options

- `--dry-run`: Shows what would happen without making any changes
- `--shadow-suffix=NAME` or `--shadow-suffix NAME`: Creates symlinks with a "-NAME" suffix
     (e.g., with `--shadow-suffix=test` or `--shadow-suffix test` creates ~/.emacs.d-test instead of ~/.emacs.d)
- `--scrub-shadow-suffix=NAME` or `--scrub-shadow-suffix NAME`: Removes all symlinks with the "-NAME" suffix
     (e.g., `--scrub-shadow-suffix=test` removes ~/.emacs.d-test, ~/.bash_profile-test, etc.)
- `--scrub-shadow-suffix=""` or `--scrub-shadow-suffix=`: Removes all default symlinks without a suffix (e.g., removes ~/.emacs.d, ~/.bash_profile, etc.)

The setup script will automatically detect and replace broken symlinks
and handle existing dotfiles by backing them up with a
`-pre-skewed-emacs` suffix. It also skips backup files ending with
tilde (~) in the dot-files directory.

##   Example with options:

```bash
   # Preview changes without modifying anything
   ./setup --dry-run
   
   # Install configuration files with regular names
   ./setup
   
   # Install configuration files with "-shadow" suffix
   # (useful for testing or for maintaining multiple configurations)
   ./setup --shadow-suffix=shadow
   
   # Install with a custom suffix
   ./setup --shadow-suffix=work
   
   # Preview shadow installation without making changes
   ./setup --dry-run --shadow-suffix=shadow
   
   # Preview custom suffix installation without making changes
   ./setup --dry-run --shadow-suffix=test
   
   # Remove all symlinks with the "-test" suffix
   ./setup --scrub-shadow-suffix=test
   
   # Preview removal of all symlinks with the "-shadow" suffix without making changes
   ./setup --dry-run --scrub-shadow-suffix=shadow
   
   # Remove all symlinks with the "-test" suffix and create new ones with "-work" suffix
   ./setup --scrub-shadow-suffix=test --shadow-suffix=work
   
   # Preview removing all symlinks with the "-test" suffix and creating
   
```


⚠️ **Warning**: The setup script will overwrite your existing
`.emacs.d` directory and several dotfiles in your home
directory. Existing files will be backed up with a `-pre-skewed-emacs`
suffix. Run it with `--dry-run` first to see what it will do without
it touching anything.

## Requirements

 - Emacs 29+ (30+ recommended)
 - Node.js (optional, 22+ recommended, for github copilot install)
 - Docker (optional, 20+ recommended, for containerized development)
 - Git
 - realpath (included in coreutils on Linux; on macOS install via
   Homebrew: `brew install coreutils`)

## Configuration Structure

 - `dot-files/` - Contains all dotfiles that will end up symlinked to
   your home directory
  - `emacs.d/` - Emacs configuration, to be linked to ~/.emacs.d/
    - `init.el` - Main Emacs configuration entry point
    - `etc/` - Modular configuration files
    - `sideloaded/` - Third-party packages
  - `bash_profile` - Bash configuration
  - `tmux.conf` - tmux configuration
  - `zshrc` - ZSH configuration

 - `notes/` - Documentation and setup guides

## Customization

For personal customizations that shouldn't be committed to this
repository, add them to a `~/.emacs-local` file, which will be loaded
at the end of the Emacs initialization process.

## Installation Options and Rationale

Skewed Emacs is not currently designed to blend automatically with
your existing configuration; it's optimized more for fresh or
containerixed setups. If you want to merge this with your
already-existing configuration, you have two installation approaches
to choose from:

### 1. Regular Installation (Default)

With the standard installation approach, the `./setup` script will:

- Back up your existing dot files with a `-pre-skewed-emacs` suffix
- Replace them with symlinks to the Skewed Emacs versions
- Allow you to merge your customizations back in a controlled manner

If you have your own preëxisting config, add it back in a stepwise,
deliberate manner in your `~/.emacs-local` file, which you can
version-control separately or together with your own private fork or
branch of this repo. 

### 2. Shadow Installation (`--shadow-suffix=NAME`)
Shadow Intallation can be used if you want to inspect or play with
Skewed Emacs without stepping on your existing setup.

- Use `--shadow-suffix=shadow` to create parallel configuration files with a `-shadow` suffix
- Or use `--shadow-suffix=NAME` to create files with your own custom suffix

#### Using Shadow Configurations

To test the shadow installation you can use techniques such as the
following:

```bash
# Start Emacs with the shadow config
emacs -q --load "~/.emacs.d-shadow/init.el"
```


```bash
# Source the shadow bash profile in your current shell
source ~/.bash_profile[SUFFIX]
```

Or you could edit your existing `~/.bash_profile` and/or
`~/.emacs.d/init.el` to load the skewed-emacs shadow versions at some
point in their execution.


## MCP Server Backend 

This repository also includes a backend for the Lisply-MCP (Model
Context Protocol) [middleware](github.com/gornskew/lisply-mcp).  The
backend implementation is
[here](./dot-files/emacs.d/sideloaded/lisply-backend).



## License

This package is licensed under the GNU Affero General Public License
v3.0 (AGPL-3.0) which presumably is compatible with Gnu Emacs's GPL.
