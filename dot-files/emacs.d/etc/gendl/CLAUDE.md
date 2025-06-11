# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Related Documentation
- **Gendl Lisply Backend**: See `gwl/lisply-backend/CLAUDE.md` for HTTP API access to Gendl REPL
- **Emacs Lisply Backend**: See `skewed-emacs` repository: `dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md` for Emacs HTTP API access
- Both services can run simultaneously for comprehensive development workflow

## Build and Run Commands
- Start Docker container: `./docker/run`
- Connect with SLIME: `M-x slime-connect RET localhost RET 5200 RET`
- Start Gendl services: `(gendl:start-gendl!)`
- Access web interface: `http://localhost:9081/geysr`
- Run regression tests: `(gdl-lift-utils:run-regression-tests-pass-fail)`
- Create a new project: `(gendl-skel:gendl-skel :project-name "project-name")`
- Use cl-lite to load project: `(cl-lite "/path/to/project/" :create-asd-file? t)`
- Add to Quicklisp: `(pushnew "/path/to/project/" ql:*local-project-directories* :test #'equalp)`
- Load project: `(ql:quickload :project-name)`

## HTTP API Access
- **Gendl Lisply Backend**: `http://localhost:9081/lisply/lisp-eval` (POST with JSON `{"code": "lisp-expression"}`)
- **Emacs Lisply Backend**: `http://localhost:7080/lisply/lisp-eval` (POST with JSON `{"code": "emacs-lisp-expression"}`)
- Use `curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat` for testing
- Enable development mode: `(setq gwl:*developing?* t)` for Update!/SetSelf links

## Code Style Guidelines
- Package names should be kebab-case (e.g., `:gendl`, `:geom-base`)
- Function/variable names should be lowercase with hyphens
- Use 2-space indentation for Lisp code
- Use proper documentation strings in `input-slots`, `computed-slots`, and `objects`
- Follow standard Lisp conventions for naming and formatting
- Export symbols intended for external use with (:export ...) in package.lisp
- Use `theo` instead of `the-object` for cleaner reference syntax

## Gendl Object Structure
- **:input-slots**: Parameters that can be passed in when instantiating
- **:computed-slots**: Values derived from inputs or other computed values
- **:objects**: Child objects with their own attributes
- **:functions**: Methods defined for the object

## Project Structure
- */project-name/* - Root directory
- */project-name/project-name.asd* - ASDF system definition
- */project-name/init.lisp* - Initialization file
- */project-name/source/* - Source code directory
  - *package.lisp* - Package definition
  - *file-ordering.isc* - File loading order configuration
  - *assembly.lisp* - Main assembly object definition

## Object Definition Example
```lisp
(define-object custom-object (base-object)
  :input-slots
  (("Documentation string" required-param)
   ("Documentation string" param-with-default default-value))
   
  :computed-slots
  ((computed-value (* 2 (the param-with-default)))
   (display-controls (list :color :blue :transparency 0.5)))
   
  :objects
  ((child-object :type 'box
                 :length (the param-with-default)
                 :width 10 
                 :height 5)))
```

## Debugging Tips
- Access object properties: `(the slot-name)`
- Access object methods: `(the (function-name args))`
- Child object access: `(the child-name slot-name)`
- Examine object structure: `(the (message-list))`
- View inputs only: `(the (message-list :category :inputs))`
- For uncertain slots: `(defaulting (the slot-name) default-value)`