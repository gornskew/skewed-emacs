;;; configure-glime.el --- Set up SLIME Lisp impls -*- lexical-binding: t; -*-
;;; Commentary:
;; 
;; This file is part of the Emacs configuration for Common Lisp development.
;; It is designed to be used with SLIME (Superior Lisp Interaction Mode for Emacs)
;; and provides the list of Lisp implementations and their respective paths.
;;
;;; Code:

(defvar slime-lisp-implementations)

(let ((cl-engine-directory "~/cl-engines/"))
  (if (file-exists-p cl-engine-directory)
      (setq slime-lisp-implementations
	    (cl-case system-type
	      (darwin `((acl (,(concat cl-engine-directory "macos/allegro101/64/non-smp/mlisp"))
			     :env ("LD_LIBRARY_PATH=/usr/local/Cellar/openssl@1.1/1.1.1j/lib"))
		        (acla (,(concat cl-engine-directory "macos/allegro101/64/non-smp/alisp"))
			      :env ("LD_LIBRARY_PATH=/usr/local/Cellar/openssl@1.1/1.1.1j/lib"))
		        (acls (,(concat cl-engine-directory "macos/allegro101/64/smp/mlisp"))
			      :env ("LD_LIBRARY_PATH=/usr/local/Cellar/openssl@1.1/1.1.1j/lib"))
		        (aclas (,(concat cl-engine-directory "macos/allegro101/64/smp/alisp"))
			       :env ("LD_LIBRARY_PATH=/usr/local/Cellar/openssl@1.1/1.1.1j/lib"))
		        (ccl (,(let ((local-path "~/system-setups/macos/cl-engines/ccl/dx86cl64"))
                                 (if (file-exists-p local-path)
                                     local-path
                                   (concat cl-engine-directory "macos/ccl-1.12/dx86cl64")))))
		        (sbcl ("sbcl"))))

	      (windows-nt `(
			    (acl64 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\non-smp\\mlisp.exe"))
			    (acla64 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\non-smp\\alisp.exe"))
			    (acl864 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\non-smp\\mlisp8.exe"))
			    (acla864 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\non-smp\\alisp8.exe"))

			    (acls64 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\smp\\mlisp.exe"))
			    (aclas64 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\smp\\alisp.exe"))
			    (acls864 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\smp\\mlisp8.exe"))
			    (aclas864 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "64\\smp\\alisp8.exe"))

			    (acl (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "32\\non-smp\\mlisp.exe"))
			    (gdl (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "32\\non-smp\\mlisp.exe"))
			    (acla (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "32\\non-smp\\alisp.exe"))
			    (acl8 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "32\\non-smp\\mlisp8.exe"))
			    (acla8 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "32\\non-smp\\alisp8.exe"))
			    (agdl8 (,(concat cl-engine-directory "windows/allegro101/run-gdl-slime.bat") "32\\non-smp\\alisp8.exe"))

			    (ccl (,(concat cl-engine-directory "windows/ccl-1.11/wx86cl64.exe")))
			    (ccl32 (,(concat cl-engine-directory "windows/ccl-1.11/wx86cl.exe")))
			    (ccl110 (,(concat cl-engine-directory "windows/ccl-1.10/wx86cl64.exe")))
			    (sbcl (,(concat cl-engine-directory "windows/sbcl/sbcl-1.1.12-win-x86/sbcl.exe")))
			    (ecl ("ecl"))
			    (clisp ("clisp"))))

	      ;;
	      ;; FLAG - detect when we are on a 32-bit system and limit to 32-bit options. 
	      ;;
	      (gnu/linux `((acl (,(concat cl-engine-directory "linux/allegro101/64/non-smp/mlisp")))
		           (acla (,(concat cl-engine-directory "linux/allegro101/64/non-smp/alisp")))
		           (acls (,(concat cl-engine-directory "linux/allegro101/64/smp/mlisp")))
		           (aclas (,(concat cl-engine-directory "linux/allegro101/64/smp/alisp")))
		           (ccl (,(concat cl-engine-directory "linux/ccl-1.12/lx86cl64")))
		           ;;(sbcl (,(concat cl-engine-directory "linux/sbcl/bin/sbcl")))
		           (sbcl ("sbcl"))
		           ))))
    (message "cl-engines directory not found in %s, no lisp-implementations for Slime."
	   cl-engine-directory)))

(defvar quicklisp-slime-helper)

(provide 'configure-glime)
;;; configure-glime.el ends here


