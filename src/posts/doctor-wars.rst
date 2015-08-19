((title . "Doctor Wars")
 (date . "2015-02-25 07:37:15"))

.. code:: elisp

    ;; I did not add this -- rms.
    ;; But he might have removed it.  I put it back.  --roland
    (defun doctor-rms ()
      (cond (doctor--rms-flag (doctor-type (doc$ doctor--stallmanlst)))
            (t (setq doctor--rms-flag t) (doctor-type '(do you know Stallman \?)))))

Unfortunately `the Git log`_ cannot tell us whether an edit war really
happened since these comments are already present in the initial
revision.

.. _the Git log: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/play/doctor.el?id=7e09ef09a479731d01b1ca46e94ddadd73ac98e3#n1597
