(in-package #:bootwright)

(defstruct (test-result
            (:constructor make-test-result
                (&key image-path successp debugcon-output serial-output
                      qemu-log exit-code duration-ms)))
  image-path
  successp
  debugcon-output
  serial-output
  qemu-log
  exit-code
  duration-ms)

(defun normalize-expectation-list (value)
  (cond ((null value)
         '())
        ((and (listp value)
              (every #'stringp value))
         value)
        ((stringp value)
         (list value))
        (t
         (error "Expected a string or list of strings, got ~S." value))))

(defun expectations-satisfied-p (output expectations)
  (every (lambda (needle)
           (search needle output :test #'char=))
         (normalize-expectation-list expectations)))

(defun slurp-text-file (pathname)
  (if (probe-file pathname)
      (with-open-file (stream pathname
                              :direction :input
                              :if-does-not-exist nil
                              :external-format :utf-8)
        (if stream
            (let ((buffer (make-string (file-length stream))))
              (read-sequence buffer stream)
              buffer)
            ""))
      ""))

(defun sanitize-test-token (designator)
  (let* ((raw (string-downcase (canonical-token designator)))
         (sanitized (coerce (loop for ch across raw
                                  if (alphanumericp ch)
                                    collect ch
                                  else if (char= ch #\-)
                                    collect ch
                                  else
                                    collect #\-)
                           'string)))
    (string-trim "-" sanitized)))

(defun make-test-artifact-pathname (designator type)
  (let* ((tag (or (sanitize-test-token designator) "bootwright"))
         (stamp (get-universal-time))
         (nonce (random 1000000)))
    (merge-pathnames
     (make-pathname :name (format nil "bootwright-~A-~D-~D" tag stamp nonce)
                    :type type)
     #P"/tmp/")))

(defun process-exit-code-or-nil (process)
  (let ((status (sb-ext:process-status process)))
    (when (member status '(:exited :signaled :stopped))
      (sb-ext:process-exit-code process))))

(defun terminate-process (process)
  (when (sb-ext:process-alive-p process)
    (sb-ext:process-kill process 15)
    (loop repeat 20
          while (sb-ext:process-alive-p process)
          do (sleep 0.05))
    (when (sb-ext:process-alive-p process)
      (sb-ext:process-kill process 9)
      (loop repeat 10
            while (sb-ext:process-alive-p process)
            do (sleep 0.05))))
  process)

(defun elapsed-milliseconds (start end)
  (round (* 1000 (- end start))
         internal-time-units-per-second))

(defun qemu-program-for-machine (machine)
  (ecase (token-keyword (machine-descriptor-isa machine))
    ((:x86-16 :x86-32) "qemu-system-i386")
    (:x86-64 "qemu-system-x86_64")))

(defun qemu-arguments-for-image (machine image-path debugcon-path serial-path)
  (typecase (machine-descriptor-boot-protocol machine)
    (bios-mbr-protocol
     (list "-display" "none"
           "-monitor" "none"
           "-parallel" "none"
           "-no-reboot"
           "-no-shutdown"
           "-serial" (format nil "file:~A" (namestring serial-path))
           "-global" "isa-debugcon.iobase=0xe9"
           "-debugcon" (format nil "file:~A" (namestring debugcon-path))
           "-drive" (format nil "format=raw,file=~A,if=floppy"
                            (namestring image-path))))
    (t
     (error "Bootwright QEMU testing does not support boot protocol ~S yet."
            (machine-descriptor-boot-protocol machine)))))

(defun finalize-test-result (process image-path debugcon-path serial-path qemu-log-path start)
  (make-test-result :image-path image-path
                    :successp nil
                    :debugcon-output (slurp-text-file debugcon-path)
                    :serial-output (slurp-text-file serial-path)
                    :qemu-log (slurp-text-file qemu-log-path)
                    :exit-code (process-exit-code-or-nil process)
                    :duration-ms (elapsed-milliseconds start
                                                       (get-internal-real-time))))

(defun signal-test-failure (result expect-debugcon expect-serial)
  (error "Bootwright test failed.~%Expected debugcon: ~S~%Expected serial: ~S~%Exit code: ~S~%Debugcon output:~%~A~%Serial output:~%~A~%QEMU log:~%~A"
         expect-debugcon
         expect-serial
         (test-result-exit-code result)
         (test-result-debugcon-output result)
         (test-result-serial-output result)
         (test-result-qemu-log result)))

(defun run-qemu-test (compiled-image image-path timeout-ms expect-debugcon expect-serial
                       qemu-command errorp)
  (unless (or expect-debugcon expect-serial)
    (error "TEST-OS requires :EXPECT-DEBUGCON or :EXPECT-SERIAL."))
  (let* ((machine (compiled-image-target compiled-image))
         (debugcon-path (make-test-artifact-pathname (compiled-image-name compiled-image) "debug"))
         (serial-path (make-test-artifact-pathname (compiled-image-name compiled-image) "serial"))
         (qemu-log-path (make-test-artifact-pathname (compiled-image-name compiled-image) "log"))
         (program (or qemu-command (qemu-program-for-machine machine)))
         (arguments (qemu-arguments-for-image machine image-path debugcon-path serial-path))
         (start (get-internal-real-time))
         (deadline (+ start
                      (round (* timeout-ms internal-time-units-per-second) 1000)))
         (process nil)
         (successp nil))
    (dolist (pathname (list debugcon-path serial-path qemu-log-path))
      (when (probe-file pathname)
        (delete-file pathname)))
    (setf process
          (handler-case
              (sb-ext:run-program program arguments
                                  :search t
                                  :wait nil
                                  :output nil
                                  :error qemu-log-path)
            (error (condition)
              (error "Unable to start QEMU command ~S: ~A" program condition))))
    (unwind-protect
         (loop
           for now = (get-internal-real-time)
           for debugcon-output = (slurp-text-file debugcon-path)
           for serial-output = (slurp-text-file serial-path)
           do (when (and (expectations-satisfied-p debugcon-output expect-debugcon)
                         (expectations-satisfied-p serial-output expect-serial))
                (setf successp t)
                (return))
              (when (or (>= now deadline)
                        (not (sb-ext:process-alive-p process)))
                (return))
              (sleep 0.05))
      (terminate-process process))
    (let ((result (finalize-test-result process
                                        image-path
                                        debugcon-path
                                        serial-path
                                        qemu-log-path
                                        start)))
      (setf (test-result-successp result)
            (and successp
                 (expectations-satisfied-p (test-result-debugcon-output result)
                                           expect-debugcon)
                 (expectations-satisfied-p (test-result-serial-output result)
                                           expect-serial)))
      (when (and errorp (not (test-result-successp result)))
        (signal-test-failure result expect-debugcon expect-serial))
      result)))

(defun test-os (designator &key output (timeout-ms 5000) expect-debugcon
                                expect-serial qemu-command (errorp t))
  (let* ((compiled-image (build-image designator))
         (image-path (or output
                         (make-test-artifact-pathname
                          (compiled-image-name compiled-image)
                          "img"))))
    (write-image compiled-image image-path)
    (run-qemu-test compiled-image
                   image-path
                   timeout-ms
                   expect-debugcon
                   expect-serial
                   qemu-command
                   errorp)))

(defun test-os-source-file (pathname &key output image (timeout-ms 5000)
                                      expect-debugcon expect-serial
                                      qemu-command (errorp t))
  (let* ((source (normalize-os-source-pathname pathname))
         (images (load-os-source source))
         (designator (select-os-image source image images)))
    (test-os designator
             :output output
             :timeout-ms timeout-ms
             :expect-debugcon expect-debugcon
             :expect-serial expect-serial
             :qemu-command qemu-command
             :errorp errorp)))
