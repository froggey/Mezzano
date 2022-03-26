(defsystem "test-bad-system-name"
    :description "Test special handling of CL-PPCRE")
(defsystem "test-bad-system-name-hyphen-separated-subsystem"
    :depends-on ("test-bad-system-name"))
