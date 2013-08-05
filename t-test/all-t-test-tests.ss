(module all-t-test-tests mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (file "t-test-test.ss"))
  (provide all-t-test-tests)
  
  (define all-t-test-tests
    (test-suite 
     "all-t-test-tests"
     t-test-tests
     ))
  )
