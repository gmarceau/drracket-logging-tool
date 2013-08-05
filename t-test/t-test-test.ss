(module t-test-test mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require "t-test.ss")
  
  (provide t-test-tests)

  (define-check (check-=-within a b eps)
    (check < (abs (- a b)) eps))

  ;; NOTE

  ;; I created the data below and chucked it into
  ;;
  ;; http://www.physics.csbsju.edu/cgi-bin/stats/t-test
  ;;
  ;; to test against the calculated results
  
  (define t-test-tests
    (test-suite
     "All tests for t-test"

     (test-case
      "t statistic is correct"
      (check-=-within
       (t-statistic
        #(1.00 2.00 2.00 2.00 3.00 3.00 4.00 4.00 5.00 12.0)
        #(12.0 13.0 15.0 17.0 19.0 21.0 21.0 22.0 32.0 43.0))
       -5.62
       0.01))

     (test-case
      "p-value is correct"
      (check-=-within
       (p-value
        #(9.00 10.0 12.0 14.0 15.0 18.0 19.0 23.0 27.0 32.0)
        #(10.0 15.0 17.0 20.0 23.0 26.0 27.0 30.0 35.0 42.0))
       0.105
       0.001))

     (test-case
      "t-test is false for unsignificant test"
      (check-false
       (t-test
        #(9.00 10.0 12.0 14.0 15.0 18.0 19.0 23.0 27.0 32.0)
        #(10.0 15.0 17.0 20.0 23.0 26.0 27.0 30.0 35.0 42.0))))

     (test-case
      "t-test is true for significant test"
      (check-true
       (t-test
        #(1.00 2.00 2.00 2.00 3.00 3.00 4.00 4.00 5.00 12.0)
        #(12.0 13.0 15.0 17.0 19.0 21.0 21.0 22.0 32.0 43.0))))

     (test-case
      "t-test accepts different significance level"
      (check-true
       (t-test
        #(9.00 10.0 12.0 14.0 15.0 18.0 19.0 23.0 27.0 32.0)
        #(10.0 15.0 17.0 20.0 23.0 26.0 27.0 30.0 35.0 42.0)
        0.110)))
     ))
  )
