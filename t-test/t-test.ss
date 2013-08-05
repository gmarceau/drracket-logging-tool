;;;
;;; Time-stamp: <2007-05-03 14:17:51 nhw>
;;;
;;; Copyright (C) 2005 by Noel Welsh. 
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

(module t-test mzscheme

  (require (lib "etc.ss")
           (lib "list.ss" "srfi" "1")
           (only (lib "43.ss" "srfi") vector-fold)
           (planet "science.ss" ("williams" "science.plt" 2)))

  (provide degrees-of-freedom
           pooled-variance
           t-statistic
           p-value
           t-test)

  ;; square : number -> number
  (define (square x)
    (* x x))

  ;; degrees-of-freedom : (vector-of number) (vector-of number) -> number
  (define (degrees-of-freedom s1 s2)
    (let ((n1 (vector-length s1))
          (n2 (vector-length s2)))
      (+ n1 n2 -2)))
  
  ;; pooled-variance : (vector-of number) (vector-of number) -> number
  ;;
  ;; Calculated the pooled sample variance, s^2, as used by
  ;; the unpaired t-test.
  (define (pooled-variance s1 s2)
    (define (sum-squared-residual s mean)
      (vector-fold
       (lambda (idx seed elt)
         (+ (square (- elt mean)) seed))
       0
       s))
    (let ((mean1 (mean s1))
          (mean2 (mean s2)))
      (/ (+ (sum-squared-residual s1 mean1)
            (sum-squared-residual s2 mean2))
        (degrees-of-freedom s1 s2))))

  ;; t-statistic : (vector-of number) (vector-of number) -> number
  ;;
  ;; Calculate the unpaired t statistic for two data sets
  (define (t-statistic s1 s2)
    (let ((mean1 (mean s1))
          (mean2 (mean s2))
          (n1 (vector-length s1))
          (n2 (vector-length s2)))
      (/ (- mean1 mean2)
         (sqrt (* (pooled-variance s1 s2)
                  (+ (/ 1 n1)
                     (/ 1 n2)))))))

  ;; p-value : (vector-of number) (vector-of number) -> number
  ;;
  ;; Calculate the p-value for the two-tailed t-test
  (define (p-value s1 s2)
    (let* ([df       (degrees-of-freedom s1 s2)]
           [t        (t-statistic s1 s2)])
      (* 2 (t-distribution-cdf t df))))

  ;; t-test :  (vector-of number) (vector-of number) [number] -> boolean
  (define t-test
    (opt-lambda (s1 s2 [significance-level 0.05])
      (< (p-value s1 s2) significance-level)))
  )
                                        ; 
