;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tuition-graph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; tuition-graph.rkt



;; A program that visualizes the tuition costs for different schools
;; =================

;; Constants:

(define TEXT-SIZE 18)
(define TEXT-COLOR "black")


(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;; =================
;; Data definitions:

(define-struct school (n c))
;; School is (make-school String Number)
;; interp. a school with name n and associated cost c in USD
(define S1 (make-school "Harvard" 50000))

#;
(define (fn-for-school s)
  (... (school-n s)       ; String
       (school-c s)))     ; Number

;; Templeate rules used:
;; - compound: 2 fields

;; ListOfSchools is one of:
;; - empty
;; - (cons School ListOfSchool)
;; interp. a list of School
(define LOS1 empty)
(define LOS2 (cons (make-school "Harvard" 50000) (cons (make-school "UBC" 20000) empty)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest los)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons School ListOfSchool)
;;  - reference: (first los) is School 
;;  - self-reference: (rest los) is ListOfSchool


;; Functions:

;; ListOfSchool -> Image
;; takes a list of schools and produces a bar chart showing the name and the tuition cost
(check-expect (chart empty) (square 0 "solid" "white"))
(check-expect (chart (cons (make-school "King's College" 9999) empty))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text (school-n (make-school "King's College" 9999)) TEXT-SIZE TEXT-COLOR))
                                           (rectangle BAR-WIDTH (* (school-c (make-school "Kings College" 9999)) Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* (school-c (make-school "Kings College" 9999)) Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

(check-expect (chart (cons (make-school "King's College" 9999) (cons (make-school "Yale" 80000) empty)))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text (school-n (make-school "King's College" 9999)) TEXT-SIZE TEXT-COLOR))
                                           (rectangle BAR-WIDTH (* (school-c (make-school "Kings College" 9999)) Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* (school-c (make-school "Kings College" 9999)) Y-SCALE) "solid" BAR-COLOR))
                            (beside (overlay/align "center" "bottom"
                                                   (rotate 90 (text (school-n (make-school "Yale" 80000)) TEXT-SIZE TEXT-COLOR))
                                                   (rectangle BAR-WIDTH (* (school-c (make-school "Yale" 80000)) Y-SCALE) "outline" "black")
                                                   (rectangle BAR-WIDTH (* (school-c (make-school "Yale" 80000)) Y-SCALE) "solid" BAR-COLOR))
                                    (square 0 "solid" "white"))))


;(define (chart los) (square 0 "solid" "white")) ; stub

;; <Template from ListOfSchool>
(define (chart los)
  (cond [(empty? los) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (make-bar (first los))
                       (chart (rest los)))]))

;; School -> Image
;; creates a bar with the school name and tuition cost
(check-expect (make-bar (make-school "King's College" 9999))
              (overlay/align "center" "bottom"
                             (rotate 90 (text (school-n (make-school "King's College" 9999)) TEXT-SIZE TEXT-COLOR))
                             (rectangle BAR-WIDTH (* (school-c (make-school "Kings College" 9999)) Y-SCALE) "outline" "black")
                             (rectangle BAR-WIDTH (* (school-c (make-school "Kings College" 9999)) Y-SCALE) "solid" BAR-COLOR)))

;(define (make-bar s) (square 0 "solid" "white")) ;stub

;; <Template used from School>
(define (make-bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-n s) TEXT-SIZE TEXT-COLOR))
                 (rectangle BAR-WIDTH (* (school-c s) Y-SCALE) "outline" "black")
                 (rectangle BAR-WIDTH (* (school-c s) Y-SCALE) "solid" BAR-COLOR)))
