;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname drawings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))


(require "a07drawinglib.rkt")

;; A Coordinate is a (make-posn Int Int)
;; An ImageColor is a Str
;; requires: the Str is from the racket/draw colour database:
;; https://docs.racket-lang.org/draw/color-database___.html

;; A PrimCircle is a (make-prim-circle Coordinate Nat ImageColor)

;; A PrimTriangle is a (make-prim-triangle
;; Coordinate Coordinate Coordinate ImageColor)
;; A PrimElement is (anyof PrimTriangle PrimCircle)


;; Question a:

(define prim-picture
  (list
   (make-prim-triangle (make-posn 150 150)
                       (make-posn 125 188)
                       (make-posn 175 188) "Indianred")
   (make-prim-circle (make-posn 112 123) 20 "Black")
   (make-prim-circle (make-posn 188 123) 20 "Black")
   (make-prim-circle (make-posn 150 150) 75 "Brown")
   (make-prim-triangle (make-posn 98 98)
                       (make-posn 68 98)
                       (make-posn 83 78) "Hotpink")
   (make-prim-triangle (make-posn 202 98)
                       (make-posn 232 98)
                       (make-posn 217 78) "Hotpink")))


;; Question b:

;; (rect coord1 coord2 color) consumes a Coordinate representing the
;;   offset of the rectangle, a Coordinate representing the width and
;;   height of the rectangle and the color of it, then produce a list
;;   of triangles representing it
;; rect: Coordinate Coordinate ImageColor ->
;;                             (list PrimTriangle PrimTriangle)
;; example:
(check-expect (rect (make-posn 20 30) (make-posn 23 54) "Blue")
              (list
 (make-prim-triangle
  (make-posn 20 30)
  (make-posn 43 30)
  (make-posn 43 84) "Blue")
 (make-prim-triangle
  (make-posn 20 30)
  (make-posn 43 84)
  (make-posn 20 84) "Blue")))
              
(define (rect coord1 coord2 color)
  (list
   (make-prim-triangle (make-posn (posn-x coord1)
                                  (posn-y coord1))
                       (make-posn (+ (posn-x coord1)
                                     (posn-x coord2))
                                  (posn-y coord1))
                       (make-posn (+ (posn-x coord1)
                                     (posn-x coord2))
                                  (+ (posn-y coord1)
                                     (posn-y coord2)))
                       color)
   (make-prim-triangle (make-posn (posn-x coord1)
                                  (posn-y coord1))
                       (make-posn (+ (posn-x coord1)
                                     (posn-x coord2))
                                  (+ (posn-y coord1)
                                     (posn-y coord2)))
                       (make-posn (posn-x coord1)
                                  (+ (posn-y coord1)
                                     (posn-y coord2)))
                       color)))

;; tests:
(check-expect (rect (make-posn 90 90) (make-posn 10 10) "Red")
              (list
 (make-prim-triangle
  (make-posn 90 90)
  (make-posn 100 90)
  (make-posn 100 100) "Red")
 (make-prim-triangle
  (make-posn 90 90)
  (make-posn 100 100)
  (make-posn 90 100) "Red")))
(check-expect (rect (make-posn 100 100) (make-posn 20 80) "Brown")
              (list
 (make-prim-triangle
  (make-posn 100 100)
  (make-posn 120 100)
  (make-posn 120 180) "Brown")
 (make-prim-triangle
  (make-posn 100 100)
  (make-posn 120 180)
  (make-posn 100 180) "Brown")))


;; Question c:

;; A Point is a (list Int Int)
;; An Offset is a Point
;; A ShapeID is a Sym
;; requires: ShapeID is not 'circle, 'triangle, 'rectangle, 'component
;; A Shape is one of:
;; - (list 'circle ShapeID radius ImageColor)
;; - (list 'triangle ShapeID Point Point Point ImageColor)
;; - (list 'rectangle ShapeID width height ImageColor)
;; - (list 'component ShapeID Picture)
;; requires: radius,width,height are Nat
;; The ShapeID of a component does not appear in its Picture
;; when recursively expanded.
;; (i.e. there are no circular definitions)
;; A Picture is a (listof (list Offset ShapeID))
;; A ShapeList is a (listof Shape)
;; requires: every ID in the ShapeList is unique
;; A BundledDrawing is a (list width height Picture ShapeList)
;; requires: width, height are Nat
;; Every ShapeID in the Picture occurs in ShapeList.

(define fun-shapes
  '((circle soccer 10 "Blue")
    (triangle pyramid (10 10) (15 20) (5 20) "Yellow")
    (component soccerpyramid
               (((30 30) soccer)
                ((20 20) pyramid)))
    (component two-soccerpyramid
               (((0 0) soccerpyramid)
                ((0 30) soccerpyramid)))
    (component two-soccer
               (((20 20) soccer)
                ((20 60) soccer)))))

(define fun-pic
  '(((10 10) soccer)
    ((0 0) two-soccer)
    ((40 40) soccerpyramid)))

(define fun-drawing (list 300 300 fun-pic fun-shapes))


;; Question d:

;; constants:
(define summer-shapes '((circle top-scoop 10 "Pink")
(circle bottom-scoop 10 "LightBlue")
(component ice-cream
(((0 40) cone)
((10 35) bottom-scoop)
((10 25) top-scoop)))
(triangle cone (0 0) (20 0) (10 50) "Burlywood")
(circle moon 35 "Light Gray")
(circle sun 40 "Yellow")))

(define ice-cream-pic '(((10 50) ice-cream)
((70 20) sun)
((130 30) ice-cream)))

;; (get-picture-ids/help pic shapelist) consumes a Picture and
;;   a ShapeList and then produces a list of ShapeIDs that occur in
;;   the picture (with duplication)
;; get-picture-ids/help: Picture ShapeList -> (listof ShapeID)

(define (get-picture-ids/help pic shapelist)
  (cond [(empty? pic) empty]
        [else
         (append (get-id (shape-id (first pic))
                         shapelist)
                 (get-picture-ids/help (rest pic)
                                       shapelist))]))

;; (get-id input-id shapelist) consumes a ShapeID and a ShapeList
;;   and produce a list of ShapeID of the Shape which the ShapeID
;;   belongs to
;; get-id: ShapeID ShapeList -> (listof ShapeID)

(define (get-id input-id shapelist)
  (local [;; (search input-id shapelist) consumes a input-id and
          ;;  a ShapeList then search the shape-id of the inputlist
          ;; search: ShapeID ShapeList -> (Anyof Sym Picture)
          (define (search input-id shapelist)
                  (cond [(not (symbol=? input-id
                                        (shape-id (first shapelist))))
                         (search input-id (rest shapelist))]
                        [(not (symbol=?
                               'component
                               (shape-type (first shapelist))))
                         input-id]
                        [else (component-picture
                               (first shapelist))]))]
    (cond [(symbol? (search input-id shapelist))
           (cons input-id empty)]
          [else
           (cons input-id
                 (get-picture-ids/help (search input-id shapelist)
                                 shapelist))])))

;; (get-picture-ids pic shapelist) consumes a Picture and a ShapeList
;;   then produce a list of ShapeID in the Picture with no duplication
;; get-picture-ids: Picture ShapeList -> (listof ShapeID)
;; example:
(check-expect (get-picture-ids ice-cream-pic summer-shapes)
              (list 'ice-cream 'cone 'bottom-scoop 'top-scoop 'sun))

(define (get-picture-ids pic shapelist)
  (local [;; (delete-duplicate list-so-far lst) consumes a list and
          ;;  a list and delete the duplicated elements in the second
          ;;  input list
          ;; delete-duplicate: (listof Sym) (listof Sym) ->
          ;;                   (listof Sym)
          (define (delete-duplicate list-so-far lst)
            (local [;; (help2 lst element) consumes a list and
                    ;;  a symbol then insert the symbol into the
                    ;; list if it hasn't appeared in the list before
                    ;; help2: (listof Sym) Sym -> (listof Sym)
                    (define (help2 lst element)
                     (cond [(empty? lst) (cons element empty)]
                           [(not (symbol=? element
                                           (first lst)))
                            (cons (first lst)
                                  (help2 (rest lst) element))]
                           [else
                            lst]))]
            (cond [(empty? lst) list-so-far]
                  [else (delete-duplicate
                         (help2 list-so-far (first lst))
                         (rest lst))])))]
    (delete-duplicate '() (get-picture-ids/help pic shapelist))))
    
;; tests:
(check-expect (get-picture-ids fun-pic fun-shapes)
              (list 'soccer 'two-soccer 'soccerpyramid 'pyramid))
(check-expect (get-picture-ids ice-cream-pic summer-shapes)
              (list 'ice-cream 'cone 'bottom-scoop 'top-scoop 'sun))


;; Question e:

;; (offset-add offset component) consumes a offset and a list of
;;  shapes in the given component
;;  then add the offset to the previous offset
;; offset-add: Offset Picture -> Picture

(define (offset-add offset component)
  (cond [(empty? component) empty]
        [else (cons (list
                     (list (+
                            (first (first (first component)))
                            (first offset))
                           (+
                            (second (first (first component)))
                            (second offset)))
                     (second (first component)))
                    (offset-add offset (rest component)))]))

;; (make-shape pic-element shape-element shapelist) consumes
;;  an element in the picture and the corresponding shape
;;  in the shapelist and the shapelist, then make the shape
;; make-shape: (list Offset ShapeID) Shape ->
;;             (Anyof (list PrimElements)
;;                    (list PrimTriangle PrimTriangle))

(define (make-shape pic-element shape-element shapelist)
  (cond [(symbol=? 'triangle
                   (first shape-element))
         (list (make-prim-triangle
          (make-posn (+ (first (first pic-element))
                        (first (third shape-element)))
                     (+ (second (first pic-element))
                        (second (third shape-element))))
          (make-posn (+ (first (first pic-element))
                        (first (fourth shape-element)))
                     (+ (second (first pic-element))
                        (second (fourth shape-element))))
          (make-posn (+ (first (first pic-element))
                        (first (fifth shape-element)))
                     (+ (second (first pic-element))
                        (second (fifth shape-element))))
          (sixth shape-element)))]
        [(symbol=? 'circle
                   (first shape-element))
         (list (make-prim-circle
          (make-posn (first (first pic-element))
                     (second (first pic-element)))
          (third shape-element)
          (fourth shape-element)))]
        [(symbol=? 'rectangle
                   (first shape-element))
         (rect (make-posn (first (first pic-element))
                          (second (first pic-element)))
               (make-posn (third shape-element)
                          (fourth shape-element))
               (fifth shape-element))]
        [(symbol=? 'component
                   (first shape-element))
         (picture->primitives (offset-add
                               (first pic-element)
                               (third shape-element))
                              shapelist)]))
;; (find-shape-element pic-element shapelist ori-shapelist) consumes
;;  an element in the picture, a shapelist and a original shapelist
;; to find the the corresponding shape in the shapelist
;; find-shape-element: (listof Offset Shape) ShapeList ShapeList ->
;;                     (Anyof (list PrimElements)
;;                            (list PrimTriangle PrimTriangle))

(define (find-shape-element pic-element shapelist ori-shapelist)
  (cond [(symbol=? (second pic-element)
                   (second (first shapelist)))
         (make-shape pic-element (first shapelist) ori-shapelist)]
        [else
         (find-shape-element
          pic-element (rest shapelist) ori-shapelist)]))

;; (picture->primitives pic shapelist) consumes a picture and
;;  a shapelist and then convert it into the list of primitive
;;  elements
;; picture->primitives: Picture ShapeList -> (listof PrimElement)
;; example:
(check-expect
 (picture->primitives '(((20 30) ice-cream)) summer-shapes)
 (list
(make-prim-triangle
(make-posn 20 70)
(make-posn 40 70)
(make-posn 30 120)
"Burlywood")
(make-prim-circle (make-posn 30 65) 10 "LightBlue")
(make-prim-circle (make-posn 30 55) 10 "Pink")))

 (define (picture->primitives pic shapelist)
  (cond [(empty? pic) empty]
        [else (append (find-shape-element
                      (first pic) shapelist shapelist)
                      (picture->primitives (rest pic) shapelist))]))

;; tests:
(check-expect
 (picture->primitives fun-pic fun-shapes)
(list
 (make-prim-circle (make-posn 10 10) 10 "Blue")
 (make-prim-circle (make-posn 20 20) 10 "Blue")
 (make-prim-circle (make-posn 20 60) 10 "Blue")
 (make-prim-circle (make-posn 70 70) 10 "Blue")
 (make-prim-triangle (make-posn 70 70)
                     (make-posn 75 80)
                     (make-posn 65 80) "Yellow")))

(check-expect (picture->primitives (list (list (list 20 20) 'Rec))
                      (list (list 'rectangle
                                  'Rec
                                  30
                                  30
                                  "Blue")))
               (list (make-prim-triangle
                      (make-posn 20 20)
                      (make-posn 50 20)
                      (make-posn 50 50) "Blue")
                     (make-prim-triangle
                      (make-posn 20 20)
                      (make-posn 50 50)
                      (make-posn 20 50) "Blue")))

(check-expect (picture->primitives ice-cream-pic summer-shapes)
              (list
 (make-prim-triangle
  (make-posn 10 90)
  (make-posn 30 90)
  (make-posn 20 140) "Burlywood")
 (make-prim-circle (make-posn 20 85) 10 "LightBlue")
 (make-prim-circle (make-posn 20 75) 10 "Pink")
 (make-prim-circle (make-posn 70 20) 40 "Yellow")
 (make-prim-triangle
  (make-posn 130 70)
  (make-posn 150 70)
  (make-posn 140 120) "Burlywood")
 (make-prim-circle (make-posn 140 65) 10 "LightBlue")
 (make-prim-circle (make-posn 140 55) 10 "Pink")))


                                                            

;; Question f:
(define ice-cream-drawing (list 200 150 ice-cream-pic summer-shapes))

;; (drawing->image input-bd) consumes a BundledDrawing and produces
;;  an Image
;; drawing->image: BundledDrawing -> Image
(define (drawing->image input-bd)
  (render-image (make-posn (first input-bd)
                           (second input-bd))
                (picture->primitives (third input-bd)
                                     (fourth input-bd))))

