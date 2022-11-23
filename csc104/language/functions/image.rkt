#lang snack/pure (require (submod "meta-language.rkt" image))

; ★ Default place to look for implementation, can override with trailing :  #:from library .
(define-for-syntax default #'"image-implementation.rkt")

; ★ Identical to  create  in non-image.rkt, until I get around to working out scoping details.
(define-syntax-parser create 
  [(_ :contracting binding)
   #:declare binding (alias/rename/from (attribute f) #'here custom)
   (define/provide this-syntax (attribute binding.library) default (attribute binding.data))])

; ★ All bindings that can be used with zero arguments, listed manually for now.
; See each declaration of the form  [(id . arguments) ⋯] .
(module* nullary #false (provide (prefix-out image: nullary-id?))
  (require syntax/parse)
  (define-literal-set nullary {blank 
                               above beside overlay 
                               align-tops align-baselines align-bottoms align-lefts align-rights})
  (define nullary-id? (literal-set->predicate nullary)))

; ★ Build some checks from contracts.rkt contracts via error.rkt combinators.

; Any arity, all the same type.

(define check-images (arguments-homogeneous-contract-checker image/C))

; Some optional arguments.

(define-values (check-1D-shape
                check-2D-shape                
                check-sizes
                check:text→image)
  (values (arity-arguments-contract-checker 1 non-negative/C                color/C)
          (arity-arguments-contract-checker 2 non-negative/C non-negative/C color/C)          
          (arity-arguments-contract-checker 0 non-negative/C non-negative/C)          
          (arity-arguments-contract-checker 1 text/C         font-size/C    color/C)))

(define check-color-lens (arity-arguments-contract-checker 1 color/C percentage/C))

; Dependent.

(define-values (check-within-width check-within-height)
  (values (binary-checker within-width/C) (binary-checker within-height/C)))

(define check-dimensions (contract-checker pixel-dimensions/C))

; ★ Create the Bindings

(map-macro create

           [(image? any/C)]

           [(url->image text/C) [#:aka url→image]]

           [(width  image/C)]
           [(height image/C)]
           
           ; ★ Transform : Rotate Scale Crop Flip ★

           ; Binaries

           [(rotate       image/C angle/C) [#:aka turn]]
           
           [(scale        image/C non-negative/C)]
           [(scale-width  image/C non-negative/C) [#:aka width-scale]]
           [(scale-height image/C non-negative/C) [#:aka height-scale]]

           [(image-left   image/C non-negative/C) check-within-width]
           [(image-top    image/C non-negative/C) check-within-height]
           [(image-right  image/C non-negative/C) check-within-width]
           [(image-bottom image/C non-negative/C) check-within-height]
           [(cut-left     image/C non-negative/C) check-within-width [#:aka remove-left]]
           [(cut-top      image/C non-negative/C) check-within-height [#:aka remove-top]]
           [(cut-right    image/C non-negative/C) check-within-width [#:aka remove-right]]
           [(cut-bottom   image/C non-negative/C) check-within-height [#:aka remove-bottom]]

           ; Unary-only

           ; Terminology: imperative.
           [(mirror    image/C)]
           [(flip      image/C)]     

           ; Unary specialized

           ; Terminology: desired version of image.
           [(anti-clockwise image/C)]
           [(clockwise      image/C)]

           ; Terminology: desired version of the image.
           [(shrink    image/C) [#:aka small]]
           [(enlarge   image/C) [#:aka large]]
           [(thinner   image/C) [#:aka thin]]
           [(wider     image/C) [#:aka wide]]
           [(shorter   image/C) [#:aka short]]
           [(taller    image/C) [#:aka tall]]

           ; ★ Combine ★
           
           [(above   . images) check-images]
           [(beside  . images) check-images]
           [(overlay . images) check-images [#:aka overlaid upon]]

           [(align-tops      . images) check-images [#:aka top-beside beside-top]]
           [(align-baselines . images) check-images [#:aka baseline-beside beside-baseline]]
           [(align-bottoms   . images) check-images [#:aka bottom-beside beside-bottom]]
           [(align-lefts     . images) check-images [#:aka left-above above-left]]
           [(align-rights    . images) check-images [#:aka right-above above-right]]

           ; ★ Shape — including Text and Blank ★
           
           [(triangle size . color) check-1D-shape]
           [(square   size . color) check-1D-shape]
           [(circle   size . color) check-1D-shape]
           [(star     size . color) check-1D-shape]
           [(rectangle width height . color) check-2D-shape]
           [(oval      width height . color) check-2D-shape]
           
           [(filled-triangle size . color) check-1D-shape [#:aka solid-triangle]]
           [(filled-square   size . color) check-1D-shape [#:aka solid-square]]
           [(filled-circle   size . color) check-1D-shape [#:aka solid-circle]]
           [(filled-star     size . color) check-1D-shape [#:aka solid-star]]
           [(filled-rectangle width height . color) check-2D-shape [#:aka solid-rectangle]]
           [(filled-oval      width height . color) check-2D-shape [#:aka solid-oval]]
           
           [(blank . dimensions) check-sizes]
           
           [(text->image text . size-color) check:text→image [#:aka text→image]]

           ; ★ Pixels ★
           
           [(render image/C)]

           [(color?  any/C) [#:aka colour?]]
           [(colors? any/C) [#:aka colours?]]

           [(image->colors image/C) [#:aka image→colors image→colours image->colours]]

           [(colors->image colors/C natural/C natural/C)
            check-dimensions [#:aka colors→image colours→image colours->image]]
           
           ; ★ Color Lens ★
           
           [(redness   color . new-%) check-color-lens]
           [(greenness color . new-%) check-color-lens]
           [(blueness  color . new-%) check-color-lens]
           [(opacity   color . new-%) check-color-lens [#:aka α alpha]])
