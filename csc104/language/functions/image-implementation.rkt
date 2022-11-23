#lang snack/pure

(reprovide (only-in mrlib/image-core image?)
           (only-in 2htdp/private/image-more
                    [freeze render]
                    [flip-vertical   flip]
                    [flip-horizontal mirror]))
(provide (except-out (all-defined-out)
                     2htdp:top 2htdp:right 2htdp:left 2htdp:bottom 2htdp:baseline
                     2htdp:solid 2htdp:outline)
         (rename-out [2htdp:image-width  width]
                     [2htdp:image-height height]))

; ★ Configuration ★
  
(module configuration racket/base (provide (all-defined-out))
    
  (define-values (configuration:default-text-image-size
                  configuration:rotation-direction
                  configuration:redirection-limit)
    (values 12 - 5)))

(require 'configuration)

; ★ Some Imports ★
  
(require (prefix-in color: (only-in "color-constants.rkt" black transparent))
         (submod snack/string no-string-literals)
           
         (prefix-in 2htdp: (combine-in
                            (only-in 2htdp/private/image-more
                                     rotate crop scale scale/xy
                                     image-width image-height
                                     above beside overlay
                                     above/align beside/align
                                     circle square triangle star rectangle ellipse
                                     text)
                            (only-in mrlib/image-core [make-color color])))

         racket/require
         (multi-in snack (symbol match definition))

         (only-in racket/class send) 
         (only-in racket/math exact-round pi)
         (only-in racket/list take drop first second third fourth)

         (only-in net/url call/input-url string->url get-pure-port)
         (only-in racket/draw bitmap%)
         (prefix-in image-more: (only-in 2htdp/private/image-more rotate))

         (only-in racket/draw make-bitmap bitmap-dc%)
         (only-in mrlib/image-core render-image bitmap->image)
         (only-in racket/class make-object))

(define-symbols #:prefix 2htdp:
  solid outline
  top baseline bottom
  left right)

; ★ url->image ★  

(define (url->image a-text)
  (image-more:rotate ; dummy rotate coerces to a 2htdp/image image
   0 (call/input-url (string->url a-text)
                     (λ (url [header '()])
                       (get-pure-port url header #:redirections configuration:redirection-limit))
                     (λ (port) (make-object bitmap% port 'unknown/alpha #false #true)))))

; ★ Transform : Rotate Scale Crop (flipping isn't customized) ★
  
(define (rotate         image angle) (2htdp:rotate (configuration:rotation-direction angle) image))
(define (clockwise      image)       (2htdp:rotate -90 image))
(define (anti-clockwise image)       (2htdp:rotate  90 image))  
  
(define (scale   image factor) (if (zero? factor) (blank) (2htdp:scale factor image)))
(define (shrink  image) (2htdp:scale 0.5 image))
(define (enlarge image) (2htdp:scale 2   image))

(define (scale-width image factor)
  (if (zero? factor) (blank 0 (2htdp:image-height image)) (2htdp:scale/xy factor 1 image)))
(define (scale-height image factor)
  (if (zero? factor) (blank (2htdp:image-width image) 0)  (2htdp:scale/xy 1 factor image)))
  
(define (thinner image) (scale-width  image 0.5))
(define (wider   image) (scale-width  image 2))
(define (shorter image) (scale-height image 0.5))
(define (taller  image) (scale-height image 2))

(defines
  (image-bottom (image- (λ (image pixels) (values 0 (- (2htdp:image-height image) pixels)
                                                  (2htdp:image-width image) pixels)))) 
  (image-right  (image- (λ (image pixels) (values (- (2htdp:image-width image) pixels) 0
                                                  pixels (2htdp:image-height image))))) 
  (image-left   (image- (λ (image pixels) (values 0 0 pixels (2htdp:image-height image))))) 
  (image-top    (image- (λ (image pixels) (values 0 0 (2htdp:image-width image) pixels))))
  (cut-bottom
   (cut- (λ (image pixels)
           (values 0      0 (2htdp:image-width image) (- (2htdp:image-height image) pixels)))))
  (cut-right
   (cut- (λ (image pixels)
           (values 0      0 (- (2htdp:image-width image) pixels) (2htdp:image-height image)))))
  (cut-left
   (cut- (λ (image pixels)
           (values pixels 0 (- (2htdp:image-width image) pixels) (2htdp:image-height image)))))
  (cut-top
   (cut- (λ (image pixels)
           (values 0 pixels (2htdp:image-width image) (- (2htdp:image-height image) pixels)))))
  #:with
  (((image- co-ordinates) image pixels)
   (define-values (x y w h) (co-ordinates image (exact-round pixels)))
   (2htdp:crop x y w h image))
  (((cut- co-ordinates) image pixels)
   (define-values (x y w h) (co-ordinates image (exact-round pixels)))
   (2htdp:crop x y w h image)))

; ★ Combine ★
  
(defines
    
  (overlay (case-lambda [() empty-image] [(img) img] [images (apply 2htdp:overlay images)])) 
  (beside  (case-lambda [() empty-image] [(img) img] [images (apply 2htdp:beside  images)]))
  (above   (case-lambda [() empty-image] [(img) img] [images (apply 2htdp:above   images)]))
    
  ((align-tops      . images) (apply align-beside 2htdp:top      images)) 
  ((align-baselines . images) (apply align-beside 2htdp:baseline images)) 
  ((align-bottoms   . images) (apply align-beside 2htdp:bottom   images))
    
  ((align-lefts     . images) (apply align-above  2htdp:left  images)) 
  ((align-rights    . images) (apply align-above  2htdp:right images))
    
  #:with
    
  (empty-image (2htdp:square 0 'solid 'black))
    
  ((align-beside   y . images) (apply 2htdp:beside/align   y empty-image empty-image images))
  ((align-above  x   . images) (apply 2htdp:above/align  x   empty-image empty-image images)))
  
; ★ Shape — including Text and Blank ★
  
(defines

  [triangle         (1D-shape 2htdp:triangle  2htdp:outline)]
  [filled-triangle  (1D-shape 2htdp:triangle  2htdp:solid)]
  [square           (1D-shape 2htdp:square    2htdp:outline)]
  [filled-square    (1D-shape 2htdp:square    2htdp:solid)]
  [circle           (1D-shape circle-shape    2htdp:outline)]
  [filled-circle    (1D-shape circle-shape    2htdp:solid)]
  [star             (1D-shape star-shape      2htdp:outline)]
  [filled-star      (1D-shape star-shape      2htdp:solid)]
  [rectangle        (2D-shape 2htdp:rectangle 2htdp:outline)]
  [filled-rectangle (2D-shape 2htdp:rectangle 2htdp:solid)]
  [oval             (2D-shape 2htdp:ellipse   2htdp:outline)]
  [filled-oval      (2D-shape 2htdp:ellipse   2htdp:solid)]

  [(blank [size₁ 0] [size₂ size₁]) (filled-rectangle size₁ size₂ color:transparent)]
    
  [(text->image string [font-size configuration:default-text-image-size] [color color:black])
   (2htdp:text string (exact-round font-size) (normalize-color color))]

  #:with

  [((1D-shape raw-shape drawing-mode) size [color color:black])
   (raw-shape size drawing-mode (normalize-color color))]
  [((2D-shape raw-shape drawing-mode) width height [color color:black])
   (raw-shape width height drawing-mode (normalize-color color))]
    
  [(circle-shape size drawing-mode color)
   (2htdp:circle (/ size 2) drawing-mode color)]
  [(star-shape   size drawing-mode color)
   (2htdp:rotate θ (2htdp:star (* size r) drawing-mode color))]
    
  [(normalize-color color) (apply 2htdp:color (map byte←%age color))]

  ; Mathematics to fit star into a specified square bounding box.
  [1/ϕ (/ ((sqrt 5) . - . 1) 2)]
  [θ -27]
  [θ-scale (/ (sqrt 2) (+ (sin (/ pi 5)) (cos (/ pi 5))))]
  [r (* 1/ϕ θ-scale)])
  
; ★ Pixels ★

(define color? (λ-♯match [(list r g b)   (and (<= 0 r 100) (<= 0 g 100) (<= 0 b 100))]
                         [(list r g b α) (and (<= 0 r 100) (<= 0 g 100) (<= 0 b 100)
                                              (<= 0 α 100))]))

(define (colors? v) (and (list? v) (andmap color? v)))

; Optimizations
;  caching bytes to percentage
;    > not making intermediate list of color structs
;      > unroll of map %age←byte
  
(module byte-percentages racket/base (provide (all-defined-out))
    
  (require (only-in racket/math exact-round))
    
  (define (byte←%age %age) (exact-round (* 2.55 %age)))
  (define (%age←byte byte) (vector-ref %ages←bytes byte))
    
  ; Cache the 256 possibilities up-front. See also racket's  rationalize .
  (define %ages←bytes (build-vector 256 (λ (byte) (* 1/10 (exact-round (* byte 1000/255)))))))
  
(require 'byte-percentages)

; For efficiency, reimplement 2htdp's   image->color-list  to directly produce our notion of colors.
(define (image->colors image)
  (define w (2htdp:image-width  image))
  (define h (2htdp:image-height image))
  (cond [(or (zero? w) (zero? h)) '()]
        [else (define bdc (make-object bitmap-dc% (make-bitmap w h)))
              (send bdc erase)
              (render-image image bdc 0 0)
              (define bytes (make-bytes (* w h 4)))
              (send bdc get-argb-pixels 0 0 w h bytes)
              ; Loop is 90% of the time.
              ; Unsafe operations net maybe 5% improvement.
              ; Not doing %age←byte gets 10% improvement.
              ; So it's the inner list building that's noticeable vs 2htdp, e.g.
              ;  trying 4-vectors saves 40%.
              (for/list ([i (in-range 0 (* w h 4) 4)])
                ; Simple tests outside this context suggest nested consing is no faster.
                (list (%age←byte (bytes-ref bytes (+ i 1)))
                      (%age←byte (bytes-ref bytes (+ i 2)))
                      (%age←byte (bytes-ref bytes (+ i 3)))
                      (%age←byte (bytes-ref bytes    i))))]))
  
; 1/3 of the time is the contract checking
; Changing  byte←%age  to use truncate and/or #i2.55 makes no detectable difference.
; Not going through a map to structs speeds it up by a factor of 5.
(define (colors->image color-list width height)
  (cond [(or (zero? width) (zero? height)) (blank width height)]
        [else (define bytes (make-bytes (* width height 4) 0))
              (for ([c (in-list color-list)] [i (in-naturals)])
                (define j (* i 4))
                (match c
                  [(list r g b)   (bytes-set! bytes    j    255)
                                  (bytes-set! bytes (+ j 1) (byte←%age r))
                                  (bytes-set! bytes (+ j 2) (byte←%age g))
                                  (bytes-set! bytes (+ j 3) (byte←%age b))]
                  [(list r g b α) (bytes-set! bytes    j    (byte←%age α))
                                  (bytes-set! bytes (+ j 1) (byte←%age r))
                                  (bytes-set! bytes (+ j 2) (byte←%age g))
                                  (bytes-set! bytes (+ j 3) (byte←%age b))]))
              (define bmp (make-bitmap width height))
              (send bmp set-argb-pixels 0 0 width height bytes)
              (bitmap->image bmp)]))

; ★ Color Lens ★
  
(defines
  [redness     (case-lambda [(color) (first  color)] [(color %) (with-element color 0 %)])] 
  [greenness   (case-lambda [(color) (second color)] [(color %) (with-element color 1 %)])] 
  [blueness    (case-lambda [(color) (third  color)] [(color %) (with-element color 2 %)])]
  [opacity     (case-lambda [(color) (fourth color)] [(color %) (with-element color 3 %)])]
  #:with [(with-element ℓ i e) (append (take ℓ i) (list e) (drop ℓ (add1 i)))])
