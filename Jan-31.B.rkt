;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "2019-fall-reader.rkt" "csc104")((modname Jan-31.B) (compthink-settings #hash((prefix-types? . #f))))
; ● A Goal: Comma-Separate a List of Texts ●

; Let's design and implement a unary function that takes a list of texts and produces a text
;  from the elements, where each element gets separated by a comma and a space.
; That format for combining texts is known as “comma separated”.

; Example of the goal ...
#;(same! (comma-separate (list "amy" "bill" "clara" "donna"))
         "amy, bill, clara, donna")

; In lecture we ran this and saw that the test reported that  comma-separate  isn't defined, because
;  it wasn't yet : we are writing down our goals and thoughts first. We also changed our goal later
;  (for reasons discussed below) which is why that assertion, and the next one, are commented-out.

; ● Problem-Solving : Work on a Simpler Problem First (In Particular, Try a Concrete Example) ●

; It's unlikely that we can write the function definition for  comma-separate  immediately, so we work
;  on a simpler problem first : comma separate a particular list of texts. So the example of the goal
;  above is also something to work on, not just an illustration.

; ● Problem-Solving : Forwards and Backwards ●

; Solving most “real” problems requires exploration and creativity.
; Most problems are about connecting “what you have” (the current resources, or the arguments
;  for a function, or the hypotheses for a mathematical proof, etc), and ”what you want” (a goal,
;  which for a function is the result). A fundamental problem-solving technique is to work ∗both∗
;  forwards and backwards to try to connect them.

; Backwards: can we imagine an expression to calculate the example result text, from something
;  that is more “like” the argument? We could separate the result text into four texts, corresponding
;  to the four elements of the example argument, and show how to combine them. See ① below.

#;(same! (comma-separate (list "amy" "bill" "clara" "donna"))

         ; ① Thinking backwards from the goal result text ...
         (text-join "amy, " "bill, " "clara, " "donna")
       
         ; Goal result for the example call of  comma-separate  ...
         "amy, bill, clara, donna")

; Keep in mind that we played around in the Interactions to create and try many of the expressions
;  that are in the final summary assertions. So don't hesitate to copy some of them back to try them
;  out while reading the summary. And while building an assertion, run it frequently to confirm that
;  you copied the expressions correctly from the Interactions, and they all produce the same result
;  (except the comparison of the first and second expression will fail until the function is finally
;   defined).

; Forwards: can we image something like the argument that gets us closer to the four texts in ①?
; How about the list with ", " added to each element except the first? Can we create ...
#;(list "amy, " "bill, " "clara, " "donna") ;  ... from the argument?
; Also, can we imagine using that list to do ①?

; Before we tackle those two sub-problems, let's note that adding ", " to the elements of a list, and
;  joining the elements, is already hard enough to work on, even without worrying about treating
;  the last element differently from the others. So again ...

; ● Problem-Solving : Work on a Simpler Problem First ●

; Let's simplify the problem. If we can solve the simplified problem, then we can see if that helps us
;  solve the original. If we can't solve the simplified problem, we can't solve the original anyway!

; For a simpler goal, let's try to add ", " after every text, including the last one ...
(same! (comma-separate (list "amy" "bill" "clara" "donna"))

       ; ⑥ A Full Design ...
       (combine text-join (map comma-space (list "amy" "bill" "clara" "donna")))

       ; Some Partial Designs ...
       ;  ... ② ...
       (combine text-join (list "amy, " "bill, " "clara, " "donna, "))
       ;  ... ① (simplified) ...
       (text-join "amy, " "bill, " "clara, " "donna, ") 
       
       ; Goal result (simplified) for the example call of  comma-separate  ...
       "amy, bill, clara, donna, ")

; To help with that, can we turn the argument into ...
#;(list "amy, " "bill, " "clara, " "donna, ")
; If we do that then we can  combine  with  text-join : see ②.

; Let's write out expressions to add ", " to each element : see ③ below ...
(same!
 ; ⑤
 (map comma-space (list "amy" "bill" "clara" "donna"))
 ; ④
 (list (comma-space "amy")
       (comma-space "bill")
       (comma-space "clara")
       (comma-space "donna"))
 ; ③
 (list (text-join "amy"   ", ")
       (text-join "bill"  ", ")
       (text-join "clara" ", ")
       (text-join "donna" ", "))
 (list "amy, " "bill, " "clara, " "donna, "))

; Since ③ does “the same thing” to each element, we can likely accomplish it with  map .

; Regardless of whether we use  map , there are multiple similar expressions in there.
; More precisely, there are four expressions that differ in one spot, by a value, so there is
;  an implicit unary function of that value, which we can make explicit to help us.

; Let's name that lurking function “comma-space”, and note that we would like ...
(same! (comma-space "amy")  (text-join "amy"   ", "))
(same! (comma-space "bill") (text-join "bill"  ", "))
;  ... etc.

; Those are Full Designs that easily turn into a definition by picking a parameter name
;  for the value that varies ...
(define (comma-space a-text)
  (text-join a-text ", "))

; Let's use that to express ③ : see ④. And that looks like a  map : see ⑤.
; (Remember, we tried these expressions, and other ideas, in the Interactions before
;  recording them in the assertions!).

; Substituting ⑤ into ② produces expression ⑥, which uses the argument as-is, undedited,
;  which turns into a definition by picking a parameter name for the argument ...
(define (comma-separate texts)
  (combine text-join (map comma-space texts)))

; Let's make sure it works for another argument, to help make sure we didn't make the body
;  too specific to the main example (while writing this summary I initially forgot to change the
;  argument in the body to the parameter name, yet all the above tests still passed!).
(same! (comma-separate (list "ant" "bat" "cod" "dog" "eel" "fly" "gnu"))
       "ant, bat, cod, dog, eel, fly, gnu, ")
