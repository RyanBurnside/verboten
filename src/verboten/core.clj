(ns verboten.core)

;; We want cl-format which handles nil properly.
(require '[clojure.pprint :refer [cl-format]]
         '[clojure.math :refer [cos sin]])

(defn enforce [predicate value]
  (assert (predicate value))
  value)

;; A function that acts like an "Object" ala LOGO's Turtles.
(defn turtle [x y direction]
  (let [x (enforce number? x)
        y (enforce number? y)
        direction (enforce number? direction)
        acc-err (fn [acc]
                  (throw (Exception. (cl-format nil "Error turtle accessor ~a is invalid." acc))))]
    (fn [& parameters]
      (case (first parameters)

        ;; Accessors
        :x x
        :y y
        :direction direction

        ;; Universal slot update (but not mutator!)
        ;; Export a fresh copy closure with updated values
        :update (fn [slot value]
                  (case slot
                    :x (turtle value y direction)
                    :y (turtle x value direction)
                    :direction (turtle x y value))
                  ;; Error, bad accessor
                  (acc-err value))
        :debug (fn []
                 (cl-format true "Turtle: x: ~a, y: ~a, direction: ~a"
                            x y direction))


        ;; Movement functions, these return a fresh copy with updates
        :fd (fn [step] (turtle (+ x (* (cos direction) step))
                               (+ y (* (sin direction) step))
                               direction))

        :bk (fn [step] (turtle (+ x (* (cos (- direction)) step))
                               (+ y (* (sin (- direction)) step))
                               direction))

        :rt (fn [angle] (turtle x y (+ direction angle)))

        :lt (fn [angle] (turtle x y (- direction angle)))

        ;; Invalid method fn request
        (acc-err (first parameters))))))

(comment
  It is essential that you first learn about closures.

  A closure is simply a function that has captured data defined outside.
  This captured data will always be referenced in the function until the
  function goes out of scope for collection.

  <simple example later>

  Also, multiple closures may reference the same closed over item.
  If one mutates the closed over data, all closures will see it update.

  <simple example later>

  Now it's essential to also know about calling an anonymous function directly.
  In s-expressions, we always try to call the first item in function position.

  (+ 1 2 3 4) 'here the + symbol refers to an additon function so it's called


  Let's make an anonymous function (fn foo [bar] (+ bar 21))
  This should be understood, it's a function that takes a parameter and adds 21.

  How do we call it with a parameter directly?
  Well, knowing you may call a function in first position let's do so.
  ((fn foo [bar] (+ bar 21)) 100) you'd get the answer 121. (try it)

  If that looks a bit funny it's because you have just been using symbols in
  function position so far. And those symbols refer to functions. The rules
  have not changed! We're just creating a function an executing it instantly
  instead due to s-expression evaluation rules.

  You're really seeing (temporary-function-here 100) get executed.

  So in the "Functional Object" turtle closure above the following happens.
  1. Some lexical variables are set up.
  2. These variables are then referenced in the returned anon function.
  3. The returned function is simply a lookup, it takes the name of one of its
  allowed "methods" and returns the fn, exporting it for your usage.
  4. You then call this returned method with YOUR new parameters.
  5. Since it's a closure, you are accessing its real values but in a
  predetermined safe way. No nils allowed, only number assignment. And this
  isn't after the fact. It's hard and immediate.


  This provides some protection that a simple hashmap does not.
  1. You can see that I use a predicate to ensure only numbers are allowed
  2. I ONLY let you get to the member data correctly.
  3. I ONLY let you modify the member data correctly.
  4. The functions only exist where needed. They don't pollute your module.
  For example, you are not allowed to access the debug message nor error message
  even though they're also in the closure. You're never allowed to set x nor y
  to "Foo" or \space etc.

  So we can create a fresh turtle like so and access his x attribute

  (let [frank (turtle 0 0 3.14)]
    (frank :x))
  => 0 (our x value)

  turtle forms a FUNCTION (our objects are closures therefore functions).
  We bind it to the symbol frank and then call frank in function position with
  the parameter :x. The case statement dispatches on :x and we get his x.

  Now the movement code allows some paraemeters. We still need old frank but
  each movement code returns a fresh frank with updated values.

  (let [frank (turtle 0 0 3.14)]
    (((frank :fd) 100) :x))
  => -99.99987317275395 (assume it's -100)

  That's a bit tricky isn't it?
  Well, We use (frank :fd) to return the fd (forward) method.
  That returned method gets called in function position with 100.
  This returns a fresh copy of new Frank in first position.
  We finally use this in function position to lookup his :x method.
  This taking no parameters, simply returns the :x.

  You can of course make objects that mutate their member data.
  You simply create the member data as Clojure atoms and mutate them with some
  :set like functions. :)

Remember though if you want to do multiple operations you need to keep hold
of the prior data. Just like a series of transformations on a list. Because you
need to call your functions like so ((frank) ...) it's best to use as->. You
have to keep those results being executed first. You don't need to use
threading macros but in small isolated cases they are helpful.

Best not to confuse yourself with -> and ->> they are wrong for this.

Mindlessly using the old crutch ...
(macroexpand '(-> (turtle 0 0 3.14)
                  ((frank :fd) 10)
                  ((frank :rt) 1.57)
                  ((frank :bk) 5)
                  ((frank :debug))))
=> ((frank :debug)
    ((frank :bk) ((frank :rt) ((frank :fd) (turtle 0 0 3.14) 10) 1.57) 5))

Maybe ->> is correct? (no)

(macroexpand '(->> (turtle 0 0 3.14)
                   ((frank :fd) 10)
                   ((frank :rt) 1.57)
                   ((frank :bk) 5)
                   ((frank :debug))))
=> ((frank :debug)
    ((frank :bk) 5 ((frank :rt) 1.57 ((frank :fd) 10 (turtle 0 0 3.14)))))

Now let's see the oft looked over as->

Ah ha! It's keeping the intermediate Franks for us!

(macroexpand '(as-> (turtle 0 0 3.14) frank
                ((frank :fd) 10)
                ((frank :rt) 1.57)
                ((frank :bk) 5)
                ((frank :debug))))

=> (let*
       [frank
        (turtle 0 0 3.14)
        frank
        ((frank :fd) 10)
        frank
        ((frank :rt) 1.57)
        frank
        ((frank :bk) 5)]
     ((frank :debug)))

Which gives us our final correct debug when run without macroexpand.
"Turtle: x: -10.011932207836802, y: 5.015912261103459, direction: 4.71"

You can call it "normally" however without as-> macro. Let's do that.

(((((((((turtle 0 0 3.14) :fd) 10) :rt) 1.57) :bk) 5) :debug))

Not quite as pleasant, due to the constant function lookup between parameters.

The world of OOP and Functional paradigms have more overlap that you may have
expected. This is because purists often promote wrong ideals based on limited
experiance. The model above is taken from Scheme and has been adapted to be
a function only simulation of an object. The Scheme model would also provide
some mutators for the closed over data. You could do that too by making them
atoms to begin with.

For a better version of OOP in general consult CLOS and the MOP in Lisp.

)

(defn foo
"I don't do a whole lot."
  [& _]

  ;; Remember to get a method "out" of a turtle we go (<turtle> :method)
  (println "Making a turtle and putting it through its ... paces.")
  (let [TAU 6.283185307179586]
    (as-> (turtle 0 0 0.0) frank
       ((frank :fd) 100)
       ((frank :rt) (* TAU 1/4))
       ((frank :fd) 50)
       ((frank :lt) (* TAU 1/8))
       ((frank :debug))))
  (println "\nFinished - Goodbye World!"))
