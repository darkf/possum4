*"Possums are quadrupedals. Fourth time's the charm."*

**Possum** is a minimalistic Scheme-inspired language aiming to be concise and simple. Its main novelty is that by using the arity (number of arguments) of functions, it lets you leave out punctuation for function application (you know that's cool because it rhymes!)

Here's a quick example of Possum code for you:

    defun factorial n is
      if = n 0
        1
      else
        * n factorial - n 1
    end
    
    print factorial 10
    
You could write this on one line, as `defun factorial n is if = n 0 1 else * n factorial - n 1 end print factorial 10`, but we don't recommend that, for your safety and the safety of others.

The only limitation is that if you want to apply a function derived at runtime (not via `defun`), such as through higher-order functions, then you must wrap the application in parentheses, like this:

    def f -> x is + 1 x end (* assign a lambda to f *)
    print (f 10) (* apply f *)
    