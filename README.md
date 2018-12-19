# PHOE-TOOLBOX - A personal utility library

This is a collection of Lisp utilities that I have collected, and will have
collected, over time.

All exported functions are expected to be documented.

## PHOE-TOOLBOX/BAG

A bag is a simple data structure that allows the following:
  * Inserting elements in O(n)
  * Retrieving elements at random in O(1)
  * Returning the count of all elements in O(1)

Basically, if a stack is LIFO, if a queue is FIFO, then a bag is RIRO (random
in, random out).

They are of type `BAG` and are created via `MAKE-BAG`. Contents of a bag are an
unspecialized adjustable vector accessible via `BAG-CONTENTS`. You can
`BAG-INSERT` an element into a bag, `BAG-COUNT` to get the element count,
`BAG-REMOVE` an element from the bag and, additionally, `BAG-COMPRESS` the bag's
internal vector.
