Writing code that runs as part of the supervisor comes with a few restrictions, particularly code that runs early or as part of an interrupt handler. This is a non-exhaustive list of restrictions.

General restrictions:
* No classes or methods. defclass/defmethod/defgeneric and the clos implementation in general are not written in a supervisor-safe way. Use structures and pass functions around instead.
* Restrictions on allocation. Depending on what kind of supervisor code you are writing, you may need to consider what allocation area is used to allocate from. If the code accessing the allocated object may be called during GC or an interrupt handler the object must be allocated from the wired area.
* Restrictions on memory access. Depending on the context the code is called from (during a GC, for example), it may not be possible to access non-wired memory.

The exact restrictions your code must follow, and the strength of them, are dependent on the context of the code.

Interrupt handler restrictions:
* No floating point. FPU registers are not saved/restored when an interrupt handler is invoked, so use of floating point may clobber interrupted state.
* No access to non-wired memory. Access to non-wired memory may cause a further interrupt as memory is paged in, or worse require that the page be read in from disk. This includes calling non-wired functions.
* No allocation. Interrupt handlers may run during GC, which inhibits allocation. All allocation required by an interrupt handler must be performed ahead of time, not in the interrupt handler itself.
* No standard error handling. Interrupt handlers must be correct, and any device-generated errors must be handled explictly.

Restrictions for interrupt handlers are hard and fast. Your code is broken if it does not follow these restrictions and will cause panics.
