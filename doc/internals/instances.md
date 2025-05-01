# Instances

Instances are a flexible, efficient, and compact representation of user-defined objects.

All of `STRUCTURE-OBJECT`, `STANDARD-OBJECT`, and `FUNCALLABLE-STANDARD-OBJECT` are built on top of instances.

## Instances

An instance is a regular object pointer value with value tag `+TAG-OBJECT+`. They can be allocated in any area, depending on the layout.

An instance consists of a header word immediately followed by the instance slots.

The header word consists of the gc/spare bits at `(BYTE 2 0)`, the tag bits at `(BYTE 6 2)` contains either `+OBJECT-TAG-INSTANCE+` or `+OBJECT-TAG-FUNCALLABLE-INSTANCE+`, depending on if it's funcallable or not. The remaining bits contain a complete object pointer to the layout object for that instance. Since pointers on all targets are at most 56 bits wide, everything fits neatly.

Instance slots immediate follow the header and are described by that instance's `specific layout`, which is either specified directly by the layout in the header or indirectly if the instance has been made obsolete.

The `specific layout` is the `LAYOUT` object that exactly describes that instance, not a newer or older layout for the same class.

The layout object for an instance is either a `LAYOUT` struct instance or an `OBSOLETE-INSTANCE-LAYOUT` struct instance, depending on if the original layout had been made obsolete.

## Layouts

The `LAYOUT` object describes the layout of an instance.

It's a `sealed` structure-object allocated in the `wired` area with the following slots:

* `CLASS` - The class of the instance. What `CLASS-OF` would return.
* `OBSOLETE` - `NIL` if the layout is current and has not been made obsolete. Otherwise it points to a newer layout for the same class, which may itself also be obsolete.
* `HEAP-SIZE` - The size, in words, of the object, not including any trailing padding.
* `HEAP-LAYOUT` - Used by the GC to know which words of the object contain Lisp values and which contain unboxed data. Either `NIL` meaning no values, `T` meaning all words contain values to be scanned, or a `SIMPLE-BIT-VECTOR` the size of the object with a `1` bit indicating that word contains a Lisp value.
* `AREA` - Allocation area in which the object should be allocated.
* `INSTANCE-SLOTS` - A `SIMPLE-VECTOR` containing two adjacent entries per slot in the instance. The first entry is the name of the slot, the second entry is the `location` of the slot in the instance.

Layouts are allocated in the `wired` area since they are used by the GC. Allowing them to be movable would complicate the GC.

The first two words after the header in a funcallable instances are used to implement function calling. This and the different tag bits are the only differences between normal instances and funcallable instances. These two words can conceptually be thought of as hidden slots in the object, and code creating new layouts must be aware of them when computing the layout.

There's a one to many relationship between layout objects and instances, with many instances potentially sharing the same layout.

## Locations

A `location` is a type-carrying way to refer to a specific slot within a specific layout of an instance. It consists of a representation for the slot and the offset of the slot within the instance.

Since locations are quite internal to the system, they are represented as the two fields (representation and offset) packed into an integer.

The representation allows locations to be generic over the actual in-memory representation of a slot. Slots can be Lisp values, or they can be some kind of unboxed integer or float value.

`SLOT-DEFINITION-LOCATION`, `STANDARD-INSTANCE-ACCESS`, and `FUNCALLABLE-STANDARD-INSTANCE-ACCESS` are built on locations.

## Superseding instances

A specific instance can be superseded with another instance by replacing the layout in the object header word with one that points to an `OBSOLETE-INSTANCE-LAYOUT` object. This is done with the `SUPERSEDE-INSTANCE` function, which takes the instance to make obsolete, and the new instance that the object should be superseded by.

This is the mechanism by which both class redefinition and `CHANGE-CLASS` works.

The `OBSOLETE-INSTANCE-LAYOUT` structure consists of the following slots:

* `NEW-INSTANCE` - The new instance that the instance has been superseded by.
* `OLD-LAYOUT` - The `specific layout` for the instance, this exactly matches the layout of the original instance.

As the original instance still exists (and is the only way normal code will refer to that object), the identity of the instance is maintained.

The original instance can be superseded multiple times, this will replace the `NEW-INSTANCE` slot in the obsolete layout with the newer instance. An instance that was used as the new-instance when superseding another object should never be superseded, as that would create a deep chain of superseded instances.

## Slot access

We will use access via `SLOT-VALUE` as an example, however other ways to access slots (such as struct accessors) follow a similar pattern.

`SLOT-LOCATION-IN-INSTANCE` is called to resolve the both the location of the slot and the actual instance in which the access should occur.

It calls `FETCH-UP-TO-DATE-INSTANCE-SLOTS-AND-LAYOUT` with the instance. This fetches the `direct layout` of the instance.

If this is a `LAYOUT`, then the layout is checked to see if it is obsolete. If it is obsolete, then an instance of the new layout is created, the original instance is superseded with it, and `F-U-T-D-I-S-A-L` is retried. If the layout is up-to-date, then the instance and layout is returned.

Otherwise, if it is an `OBSOLETE-INSTANCE-LAYOUT` then the new instance is fetched from it. The layout of the new instance is examined to see if it is obsolete, if so then another more up-to-date instance is created and the original (not the intermediate) instance is superseded once again and the cycle repeats.

If the new instance is not obsolete then it and it's layout are returned from `F-U-T-D-I-S-A-L`.

Once the up-to-date instance and layout have been located, the slot-name is resolved to a location by calling `SLOT-LOCATION-USING-LAYOUT` with the layout and name. This looks up the slot by name in the layout's `INSTANCE-SLOTS` slot and returns the associated location.

If the slot is not an instance slot or does not exist then `SLOT-LOCATION-USING-LAYOUT` returns `NIL` and a different mechanism is used to locate the slot.

`SLOT-LOCATION-IN-INSTANCE` returns the up-to-date instance and the location to access. `SLOT-VALUE` then calls `STANDARD-INSTANCE-ACCESS` with the instance and location to access the slot.

## Indirection

In the common case, where the instance has not been made obsolete or superseded, there is no indirection when accessing slots. Fast slot accessor methods (including `SLOT-VALUE` with a known slot name) cache the location of their slots, and access the instance directly using it. There is a need to check that the instance hasn't been superseded, however this can be done just by looking at the header word. If the instance has been superseded, then we fall off the cached path and do the full lookup.

## GC snapping

The GC is aware of instances and obsolete instances. When it tries to transport an obsolete instance, instead of transporting the obsolete instance it uses the new instance. This maintains identity as it will update all references to the original obsolete instance so they point to the new instance.

## Structures

Since structure objects are also implemented using instances they get class redefinition effectively "for free", unlike most other Common Lisp implementations.

## Tricky nonsense for unnecessary optimization

The two object tags (`+OBJECT-TAG-INSTANCE+` and `+OBJECT-TAG-FUNCALLABLE-INSTANCE+`) have been carefully chosen to be different by exactly one bit, so that it's cheap to test if an object is one of either kind. This only really matters for `CLASS-OF` and is an optimization that could be omitted.

A special quasi-immediate object type exists, the instance-header, which is a value that has identical layout to the header word of instance. This gives it a value tag of `+TAG-INSTANCE-HEADER+`. Due to the way that object headers and values are represented, this partially overlaps the value tag with the object tag in the header. The value tag and object tags are carefully chosen to allow this. `+TAG-INSTANCE-HEADER+` is also carefully chosen so that an object's header word can be read and trivially converted to be bitwise-identical to an instance header object if that object is an instance.

This allows for fast type checks when looking for a specific class of instance. Fast being 3 instructions.

## Glossary

`header word` - The very first word of an instance (or heap object in general), containing the object tag bits and reference to the direct layout object.

`direct layout` - The layout or obsolete instance layout directly referenced by an instance's header word.

`specific layout` - The `LAYOUT` object that exactly describes a specific instance, not a newer or older layout for the same class.

`sealed` - Of a class, no subclasses can be defined and the class cannot be redefined.

`area` - Kind of memory where an object can be allocated. May be pageable or non-pageable, and copying or non-copying.

`wired` - Stored in an allocation area that is non-pageable and not moved by the garbage collector.

`location` - Offset and representation of a slot within an instance.

`representation` - The in-memory storage format of a value. For example, a full tagged lisp object, an untagged single float, an untagged 64-bit unsigned integer, etc.

`value tag` - Tag bits stored in the low bits of a value.

`value` - Tagged pointer pointing to an object, cons, or directly containing an immediate value such as a fixnum.

`object tag` - Tag bits that exist in the header word of an object.

`object` - Any Lisp object or array that is stored on the heap, except for conses. Consists of a header word followed by the contents of the object. Conses have a special representation with no header word for efficiency.
