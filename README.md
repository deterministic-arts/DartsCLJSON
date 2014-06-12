DartsCLJSON
===========

This library provides basic functions to read/parse JSON encoded data,
as well as stuff to generate properly formatted JSON representations of
objects. Unlike CL-JSON, this library provides mostly low-level stuff.
In particular, this library does not include a serialization layer for
arbitrary objects (though one could be added, using what is provided 
here as the building blocks)

One feature not found in other JSON-handling libraries AFAICT is the
optional push-parser support, which allows for "event-driven" parsing
of JSON encoded data (i.e., incrementally parse parts of the object
tree as soon as input bytes become available and get notified via 
callback, when a complete object has been consumed).


