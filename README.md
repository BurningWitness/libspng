# libspng

Bindings to the [libspng](http://www.libspng.org) C library (version 0.72).

You can find the doc [here](https://libspng.org/docs/).

A few liberties were taken when making these bindings:

- Functions that return error codes are wrapped to throw `SpngError` instead;

- Helper functions are provided for working with a context (`spng_ctx_with`),
  creating PNG streams (`hReadFn`/`hWriteFn`/`memReadFn`) and option management.

- Every raw import can be found with a `'` appended in `Codec.Image.PNG.Simple.Internal`;
