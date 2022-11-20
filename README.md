# libspng

Raw bindings to the [libspng](http://www.libspng.org) C library (version 0.7.2).
    
This package reexports the    
[Foreign.Storable.Offset](https://hackage.haskell.org/package/storable-offset-0.1.0.0/docs/Foreign-Storable-Offset.html)
module and implements the `Offset` instance for all the datatypes.    
Alas Hackage currently does not show this (as per [haddock#563](https://github.com/haskell/haddock/issues/563)).
    
Caveats of this library:    
- `DuplicateRecordFields` (since GHC 9.2 also `NoFieldSelectors`) are turned on in all modules with datatype
  definitions, record field names are the same as in the C library.    
  You should use `GHC.Records.getField` or record dot syntax to access datatype fields;    
    
- `data` and `type` record fields are replaced with `data_` and `type_` respectively.
  `GHC.Records.HasField` and `Foreign.Storable.Offset.Offset` instances are defined
  over both variants;    
   
## Maintenance    
Some bindings are bound to have errors in them, feel free to report them through Github.
