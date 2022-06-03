{-# LANGUAGE DataKinds
           , DerivingStrategies
           , ExplicitNamespaces
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , NegativeLiterals
           , PatternSynonyms
           , TypeApplications #-}

{-# OPTIONS_HADDOCK not-home #-}

module Codec.Image.PNG.Simple.Internal where

import           Data.Bits
import           Data.Int
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset

#include "spng.h"

pattern SPNG_VERSION_MAJOR
      , SPNG_VERSION_MINOR
      , SPNG_VERSION_PATCH
     :: (Eq a, Num a) => a
pattern SPNG_VERSION_MAJOR = #const SPNG_VERSION_MAJOR
pattern SPNG_VERSION_MINOR = #const SPNG_VERSION_MINOR
pattern SPNG_VERSION_PATCH = #const SPNG_VERSION_PATCH



newtype SpngErrNo = SpngErrNo { unSpngErrNo :: #{type enum spng_errno} }
                    deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_IO_ERROR
      , SPNG_IO_EOF
      , SPNG_OK
      , SPNG_EINVAL
      , SPNG_EMEM
      , SPNG_EOVERFLOW
      , SPNG_ESIGNATURE
      , SPNG_EWIDTH
      , SPNG_EHEIGHT
      , SPNG_EUSER_WIDTH
      , SPNG_EUSER_HEIGHT
      , SPNG_EBIT_DEPTH
      , SPNG_ECOLOR_TYPE
      , SPNG_ECOMPRESSION_METHOD
      , SPNG_EFILTER_METHOD
      , SPNG_EINTERLACE_METHOD
      , SPNG_EIHDR_SIZE
      , SPNG_ENOIHDR
      , SPNG_ECHUNK_POS
      , SPNG_ECHUNK_SIZE
      , SPNG_ECHUNK_CRC
      , SPNG_ECHUNK_TYPE
      , SPNG_ECHUNK_UNKNOWN_CRITICAL
      , SPNG_EDUP_PLTE
      , SPNG_EDUP_CHRM
      , SPNG_EDUP_GAMA
      , SPNG_EDUP_ICCP
      , SPNG_EDUP_SBIT
      , SPNG_EDUP_SRGB
      , SPNG_EDUP_BKGD
      , SPNG_EDUP_HIST
      , SPNG_EDUP_TRNS
      , SPNG_EDUP_PHYS
      , SPNG_EDUP_TIME
      , SPNG_EDUP_OFFS
      , SPNG_EDUP_EXIF
      , SPNG_ECHRM
      , SPNG_EPLTE_IDX
      , SPNG_ETRNS_COLOR_TYPE
      , SPNG_ETRNS_NO_PLTE
      , SPNG_EGAMA
      , SPNG_EICCP_NAME
      , SPNG_EICCP_COMPRESSION_METHOD
      , SPNG_ESBIT
      , SPNG_ESRGB
      , SPNG_ETEXT
      , SPNG_ETEXT_KEYWORD
      , SPNG_EZTXT
      , SPNG_EZTXT_COMPRESSION_METHOD
      , SPNG_EITXT
      , SPNG_EITXT_COMPRESSION_FLAG
      , SPNG_EITXT_COMPRESSION_METHOD
      , SPNG_EITXT_LANG_TAG
      , SPNG_EITXT_TRANSLATED_KEY
      , SPNG_EBKGD_NO_PLTE
      , SPNG_EBKGD_PLTE_IDX
      , SPNG_EHIST_NO_PLTE
      , SPNG_EPHYS
      , SPNG_ESPLT_NAME
      , SPNG_ESPLT_DUP_NAME
      , SPNG_ESPLT_DEPTH
      , SPNG_ETIME
      , SPNG_EOFFS
      , SPNG_EEXIF
      , SPNG_EIDAT_TOO_SHORT
      , SPNG_EIDAT_STREAM
      , SPNG_EZLIB
      , SPNG_EFILTER
      , SPNG_EBUFSIZ
      , SPNG_EIO
      , SPNG_EOF
      , SPNG_EBUF_SET
      , SPNG_EBADSTATE
      , SPNG_EFMT
      , SPNG_EFLAGS
      , SPNG_ECHUNKAVAIL
      , SPNG_ENCODE_ONLY
      , SPNG_EOI
      , SPNG_ENOPLTE
      , SPNG_ECHUNK_LIMITS
      , SPNG_EZLIB_INIT
      , SPNG_ECHUNK_STDLEN
      , SPNG_EINTERNAL
      , SPNG_ECTXTYPE
      , SPNG_ENOSRC
      , SPNG_ENODST
      , SPNG_EOPSTATE
      , SPNG_ENOTFINAL
     :: SpngErrNo
pattern SPNG_IO_ERROR                 = SpngErrNo #const SPNG_IO_ERROR
pattern SPNG_IO_EOF                   = SpngErrNo #const SPNG_IO_EOF
pattern SPNG_OK                       = SpngErrNo #const SPNG_OK
pattern SPNG_EINVAL                   = SpngErrNo #const SPNG_EINVAL
pattern SPNG_EMEM                     = SpngErrNo #const SPNG_EMEM
pattern SPNG_EOVERFLOW                = SpngErrNo #const SPNG_EOVERFLOW
pattern SPNG_ESIGNATURE               = SpngErrNo #const SPNG_ESIGNATURE
pattern SPNG_EWIDTH                   = SpngErrNo #const SPNG_EWIDTH
pattern SPNG_EHEIGHT                  = SpngErrNo #const SPNG_EHEIGHT
pattern SPNG_EUSER_WIDTH              = SpngErrNo #const SPNG_EUSER_WIDTH
pattern SPNG_EUSER_HEIGHT             = SpngErrNo #const SPNG_EUSER_HEIGHT
pattern SPNG_EBIT_DEPTH               = SpngErrNo #const SPNG_EBIT_DEPTH
pattern SPNG_ECOLOR_TYPE              = SpngErrNo #const SPNG_ECOLOR_TYPE
pattern SPNG_ECOMPRESSION_METHOD      = SpngErrNo #const SPNG_ECOMPRESSION_METHOD
pattern SPNG_EFILTER_METHOD           = SpngErrNo #const SPNG_EFILTER_METHOD
pattern SPNG_EINTERLACE_METHOD        = SpngErrNo #const SPNG_EINTERLACE_METHOD
pattern SPNG_EIHDR_SIZE               = SpngErrNo #const SPNG_EIHDR_SIZE
pattern SPNG_ENOIHDR                  = SpngErrNo #const SPNG_ENOIHDR
pattern SPNG_ECHUNK_POS               = SpngErrNo #const SPNG_ECHUNK_POS
pattern SPNG_ECHUNK_SIZE              = SpngErrNo #const SPNG_ECHUNK_SIZE
pattern SPNG_ECHUNK_CRC               = SpngErrNo #const SPNG_ECHUNK_CRC
pattern SPNG_ECHUNK_TYPE              = SpngErrNo #const SPNG_ECHUNK_TYPE
pattern SPNG_ECHUNK_UNKNOWN_CRITICAL  = SpngErrNo #const SPNG_ECHUNK_UNKNOWN_CRITICAL
pattern SPNG_EDUP_PLTE                = SpngErrNo #const SPNG_EDUP_PLTE
pattern SPNG_EDUP_CHRM                = SpngErrNo #const SPNG_EDUP_CHRM
pattern SPNG_EDUP_GAMA                = SpngErrNo #const SPNG_EDUP_GAMA
pattern SPNG_EDUP_ICCP                = SpngErrNo #const SPNG_EDUP_ICCP
pattern SPNG_EDUP_SBIT                = SpngErrNo #const SPNG_EDUP_SBIT
pattern SPNG_EDUP_SRGB                = SpngErrNo #const SPNG_EDUP_SRGB
pattern SPNG_EDUP_BKGD                = SpngErrNo #const SPNG_EDUP_BKGD
pattern SPNG_EDUP_HIST                = SpngErrNo #const SPNG_EDUP_HIST
pattern SPNG_EDUP_TRNS                = SpngErrNo #const SPNG_EDUP_TRNS
pattern SPNG_EDUP_PHYS                = SpngErrNo #const SPNG_EDUP_PHYS
pattern SPNG_EDUP_TIME                = SpngErrNo #const SPNG_EDUP_TIME
pattern SPNG_EDUP_OFFS                = SpngErrNo #const SPNG_EDUP_OFFS
pattern SPNG_EDUP_EXIF                = SpngErrNo #const SPNG_EDUP_EXIF
pattern SPNG_ECHRM                    = SpngErrNo #const SPNG_ECHRM
pattern SPNG_EPLTE_IDX                = SpngErrNo #const SPNG_EPLTE_IDX
pattern SPNG_ETRNS_COLOR_TYPE         = SpngErrNo #const SPNG_ETRNS_COLOR_TYPE
pattern SPNG_ETRNS_NO_PLTE            = SpngErrNo #const SPNG_ETRNS_NO_PLTE
pattern SPNG_EGAMA                    = SpngErrNo #const SPNG_EGAMA
pattern SPNG_EICCP_NAME               = SpngErrNo #const SPNG_EICCP_NAME
pattern SPNG_EICCP_COMPRESSION_METHOD = SpngErrNo #const SPNG_EICCP_COMPRESSION_METHOD
pattern SPNG_ESBIT                    = SpngErrNo #const SPNG_ESBIT
pattern SPNG_ESRGB                    = SpngErrNo #const SPNG_ESRGB
pattern SPNG_ETEXT                    = SpngErrNo #const SPNG_ETEXT
pattern SPNG_ETEXT_KEYWORD            = SpngErrNo #const SPNG_ETEXT_KEYWORD
pattern SPNG_EZTXT                    = SpngErrNo #const SPNG_EZTXT
pattern SPNG_EZTXT_COMPRESSION_METHOD = SpngErrNo #const SPNG_EZTXT_COMPRESSION_METHOD
pattern SPNG_EITXT                    = SpngErrNo #const SPNG_EITXT
pattern SPNG_EITXT_COMPRESSION_FLAG   = SpngErrNo #const SPNG_EITXT_COMPRESSION_FLAG
pattern SPNG_EITXT_COMPRESSION_METHOD = SpngErrNo #const SPNG_EITXT_COMPRESSION_METHOD
pattern SPNG_EITXT_LANG_TAG           = SpngErrNo #const SPNG_EITXT_LANG_TAG
pattern SPNG_EITXT_TRANSLATED_KEY     = SpngErrNo #const SPNG_EITXT_TRANSLATED_KEY
pattern SPNG_EBKGD_NO_PLTE            = SpngErrNo #const SPNG_EBKGD_NO_PLTE
pattern SPNG_EBKGD_PLTE_IDX           = SpngErrNo #const SPNG_EBKGD_PLTE_IDX
pattern SPNG_EHIST_NO_PLTE            = SpngErrNo #const SPNG_EHIST_NO_PLTE
pattern SPNG_EPHYS                    = SpngErrNo #const SPNG_EPHYS
pattern SPNG_ESPLT_NAME               = SpngErrNo #const SPNG_ESPLT_NAME
pattern SPNG_ESPLT_DUP_NAME           = SpngErrNo #const SPNG_ESPLT_DUP_NAME
pattern SPNG_ESPLT_DEPTH              = SpngErrNo #const SPNG_ESPLT_DEPTH
pattern SPNG_ETIME                    = SpngErrNo #const SPNG_ETIME
pattern SPNG_EOFFS                    = SpngErrNo #const SPNG_EOFFS
pattern SPNG_EEXIF                    = SpngErrNo #const SPNG_EEXIF
pattern SPNG_EIDAT_TOO_SHORT          = SpngErrNo #const SPNG_EIDAT_TOO_SHORT
pattern SPNG_EIDAT_STREAM             = SpngErrNo #const SPNG_EIDAT_STREAM
pattern SPNG_EZLIB                    = SpngErrNo #const SPNG_EZLIB
pattern SPNG_EFILTER                  = SpngErrNo #const SPNG_EFILTER
pattern SPNG_EBUFSIZ                  = SpngErrNo #const SPNG_EBUFSIZ
pattern SPNG_EIO                      = SpngErrNo #const SPNG_EIO
pattern SPNG_EOF                      = SpngErrNo #const SPNG_EOF
pattern SPNG_EBUF_SET                 = SpngErrNo #const SPNG_EBUF_SET
pattern SPNG_EBADSTATE                = SpngErrNo #const SPNG_EBADSTATE
pattern SPNG_EFMT                     = SpngErrNo #const SPNG_EFMT
pattern SPNG_EFLAGS                   = SpngErrNo #const SPNG_EFLAGS
pattern SPNG_ECHUNKAVAIL              = SpngErrNo #const SPNG_ECHUNKAVAIL
pattern SPNG_ENCODE_ONLY              = SpngErrNo #const SPNG_ENCODE_ONLY
pattern SPNG_EOI                      = SpngErrNo #const SPNG_EOI
pattern SPNG_ENOPLTE                  = SpngErrNo #const SPNG_ENOPLTE
pattern SPNG_ECHUNK_LIMITS            = SpngErrNo #const SPNG_ECHUNK_LIMITS
pattern SPNG_EZLIB_INIT               = SpngErrNo #const SPNG_EZLIB_INIT
pattern SPNG_ECHUNK_STDLEN            = SpngErrNo #const SPNG_ECHUNK_STDLEN
pattern SPNG_EINTERNAL                = SpngErrNo #const SPNG_EINTERNAL
pattern SPNG_ECTXTYPE                 = SpngErrNo #const SPNG_ECTXTYPE
pattern SPNG_ENOSRC                   = SpngErrNo #const SPNG_ENOSRC
pattern SPNG_ENODST                   = SpngErrNo #const SPNG_ENODST
pattern SPNG_EOPSTATE                 = SpngErrNo #const SPNG_EOPSTATE
pattern SPNG_ENOTFINAL                = SpngErrNo #const SPNG_ENOTFINAL



newtype SpngTextType = SpngTextType { unSpngTextType :: #{type enum spng_text_type} }
                       deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_TEXT
      , SPNG_ZTXT
      , SPNG_ITXT
     :: SpngTextType
pattern SPNG_TEXT = SpngTextType #const SPNG_TEXT
pattern SPNG_ZTXT = SpngTextType #const SPNG_ZTXT
pattern SPNG_ITXT = SpngTextType #const SPNG_ITXT



newtype SpngColorType = SpngColorType { unSpngColorType :: #{type enum spng_color_type} }
                        deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_COLOR_TYPE_GRAYSCALE
      , SPNG_COLOR_TYPE_TRUECOLOR
      , SPNG_COLOR_TYPE_INDEXED
      , SPNG_COLOR_TYPE_GRAYSCALE_ALPHA
      , SPNG_COLOR_TYPE_TRUECOLOR_ALPHA
     :: SpngColorType
pattern SPNG_COLOR_TYPE_GRAYSCALE       = SpngColorType #const SPNG_COLOR_TYPE_GRAYSCALE
pattern SPNG_COLOR_TYPE_TRUECOLOR       = SpngColorType #const SPNG_COLOR_TYPE_TRUECOLOR
pattern SPNG_COLOR_TYPE_INDEXED         = SpngColorType #const SPNG_COLOR_TYPE_INDEXED
pattern SPNG_COLOR_TYPE_GRAYSCALE_ALPHA = SpngColorType #const SPNG_COLOR_TYPE_GRAYSCALE_ALPHA
pattern SPNG_COLOR_TYPE_TRUECOLOR_ALPHA = SpngColorType #const SPNG_COLOR_TYPE_TRUECOLOR_ALPHA



newtype SpngFilter = SpngFilter { unSpngFilter :: #{type enum spng_filter} }
                     deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_FILTER_NONE
      , SPNG_FILTER_SUB
      , SPNG_FILTER_UP
      , SPNG_FILTER_AVERAGE
      , SPNG_FILTER_PAETH
     :: SpngFilter
pattern SPNG_FILTER_NONE    = SpngFilter #const SPNG_FILTER_NONE
pattern SPNG_FILTER_SUB     = SpngFilter #const SPNG_FILTER_SUB
pattern SPNG_FILTER_UP      = SpngFilter #const SPNG_FILTER_UP
pattern SPNG_FILTER_AVERAGE = SpngFilter #const SPNG_FILTER_AVERAGE
pattern SPNG_FILTER_PAETH   = SpngFilter #const SPNG_FILTER_PAETH



newtype SpngFilterChoice = SpngFilterChoice { unSpngFilterChoice :: #{type enum spng_filter_choice} }
                           deriving newtype (Show, Eq, Ord, Storable)

instance Semigroup SpngFilterChoice where
  SpngFilterChoice a <> SpngFilterChoice b = SpngFilterChoice $ a .|. b

pattern SPNG_DISABLE_FILTERING
      , SPNG_FILTER_CHOICE_NONE
      , SPNG_FILTER_CHOICE_SUB
      , SPNG_FILTER_CHOICE_UP
      , SPNG_FILTER_CHOICE_AVG
      , SPNG_FILTER_CHOICE_PAETH
      , SPNG_FILTER_CHOICE_ALL
     :: SpngFilterChoice
pattern SPNG_DISABLE_FILTERING   = SpngFilterChoice #const SPNG_DISABLE_FILTERING
pattern SPNG_FILTER_CHOICE_NONE  = SpngFilterChoice #const SPNG_FILTER_CHOICE_NONE
pattern SPNG_FILTER_CHOICE_SUB   = SpngFilterChoice #const SPNG_FILTER_CHOICE_SUB
pattern SPNG_FILTER_CHOICE_UP    = SpngFilterChoice #const SPNG_FILTER_CHOICE_UP
pattern SPNG_FILTER_CHOICE_AVG   = SpngFilterChoice #const SPNG_FILTER_CHOICE_AVG
pattern SPNG_FILTER_CHOICE_PAETH = SpngFilterChoice #const SPNG_FILTER_CHOICE_PAETH
pattern SPNG_FILTER_CHOICE_ALL   = SpngFilterChoice #const SPNG_FILTER_CHOICE_ALL



newtype SpngInterlaceMethod = SpngInterlaceMethod { unSpngInterlaceMethod :: #{type enum spng_interlace_method} }
                              deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_INTERLACE_NONE
      , SPNG_INTERLACE_ADAM7
     :: SpngInterlaceMethod
pattern SPNG_INTERLACE_NONE  = SpngInterlaceMethod #const SPNG_INTERLACE_NONE
pattern SPNG_INTERLACE_ADAM7 = SpngInterlaceMethod #const SPNG_INTERLACE_ADAM7



newtype SpngFormat = SpngFormat { unSpngFormat :: #{type enum spng_format} }
                     deriving newtype (Show, Eq, Ord, Storable)

instance Semigroup SpngFormat where
  SpngFormat a <> SpngFormat b = SpngFormat $ a .|. b

pattern SPNG_FMT_RGBA8  :: SpngFormat
pattern SPNG_FMT_RGBA16 :: SpngFormat
pattern SPNG_FMT_RGB8   :: SpngFormat
pattern SPNG_FMT_GA8    :: SpngFormat
pattern SPNG_FMT_GA16   :: SpngFormat
pattern SPNG_FMT_G8     :: SpngFormat
-- | host-endian
pattern SPNG_FMT_PNG    :: SpngFormat
-- | big-endian
pattern SPNG_FMT_RAW    :: SpngFormat
pattern SPNG_FMT_RGBA8  = SpngFormat #const SPNG_FMT_RGBA8
pattern SPNG_FMT_RGBA16 = SpngFormat #const SPNG_FMT_RGBA16
pattern SPNG_FMT_RGB8   = SpngFormat #const SPNG_FMT_RGB8
pattern SPNG_FMT_GA8    = SpngFormat #const SPNG_FMT_GA8
pattern SPNG_FMT_GA16   = SpngFormat #const SPNG_FMT_GA16
pattern SPNG_FMT_G8     = SpngFormat #const SPNG_FMT_G8
pattern SPNG_FMT_PNG    = SpngFormat #const SPNG_FMT_PNG
pattern SPNG_FMT_RAW    = SpngFormat #const SPNG_FMT_RAW



newtype SpngCtxFlags = SpngCtxFlags { unSpngCtxFlags :: #{type enum spng_ctx_flags} }
                       deriving newtype (Show, Eq, Ord, Storable)

instance Monoid SpngCtxFlags where
  mempty = SpngCtxFlags 0

instance Semigroup SpngCtxFlags where
  SpngCtxFlags a <> SpngCtxFlags b = SpngCtxFlags $ a .|. b

-- | Ignore checksum in DEFLATE streams
pattern SPNG_CTX_IGNORE_ADLER32 :: SpngCtxFlags
-- | Create an encoder context
pattern SPNG_CTX_ENCODER        :: SpngCtxFlags
pattern SPNG_CTX_IGNORE_ADLER32 = SpngCtxFlags #const SPNG_CTX_IGNORE_ADLER32
pattern SPNG_CTX_ENCODER        = SpngCtxFlags #const SPNG_CTX_ENCODER



newtype SpngDecodeFlags = SpngDecodeFlags { unSpngDecodeFlags :: #{type enum spng_decode_flags} }
                          deriving newtype (Show, Eq, Ord, Storable)

instance Monoid SpngDecodeFlags where
  mempty = SpngDecodeFlags 0

instance Semigroup SpngDecodeFlags where
  SpngDecodeFlags a <> SpngDecodeFlags b = SpngDecodeFlags $ a .|. b

-- | Apply transparency
pattern SPNG_DECODE_TRNS        :: SpngDecodeFlags
-- | Apply gamma correction
pattern SPNG_DECODE_GAMMA       :: SpngDecodeFlags
-- | Undocumented
pattern SPNG_DECODE_USE_SBIT    :: SpngDecodeFlags
-- | Initialize for progressive reads
pattern SPNG_DECODE_PROGRESSIVE :: SpngDecodeFlags
pattern SPNG_DECODE_TRNS        = SpngDecodeFlags #const SPNG_DECODE_TRNS
pattern SPNG_DECODE_GAMMA       = SpngDecodeFlags #const SPNG_DECODE_GAMMA
pattern SPNG_DECODE_USE_SBIT    = SpngDecodeFlags #const SPNG_DECODE_USE_SBIT
pattern SPNG_DECODE_PROGRESSIVE = SpngDecodeFlags #const SPNG_DECODE_PROGRESSIVE



newtype SpngCrcAction = SpngCrcAction { unSpngCrcAction :: #{type enum spng_crc_action} }
                        deriving newtype (Show, Eq, Ord, Storable)

-- | Default for critical chunks
pattern SPNG_CRC_ERROR   :: SpngCrcAction
-- | Discard chunk, invalid for critical chunks.
--
--   Since v0.6.2: default for ancillary chunks
pattern SPNG_CRC_DISCARD :: SpngCrcAction
-- | Ignore and don't calculate checksum.
--
--   Since v0.6.2: also ignores checksums in DEFLATE streams
pattern SPNG_CRC_USE     :: SpngCrcAction
pattern SPNG_CRC_ERROR   = SpngCrcAction #const SPNG_CRC_ERROR
pattern SPNG_CRC_DISCARD = SpngCrcAction #const SPNG_CRC_DISCARD
pattern SPNG_CRC_USE     = SpngCrcAction #const SPNG_CRC_USE



newtype SpngEncodeFlags = SpngEncodeFlags { unSpngEncodeFlags :: #{type enum spng_encode_flags} }
                          deriving newtype (Show, Eq, Ord, Storable)

instance Monoid SpngEncodeFlags where
  mempty = SpngEncodeFlags 0

instance Semigroup SpngEncodeFlags where
  SpngEncodeFlags a <> SpngEncodeFlags b = SpngEncodeFlags $ a .|. b

-- | Initialize for progressive writes
pattern SPNG_ENCODE_PROGRESSIVE :: SpngEncodeFlags
-- | Finalize PNG after encoding image
pattern SPNG_ENCODE_FINALIZE    :: SpngEncodeFlags
pattern SPNG_ENCODE_PROGRESSIVE = SpngEncodeFlags #const SPNG_ENCODE_PROGRESSIVE
pattern SPNG_ENCODE_FINALIZE    = SpngEncodeFlags #const SPNG_ENCODE_FINALIZE



data SpngIhdr =
       SpngIhdr
         { siWidth              :: #type uint32_t
         , siHeight             :: #type uint32_t
         , siBit_depth          :: #type uint8_t
         , siColor_type         :: SpngColorType
         , siCompression_method :: #type uint8_t
         , siFilter_method      :: SpngFilter
         , siInterlace_method   :: SpngInterlaceMethod
         }

instance Offset "siWidth"              SpngIhdr where rawOffset = #offset struct spng_ihdr, width
instance Offset "siHeight"             SpngIhdr where rawOffset = #offset struct spng_ihdr, height
instance Offset "siBit_depth"          SpngIhdr where rawOffset = #offset struct spng_ihdr, bit_depth
instance Offset "siColor_type"         SpngIhdr where rawOffset = #offset struct spng_ihdr, color_type
instance Offset "siCompression_method" SpngIhdr where rawOffset = #offset struct spng_ihdr, compression_method
instance Offset "siFilter_method"      SpngIhdr where rawOffset = #offset struct spng_ihdr, filter_method
instance Offset "siInterlace_method"   SpngIhdr where rawOffset = #offset struct spng_ihdr, interlace_method

instance Storable SpngIhdr where
  sizeOf    _ = #size      struct spng_ihdr
  alignment _ = #alignment struct spng_ihdr

  peek ptr =
    SpngIhdr
      <$> peek (offset @"siWidth"              ptr)
      <*> peek (offset @"siHeight"             ptr)
      <*> peek (offset @"siBit_depth"          ptr)
      <*> peek (offset @"siColor_type"         ptr)
      <*> peek (offset @"siCompression_method" ptr)
      <*> peek (offset @"siFilter_method"      ptr)
      <*> peek (offset @"siInterlace_method"   ptr)

  poke ptr val = do
    pokeField @"siWidth"              ptr val
    pokeField @"siHeight"             ptr val
    pokeField @"siBit_depth"          ptr val
    pokeField @"siColor_type"         ptr val
    pokeField @"siCompression_method" ptr val
    pokeField @"siFilter_method"      ptr val
    pokeField @"siInterlace_method"   ptr val



data SpngPlteEntry =
       SpngPlteEntry
         { speRed   :: #type uint8_t
         , speGreen :: #type uint8_t
         , speBlue  :: #type uint8_t
         , speAlpha :: #{type uint8_t} -- ^ Reserved for internal use
         }

instance Offset "speRed"   SpngPlteEntry where rawOffset = #offset struct spng_plte_entry, red
instance Offset "speGreen" SpngPlteEntry where rawOffset = #offset struct spng_plte_entry, green
instance Offset "speBlue"  SpngPlteEntry where rawOffset = #offset struct spng_plte_entry, blue
instance Offset "speAlpha" SpngPlteEntry where rawOffset = #offset struct spng_plte_entry, alpha

instance Storable SpngPlteEntry where
  sizeOf    _ = #size      struct spng_plte_entry
  alignment _ = #alignment struct spng_plte_entry

  peek ptr =
    SpngPlteEntry
      <$> peek (offset @"speRed"   ptr)
      <*> peek (offset @"speGreen" ptr)
      <*> peek (offset @"speBlue"  ptr)
      <*> peek (offset @"speAlpha" ptr)

  poke ptr val = do
    pokeField @"speRed"   ptr val
    pokeField @"speGreen" ptr val
    pokeField @"speBlue"  ptr val
    pokeField @"speAlpha" ptr val



data SpngPlte =
       SpngPlte
         { spN_entries :: #type uint32_t
         , spEntries   :: Ptr SpngPlteEntry -- ^ Array of 256 elements
         }

instance Offset "spN_entries" SpngPlte where rawOffset = #offset struct spng_plte, n_entries
instance Offset "spEntries"   SpngPlte where rawOffset = #offset struct spng_plte, entries

instance Storable SpngPlte where
  sizeOf    _ = #size      struct spng_plte
  alignment _ = #alignment struct spng_plte

  peek ptr =
    SpngPlte
      <$> peek (offset @"spN_entries" ptr)
      <*> peek (offset @"spEntries"   ptr)

  poke ptr val = do
    pokeField @"spN_entries" ptr val
    pokeField @"spEntries"   ptr val



data SpngTrns =
       SpngTrns
         { stGray            :: #type uint16_t
         , stRed             :: #type uint16_t
         , stGreen           :: #type uint16_t
         , stBlue            :: #type uint16_t
         , stN_type3_entries :: #type uint32_t
         , stType3_alpha     :: Ptr #{type uint8_t} -- ^ Array of 256 elements
         }

instance Offset "stGray"            SpngTrns where rawOffset = #offset struct spng_trns, gray
instance Offset "stRed"             SpngTrns where rawOffset = #offset struct spng_trns, red
instance Offset "stGreen"           SpngTrns where rawOffset = #offset struct spng_trns, green
instance Offset "stBlue"            SpngTrns where rawOffset = #offset struct spng_trns, blue
instance Offset "stN_type3_entries" SpngTrns where rawOffset = #offset struct spng_trns, n_type3_entries
instance Offset "stType3_alpha"     SpngTrns where rawOffset = #offset struct spng_trns, type3_alpha

instance Storable SpngTrns where
  sizeOf    _ = #size      struct spng_trns
  alignment _ = #alignment struct spng_trns

  peek ptr =
    SpngTrns
      <$> peek (offset @"stGray"            ptr)
      <*> peek (offset @"stRed"             ptr)
      <*> peek (offset @"stGreen"           ptr)
      <*> peek (offset @"stBlue"            ptr)
      <*> peek (offset @"stN_type3_entries" ptr)
      <*> peek (offset @"stType3_alpha"     ptr)

  poke ptr val = do
    pokeField @"stGray"            ptr val
    pokeField @"stRed"             ptr val
    pokeField @"stGreen"           ptr val
    pokeField @"stBlue"            ptr val
    pokeField @"stN_type3_entries" ptr val
    pokeField @"stType3_alpha"     ptr val



data SpngChrmInt =
       SpngChrmInt
         { sciWhite_point_x :: #type uint32_t
         , sciWhite_point_y :: #type uint32_t
         , sciRed_x         :: #type uint32_t
         , sciRed_y         :: #type uint32_t
         , sciGreen_x       :: #type uint32_t
         , sciGreen_y       :: #type uint32_t
         , sciBlue_x        :: #type uint32_t
         , sciBlue_y        :: #type uint32_t
         }

instance Offset "sciWhite_point_x" SpngChrmInt where rawOffset = #offset struct spng_chrm_int, white_point_x
instance Offset "sciWhite_point_y" SpngChrmInt where rawOffset = #offset struct spng_chrm_int, white_point_y
instance Offset "sciRed_x"         SpngChrmInt where rawOffset = #offset struct spng_chrm_int, red_x
instance Offset "sciRed_y"         SpngChrmInt where rawOffset = #offset struct spng_chrm_int, red_y
instance Offset "sciGreen_x"       SpngChrmInt where rawOffset = #offset struct spng_chrm_int, green_x
instance Offset "sciGreen_y"       SpngChrmInt where rawOffset = #offset struct spng_chrm_int, green_y
instance Offset "sciBlue_x"        SpngChrmInt where rawOffset = #offset struct spng_chrm_int, blue_x
instance Offset "sciBlue_y"        SpngChrmInt where rawOffset = #offset struct spng_chrm_int, blue_y

instance Storable SpngChrmInt where
  sizeOf    _ = #size      struct spng_chrm_int
  alignment _ = #alignment struct spng_chrm_int

  peek ptr =
    SpngChrmInt
      <$> peek (offset @"sciWhite_point_x" ptr)
      <*> peek (offset @"sciWhite_point_y" ptr)
      <*> peek (offset @"sciRed_x"         ptr)
      <*> peek (offset @"sciRed_y"         ptr)
      <*> peek (offset @"sciGreen_x"       ptr)
      <*> peek (offset @"sciGreen_y"       ptr)
      <*> peek (offset @"sciBlue_x"        ptr)
      <*> peek (offset @"sciBlue_y"        ptr)

  poke ptr val = do
    pokeField @"sciWhite_point_x" ptr val
    pokeField @"sciWhite_point_y" ptr val
    pokeField @"sciRed_x"         ptr val
    pokeField @"sciRed_y"         ptr val
    pokeField @"sciGreen_x"       ptr val
    pokeField @"sciGreen_y"       ptr val
    pokeField @"sciBlue_x"        ptr val
    pokeField @"sciBlue_y"        ptr val



data SpngChrm =
       SpngChrm
         { scWhite_point_x :: #type double
         , scWhite_point_y :: #type double
         , scRed_x         :: #type double
         , scRed_y         :: #type double
         , scGreen_x       :: #type double
         , scGreen_y       :: #type double
         , scBlue_x        :: #type double
         , scBlue_y        :: #type double
         }

instance Offset "scWhite_point_x" SpngChrm where rawOffset = #offset struct spng_chrm, white_point_x
instance Offset "scWhite_point_y" SpngChrm where rawOffset = #offset struct spng_chrm, white_point_y
instance Offset "scRed_x"         SpngChrm where rawOffset = #offset struct spng_chrm, red_x
instance Offset "scRed_y"         SpngChrm where rawOffset = #offset struct spng_chrm, red_y
instance Offset "scGreen_x"       SpngChrm where rawOffset = #offset struct spng_chrm, green_x
instance Offset "scGreen_y"       SpngChrm where rawOffset = #offset struct spng_chrm, green_y
instance Offset "scBlue_x"        SpngChrm where rawOffset = #offset struct spng_chrm, blue_x
instance Offset "scBlue_y"        SpngChrm where rawOffset = #offset struct spng_chrm, blue_y

instance Storable SpngChrm where
  sizeOf    _ = #size      struct spng_chrm
  alignment _ = #alignment struct spng_chrm

  peek ptr =
    SpngChrm
      <$> peek (offset @"scWhite_point_x" ptr)
      <*> peek (offset @"scWhite_point_y" ptr)
      <*> peek (offset @"scRed_x"         ptr)
      <*> peek (offset @"scRed_y"         ptr)
      <*> peek (offset @"scGreen_x"       ptr)
      <*> peek (offset @"scGreen_y"       ptr)
      <*> peek (offset @"scBlue_x"        ptr)
      <*> peek (offset @"scBlue_y"        ptr)

  poke ptr val = do
    pokeField @"scWhite_point_x" ptr val
    pokeField @"scWhite_point_y" ptr val
    pokeField @"scRed_x"         ptr val
    pokeField @"scRed_y"         ptr val
    pokeField @"scGreen_x"       ptr val
    pokeField @"scGreen_y"       ptr val
    pokeField @"scBlue_x"        ptr val
    pokeField @"scBlue_y"        ptr val




data SpngIccp =
       SpngIccp
         { siProfile_name :: Ptr #{type char} -- ^ Array of 80 elements
         , siProfile_len  :: #type size_t
         , siProfile      :: Ptr #type char
         }

instance Offset "siProfile_name" SpngIccp where rawOffset = #offset struct spng_iccp, profile_name
instance Offset "siProfile_len"  SpngIccp where rawOffset = #offset struct spng_iccp, profile_len
instance Offset "siProfile"      SpngIccp where rawOffset = #offset struct spng_iccp, profile

instance Storable SpngIccp where
  sizeOf    _ = #size      struct spng_iccp
  alignment _ = #alignment struct spng_iccp

  peek ptr =
    SpngIccp
      <$> peek (offset @"siProfile_name" ptr)
      <*> peek (offset @"siProfile_len"  ptr)
      <*> peek (offset @"siProfile"      ptr)

  poke ptr val = do
    pokeField @"siProfile_name" ptr val
    pokeField @"siProfile_len"  ptr val
    pokeField @"siProfile"      ptr val



data SpngSbit =
       SpngSbit
         { ssGrayscale_bits :: #type uint8_t
         , ssRed_bits       :: #type uint8_t
         , ssGreen_bits     :: #type uint8_t
         , ssBlue_bits      :: #type uint8_t
         , ssAlpha_bits     :: #type uint8_t
         }

instance Offset "ssGrayscale_bits" SpngSbit where rawOffset = #offset struct spng_sbit, grayscale_bits
instance Offset "ssRed_bits"       SpngSbit where rawOffset = #offset struct spng_sbit, red_bits
instance Offset "ssGreen_bits"     SpngSbit where rawOffset = #offset struct spng_sbit, green_bits
instance Offset "ssBlue_bits"      SpngSbit where rawOffset = #offset struct spng_sbit, blue_bits
instance Offset "ssAlpha_bits"     SpngSbit where rawOffset = #offset struct spng_sbit, alpha_bits

instance Storable SpngSbit where
  sizeOf    _ = #size      struct spng_sbit
  alignment _ = #alignment struct spng_sbit

  peek ptr =
    SpngSbit
      <$> peek (offset @"ssGrayscale_bits" ptr)
      <*> peek (offset @"ssRed_bits"       ptr)
      <*> peek (offset @"ssGreen_bits"     ptr)
      <*> peek (offset @"ssBlue_bits"      ptr)
      <*> peek (offset @"ssAlpha_bits"     ptr)

  poke ptr val = do
    pokeField @"ssGrayscale_bits" ptr val
    pokeField @"ssRed_bits"       ptr val
    pokeField @"ssGreen_bits"     ptr val
    pokeField @"ssBlue_bits"      ptr val
    pokeField @"ssAlpha_bits"     ptr val



data SpngText =
       SpngText
         { stKeyword            :: Ptr #{type char} -- ^ Array of 80 elements
         , stType               :: SpngTextType
         , stLength             :: #type size_t
         , stText               :: Ptr #type char
         , stCompression_flag   :: #{type uint8_t}  -- ^ iTXt only
         , stCompression_method :: #{type uint8_t}  -- ^ iTXt, ztXt only
         , stLanguage_tag       :: Ptr #{type char} -- ^ iTXt only
         , stTranslated_keyword :: Ptr #{type char} -- ^ iTXt only
         }

instance Offset "stKeyword"            SpngText where rawOffset = #offset struct spng_text, keyword
instance Offset "stType"               SpngText where rawOffset = #offset struct spng_text, type
instance Offset "stLength"             SpngText where rawOffset = #offset struct spng_text, length
instance Offset "stText"               SpngText where rawOffset = #offset struct spng_text, text
instance Offset "stCompression_flag"   SpngText where rawOffset = #offset struct spng_text, compression_flag
instance Offset "stCompression_method" SpngText where rawOffset = #offset struct spng_text, compression_method
instance Offset "stLanguage_tag"       SpngText where rawOffset = #offset struct spng_text, language_tag
instance Offset "stTranslated_keyword" SpngText where rawOffset = #offset struct spng_text, translated_keyword

instance Storable SpngText where
  sizeOf    _ = #size      struct spng_text
  alignment _ = #alignment struct spng_text

  peek ptr =
    SpngText
      <$> peek (offset @"stKeyword"            ptr)
      <*> peek (offset @"stType"               ptr)
      <*> peek (offset @"stLength"             ptr)
      <*> peek (offset @"stText"               ptr)
      <*> peek (offset @"stCompression_flag"   ptr)
      <*> peek (offset @"stCompression_method" ptr)
      <*> peek (offset @"stLanguage_tag"       ptr)
      <*> peek (offset @"stTranslated_keyword" ptr)

  poke ptr val = do
    pokeField @"stKeyword"            ptr val
    pokeField @"stType"               ptr val
    pokeField @"stLength"             ptr val
    pokeField @"stText"               ptr val
    pokeField @"stCompression_flag"   ptr val
    pokeField @"stCompression_method" ptr val
    pokeField @"stLanguage_tag"       ptr val
    pokeField @"stTranslated_keyword" ptr val



data SpngBkgd =
       SpngBkgd
         { sbGray       :: #{type uint16_t} -- ^ Only for gray/gray alpha
         , sbRed        :: #type uint16_t
         , sbGreen      :: #type uint16_t
         , sbBlue       :: #type uint16_t
         , sbPlte_index :: #{type uint16_t} -- ^ Only for indexed color
         }

instance Offset "sbGray"       SpngBkgd where rawOffset = #offset struct spng_bkgd, gray
instance Offset "sbRed"        SpngBkgd where rawOffset = #offset struct spng_bkgd, red
instance Offset "sbGreen"      SpngBkgd where rawOffset = #offset struct spng_bkgd, green
instance Offset "sbBlue"       SpngBkgd where rawOffset = #offset struct spng_bkgd, blue
instance Offset "sbPlte_index" SpngBkgd where rawOffset = #offset struct spng_bkgd, plte_index

instance Storable SpngBkgd where
  sizeOf    _ = #size      struct spng_bkgd
  alignment _ = #alignment struct spng_bkgd

  peek ptr =
    SpngBkgd
      <$> peek (offset @"sbGray"       ptr)
      <*> peek (offset @"sbRed"        ptr)
      <*> peek (offset @"sbGreen"      ptr)
      <*> peek (offset @"sbBlue"       ptr)
      <*> peek (offset @"sbPlte_index" ptr)

  poke ptr val = do
    pokeField @"sbGray"       ptr val
    pokeField @"sbRed"        ptr val
    pokeField @"sbGreen"      ptr val
    pokeField @"sbBlue"       ptr val
    pokeField @"sbPlte_index" ptr val



newtype SpngHist =
       SpngHist
         { shFrequency :: Ptr #{type uint16_t} -- ^ Array of 256 elements
         }

instance Offset "shFrequency" SpngHist where rawOffset = #offset struct spng_hist, frequency

instance Storable SpngHist where
  sizeOf    _ = #size      struct spng_hist
  alignment _ = #alignment struct spng_hist

  peek ptr =
    SpngHist
      <$> peek (offset @"shFrequency" ptr)

  poke ptr val =
    pokeField @"shFrequency" ptr val



data SpngPhys =
       SpngPhys
         { spPpu_x          :: #type int32_t
         , spPpu_y          :: #type int32_t
         , spUnit_specifier :: #type uint8_t
         }

instance Offset "spPpu_x"          SpngPhys where rawOffset = #offset struct spng_phys, ppu_x
instance Offset "spPpu_y"          SpngPhys where rawOffset = #offset struct spng_phys, ppu_y
instance Offset "spUnit_specifier" SpngPhys where rawOffset = #offset struct spng_phys, unit_specifier

instance Storable SpngPhys where
  sizeOf    _ = #size      struct spng_phys
  alignment _ = #alignment struct spng_phys

  peek ptr =
    SpngPhys
      <$> peek (offset @"spPpu_x"          ptr)
      <*> peek (offset @"spPpu_y"          ptr)
      <*> peek (offset @"spUnit_specifier" ptr)

  poke ptr val = do
    pokeField @"spPpu_x"          ptr val
    pokeField @"spPpu_y"          ptr val
    pokeField @"spUnit_specifier" ptr val




data SpngSpltEntry =
       SpngSpltEntry
         { sseRed       :: #type uint16_t
         , sseGreen     :: #type uint16_t
         , sseBlue      :: #type uint16_t
         , sseAlpha     :: #type uint16_t
         , sseFrequency :: #type uint16_t
         }

instance Offset "sseRed"       SpngSpltEntry where rawOffset = #offset struct spng_splt_entry, red
instance Offset "sseGreen"     SpngSpltEntry where rawOffset = #offset struct spng_splt_entry, green
instance Offset "sseBlue"      SpngSpltEntry where rawOffset = #offset struct spng_splt_entry, blue
instance Offset "sseAlpha"     SpngSpltEntry where rawOffset = #offset struct spng_splt_entry, alpha
instance Offset "sseFrequency" SpngSpltEntry where rawOffset = #offset struct spng_splt_entry, frequency

instance Storable SpngSpltEntry where
  sizeOf    _ = #size      struct spng_splt_entry
  alignment _ = #alignment struct spng_splt_entry

  peek ptr =
    SpngSpltEntry
      <$> peek (offset @"sseRed"       ptr)
      <*> peek (offset @"sseGreen"     ptr)
      <*> peek (offset @"sseBlue"      ptr)
      <*> peek (offset @"sseAlpha"     ptr)
      <*> peek (offset @"sseFrequency" ptr)

  poke ptr val = do
    pokeField @"sseRed"       ptr val
    pokeField @"sseGreen"     ptr val
    pokeField @"sseBlue"      ptr val
    pokeField @"sseAlpha"     ptr val
    pokeField @"sseFrequency" ptr val



data SpngSplt =
       SpngSplt
         { ssName         :: Ptr #{type char} -- ^ Array of 80 elements
         , ssSample_depth :: #type uint8_t
         , ssN_entries    :: #type uint32_t
         , ssEntries      :: Ptr SpngSpltEntry
         }

instance Offset "ssName"         SpngSplt where rawOffset = #offset struct spng_splt, name
instance Offset "ssSample_depth" SpngSplt where rawOffset = #offset struct spng_splt, sample_depth
instance Offset "ssN_entries"    SpngSplt where rawOffset = #offset struct spng_splt, n_entries
instance Offset "ssEntries"      SpngSplt where rawOffset = #offset struct spng_splt, entries

instance Storable SpngSplt where
  sizeOf    _ = #size      struct spng_splt
  alignment _ = #alignment struct spng_splt

  peek ptr =
    SpngSplt
      <$> peek (offset @"ssName"         ptr)
      <*> peek (offset @"ssSample_depth" ptr)
      <*> peek (offset @"ssN_entries"    ptr)
      <*> peek (offset @"ssEntries"      ptr)

  poke ptr val = do
    pokeField @"ssName"         ptr val
    pokeField @"ssSample_depth" ptr val
    pokeField @"ssN_entries"    ptr val
    pokeField @"ssEntries"      ptr val



data SpngTime =
       SpngTime
         { stYear   :: #type uint16_t
         , stMonth  :: #type uint8_t
         , stDay    :: #type uint8_t
         , stHour   :: #type uint8_t
         , stMinute :: #type uint8_t
         , stSecond :: #type uint8_t
         }

instance Offset "stYear"   SpngTime where rawOffset = #offset struct spng_time, year
instance Offset "stMonth"  SpngTime where rawOffset = #offset struct spng_time, month
instance Offset "stDay"    SpngTime where rawOffset = #offset struct spng_time, day
instance Offset "stHour"   SpngTime where rawOffset = #offset struct spng_time, hour
instance Offset "stMinute" SpngTime where rawOffset = #offset struct spng_time, minute
instance Offset "stSecond" SpngTime where rawOffset = #offset struct spng_time, second

instance Storable SpngTime where
  sizeOf    _ = #size      struct spng_time
  alignment _ = #alignment struct spng_time

  peek ptr =
    SpngTime
      <$> peek (offset @"stYear"   ptr)
      <*> peek (offset @"stMonth"  ptr)
      <*> peek (offset @"stDay"    ptr)
      <*> peek (offset @"stHour"   ptr)
      <*> peek (offset @"stMinute" ptr)
      <*> peek (offset @"stSecond" ptr)

  poke ptr val = do
    pokeField @"stYear"   ptr val
    pokeField @"stMonth"  ptr val
    pokeField @"stDay"    ptr val
    pokeField @"stHour"   ptr val
    pokeField @"stMinute" ptr val
    pokeField @"stSecond" ptr val



data SpngOffs =
       SpngOffs
         { soX              :: #type int32_t
         , soY              :: #type int32_t
         , soUnit_specifier :: #type uint8_t
         }

instance Offset "soX"              SpngOffs where rawOffset = #offset struct spng_offs, x
instance Offset "soY"              SpngOffs where rawOffset = #offset struct spng_offs, y
instance Offset "soUnit_specifier" SpngOffs where rawOffset = #offset struct spng_offs, unit_specifier

instance Storable SpngOffs where
  sizeOf    _ = #size      struct spng_offs
  alignment _ = #alignment struct spng_offs

  peek ptr =
    SpngOffs
      <$> peek (offset @"soX"              ptr)
      <*> peek (offset @"soY"              ptr)
      <*> peek (offset @"soUnit_specifier" ptr)

  poke ptr val = do
    pokeField @"soX"              ptr val
    pokeField @"soY"              ptr val
    pokeField @"soUnit_specifier" ptr val



data SpngExif =
       SpngExif
         { seLength :: Ptr #type size_t
         , seData   :: Ptr #type char
         }

instance Offset "seLength" SpngExif where rawOffset = #offset struct spng_exif, length
instance Offset "seData"   SpngExif where rawOffset = #offset struct spng_exif, data

instance Storable SpngExif where
  sizeOf    _ = #size      struct spng_exif
  alignment _ = #alignment struct spng_exif

  peek ptr =
    SpngExif
      <$> peek (offset @"seLength" ptr)
      <*> peek (offset @"seData"   ptr)

  poke ptr val = do
    pokeField @"seLength" ptr val
    pokeField @"seData"   ptr val



data SpngChunk =
       SpngChunk
         { scOffset :: #type size_t
         , scLength :: #type uint32_t
         , scType   :: Ptr #{type uint8_t} -- ^ Array of four elements
         , scCrc    :: #type uint32_t
         }

instance Offset "scOffset" SpngChunk where rawOffset = #offset struct spng_chunk, offset
instance Offset "scLength" SpngChunk where rawOffset = #offset struct spng_chunk, length
instance Offset "scType"   SpngChunk where rawOffset = #offset struct spng_chunk, type
instance Offset "scCrc"    SpngChunk where rawOffset = #offset struct spng_chunk, crc

instance Storable SpngChunk where
  sizeOf    _ = #size      struct spng_chunk
  alignment _ = #alignment struct spng_chunk

  peek ptr =
    SpngChunk
      <$> peek (offset @"scOffset" ptr)
      <*> peek (offset @"scLength" ptr)
      <*> peek (offset @"scType"   ptr)
      <*> peek (offset @"scCrc"    ptr)

  poke ptr val = do
    pokeField @"scOffset" ptr val
    pokeField @"scLength" ptr val
    pokeField @"scType"   ptr val
    pokeField @"scCrc"    ptr val



newtype SpngLocation = SpngLocation { unSpngLocation :: #{type enum spng_location} }
                       deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_AFTER_IHDR
      , SPNG_AFTER_PLTE
      , SPNG_AFTER_IDAT
     :: SpngLocation
pattern SPNG_AFTER_IHDR = SpngLocation #const SPNG_AFTER_IHDR
pattern SPNG_AFTER_PLTE = SpngLocation #const SPNG_AFTER_PLTE
pattern SPNG_AFTER_IDAT = SpngLocation #const SPNG_AFTER_IDAT



data SpngUnknownChunk =
       SpngUnknownChunk
         { sucType     :: Ptr #{type uint8_t} -- ^ Array of four elements
         , sucLength   :: #type size_t
         , sucData     :: Ptr ()
         , sucLocation :: SpngLocation
         }

instance Offset "sucType"     SpngUnknownChunk where rawOffset = #offset struct spng_unknown_chunk, type
instance Offset "sucLength"   SpngUnknownChunk where rawOffset = #offset struct spng_unknown_chunk, length
instance Offset "sucData"     SpngUnknownChunk where rawOffset = #offset struct spng_unknown_chunk, data
instance Offset "sucLocation" SpngUnknownChunk where rawOffset = #offset struct spng_unknown_chunk, location

instance Storable SpngUnknownChunk where
  sizeOf    _ = #size      struct spng_unknown_chunk
  alignment _ = #alignment struct spng_unknown_chunk

  peek ptr =
    SpngUnknownChunk
      <$> peek (offset @"sucType"     ptr)
      <*> peek (offset @"sucLength"   ptr)
      <*> peek (offset @"sucData"     ptr)
      <*> peek (offset @"sucLocation" ptr)

  poke ptr val = do
    pokeField @"sucType"     ptr val
    pokeField @"sucLength"   ptr val
    pokeField @"sucData"     ptr val
    pokeField @"sucLocation" ptr val



newtype SpngOption = SpngOption { unSpngOption :: #{type enum spng_option} }
                     deriving newtype (Show, Eq, Ord, Storable)

pattern SPNG_KEEP_UNKNOWN_CHUNKS
      , SPNG_IMG_COMPRESSION_LEVEL
      , SPNG_IMG_WINDOW_BITS
      , SPNG_IMG_MEM_LEVEL
      , SPNG_IMG_COMPRESSION_STRATEGY
      , SPNG_TEXT_COMPRESSION_LEVEL
      , SPNG_TEXT_WINDOW_BITS
      , SPNG_TEXT_MEM_LEVEL
      , SPNG_TEXT_COMPRESSION_STRATEGY
      , SPNG_FILTER_CHOICE
      , SPNG_CHUNK_COUNT_LIMIT
      , SPNG_ENCODE_TO_BUFFER
     :: SpngOption
pattern SPNG_KEEP_UNKNOWN_CHUNKS       = SpngOption #const SPNG_KEEP_UNKNOWN_CHUNKS
pattern SPNG_IMG_COMPRESSION_LEVEL     = SpngOption #const SPNG_IMG_COMPRESSION_LEVEL
pattern SPNG_IMG_WINDOW_BITS           = SpngOption #const SPNG_IMG_WINDOW_BITS
pattern SPNG_IMG_MEM_LEVEL             = SpngOption #const SPNG_IMG_MEM_LEVEL
pattern SPNG_IMG_COMPRESSION_STRATEGY  = SpngOption #const SPNG_IMG_COMPRESSION_STRATEGY
pattern SPNG_TEXT_COMPRESSION_LEVEL    = SpngOption #const SPNG_TEXT_COMPRESSION_LEVEL
pattern SPNG_TEXT_WINDOW_BITS          = SpngOption #const SPNG_TEXT_WINDOW_BITS
pattern SPNG_TEXT_MEM_LEVEL            = SpngOption #const SPNG_TEXT_MEM_LEVEL
pattern SPNG_TEXT_COMPRESSION_STRATEGY = SpngOption #const SPNG_TEXT_COMPRESSION_STRATEGY
pattern SPNG_FILTER_CHOICE             = SpngOption #const SPNG_FILTER_CHOICE
pattern SPNG_CHUNK_COUNT_LIMIT         = SpngOption #const SPNG_CHUNK_COUNT_LIMIT
pattern SPNG_ENCODE_TO_BUFFER          = SpngOption #const SPNG_ENCODE_TO_BUFFER



type SpngMallocFn  = #{type size_t} -- ^ size
                  -> IO (Ptr ())

foreign import ccall "wrapper"
  mkSpngMallocFn :: SpngMallocFn -> IO (FunPtr SpngMallocFn)

type SpngReallocFn = Ptr ()         -- ^ ptr
                  -> #{type size_t} -- ^ size
                  -> IO (Ptr ())

foreign import ccall "wrapper"
  mkSpngReallocFn :: SpngReallocFn -> IO (FunPtr SpngReallocFn)

type SpngCallocFn  = #{type size_t} -- ^ count
                  -> #{type size_t} -- ^ size
                  -> IO (Ptr ())

foreign import ccall "wrapper"
  mkSpngCallocFn :: SpngCallocFn -> IO (FunPtr SpngCallocFn)

type SpngFreeFn    = Ptr ()         -- ^ ptr
                  -> IO ()

foreign import ccall "wrapper"
  mkSpngFreeFn :: SpngFreeFn -> IO (FunPtr SpngFreeFn)



data SpngAlloc =
       SpngAlloc
         { saMalloc_fn  :: FunPtr SpngMallocFn
         , saRealloc_fn :: FunPtr SpngReallocFn
         , saCalloc_fn  :: FunPtr SpngCallocFn
         , saFree_fn    :: FunPtr SpngFreeFn
         }

instance Offset "saMalloc_fn"  SpngAlloc where rawOffset = #offset struct spng_alloc, malloc_fn
instance Offset "saRealloc_fn" SpngAlloc where rawOffset = #offset struct spng_alloc, realloc_fn
instance Offset "saCalloc_fn"  SpngAlloc where rawOffset = #offset struct spng_alloc, calloc_fn
instance Offset "saFree_fn"    SpngAlloc where rawOffset = #offset struct spng_alloc, free_fn

instance Storable SpngAlloc where
  sizeOf    _ = #size      struct spng_alloc
  alignment _ = #alignment struct spng_alloc

  peek ptr =
    SpngAlloc
      <$> peek (offset @"saMalloc_fn"  ptr)
      <*> peek (offset @"saRealloc_fn" ptr)
      <*> peek (offset @"saCalloc_fn"  ptr)
      <*> peek (offset @"saFree_fn"    ptr)

  poke ptr val = do
    pokeField @"saMalloc_fn"  ptr val
    pokeField @"saRealloc_fn" ptr val
    pokeField @"saCalloc_fn"  ptr val
    pokeField @"saFree_fn"    ptr val



data SpngRowInfo =
       SpngRowInfo
         { sriScanline_idx :: #type uint32_t
         , sriRow_num      :: #{type uint32_t} -- ^ deinterlaced row index
         , sriPass         :: #type int
         , sriFilter       :: SpngFilter
         }

instance Offset "sriScanline_idx" SpngRowInfo where rawOffset = #offset struct spng_row_info, scanline_idx
instance Offset "sriRow_num"      SpngRowInfo where rawOffset = #offset struct spng_row_info, row_num
instance Offset "sriPass"         SpngRowInfo where rawOffset = #offset struct spng_row_info, pass
instance Offset "sriFilter"       SpngRowInfo where rawOffset = #offset struct spng_row_info, filter

instance Storable SpngRowInfo where
  sizeOf    _ = #size      struct spng_row_info
  alignment _ = #alignment struct spng_row_info

  peek ptr =
    SpngRowInfo
      <$> peek (offset @"sriScanline_idx" ptr)
      <*> peek (offset @"sriRow_num"      ptr)
      <*> peek (offset @"sriPass"         ptr)
      <*> peek (offset @"sriFilter"       ptr)

  poke ptr val = do
    pokeField @"sriScanline_idx" ptr val
    pokeField @"sriRow_num"      ptr val
    pokeField @"sriPass"         ptr val
    pokeField @"sriFilter"       ptr val



data SpngCtx



type SpngReadFn = Ptr SpngCtx    -- ^ ctx
               -> Ptr ()         -- ^ user
               -> Ptr ()         -- ^ dest
               -> #{type size_t} -- ^ length
               -> IO SpngErrNo

mkSpngReadFn :: SpngReadFn -> IO (FunPtr SpngReadFn)
mkSpngReadFn = mkSpngRwFn

type SpngWriteFn = Ptr SpngCtx    -- ^ ctx
                -> Ptr ()         -- ^ user
                -> Ptr ()         -- ^ src
                -> #{type size_t} -- ^ length
                -> IO SpngErrNo

mkSpngWriteFn :: SpngWriteFn -> IO (FunPtr SpngWriteFn)
mkSpngWriteFn = mkSpngRwFn

type SpngRwFn = Ptr SpngCtx    -- ^ ctx
             -> Ptr ()         -- ^ user
             -> Ptr ()         -- ^ dst_src
             -> #{type size_t} -- ^ length
             -> IO SpngErrNo

foreign import ccall "wrapper"
  mkSpngRwFn :: SpngRwFn -> IO (FunPtr SpngRwFn)



foreign import ccall unsafe "spng_ctx_new"
  spng_ctx_new'
    :: #{type int}      -- ^ flags
    -> IO (Ptr SpngCtx)

foreign import ccall unsafe "spng_ctx_new2"
  spng_ctx_new2
    :: Ptr SpngAlloc -- ^ alloc
    -> #{type int}   -- ^ flags
    -> IO (Ptr SpngCtx)

foreign import ccall unsafe "spng_ctx_free"
  spng_ctx_free
    :: Ptr SpngCtx -- ^ ctx
    -> IO ()

foreign import ccall "spng_set_png_buffer"
  spng_set_png_buffer'
    :: Ptr SpngCtx    -- ^ ctx
    -> Ptr ()         -- ^ const buf
    -> #{type size_t} -- ^ size
    -> IO #type int

foreign import ccall "spng_set_png_stream"
  spng_set_png_stream'
    :: Ptr SpngCtx     -- ^ ctx
    -> FunPtr SpngRwFn -- ^ rw_func
    -> Ptr ()          -- ^ user
    -> IO #type int

foreign import ccall "spng_set_png_file"
  spng_set_png_file'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr CFile    -- ^ file
    -> IO #type int

foreign import ccall "spng_get_png_buffer"
  spng_get_png_buffer'
    :: Ptr SpngCtx        -- ^ ctx
    -> Ptr #{type size_t} -- ^ len
    -> Ptr SpngErrNo      -- ^ error
    -> IO (Ptr ())

foreign import ccall "spng_set_image_limits"
  spng_set_image_limits'
    :: Ptr SpngCtx      -- ^ ctx
    -> #{type uint32_t} -- ^ width
    -> #{type uint32_t} -- ^ height
    -> IO #type int

foreign import ccall "spng_get_image_limits"
  spng_get_image_limits'
    :: Ptr SpngCtx          -- ^ ctx
    -> Ptr #{type uint32_t} -- ^ width
    -> Ptr #{type uint32_t} -- ^ height
    -> IO #type int

foreign import ccall "spng_set_chunk_limits"
  spng_set_chunk_limits'
    :: Ptr SpngCtx    -- ^ ctx
    -> #{type size_t} -- ^ chunk_size
    -> #{type size_t} -- ^ cache_size
    -> IO #type int

foreign import ccall "spng_get_chunk_limits"
  spng_get_chunk_limits'
    :: Ptr SpngCtx        -- ^ ctx
    -> Ptr #{type size_t} -- ^ chunk_size
    -> Ptr #{type size_t} -- ^ cache_size
    -> IO #type int

foreign import ccall "spng_set_crc_action"
  spng_set_crc_action'
    :: Ptr SpngCtx -- ^ ctx
    -> #{type int} -- ^ critical
    -> #{type int} -- ^ ancillary
    -> IO #type int

foreign import ccall "spng_set_option"
  spng_set_option'
    :: Ptr SpngCtx  -- ^ ctx
    -> SpngOption   -- ^ option
    -> #{type int}  -- ^ value
    -> IO #type int

foreign import ccall "spng_get_option"
  spng_get_option'
    :: Ptr SpngCtx     -- ^ ctx
    -> SpngOption      -- ^ option
    -> Ptr #{type int} -- ^ value
    -> IO #type int

foreign import ccall "spng_decoded_image_size"
  spng_decoded_image_size'
    :: Ptr SpngCtx        -- ^ ctx
    -> SpngFormat         -- ^ fmt
    -> Ptr #{type size_t} -- ^ len
    -> IO #type int

foreign import ccall "spng_decode_image"
  spng_decode_image'
    :: Ptr SpngCtx     -- ^ ctx
    -> Ptr ()          -- ^ out
    -> #{type size_t}  -- ^ len
    -> SpngFormat      -- ^ fmt
    -> SpngDecodeFlags -- ^ flags
    -> IO #type int

foreign import ccall "spng_decode_scanline"
  spng_decode_scanline'
    :: Ptr SpngCtx    -- ^ ctx
    -> Ptr ()         -- ^ out
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import ccall "spng_decode_row"
  spng_decode_row'
    :: Ptr SpngCtx    -- ^ ctx
    -> Ptr ()         -- ^ out
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import ccall "spng_decode_chunks"
  spng_decode_chunks'
    :: Ptr SpngCtx -- ^ ctx
    -> IO #type int

foreign import ccall "spng_get_row_info"
  spng_get_row_info'
    :: Ptr SpngCtx     -- ^ ctx
    -> Ptr SpngRowInfo -- ^ row_info
    -> IO #type int

foreign import ccall "spng_encode_image"
  spng_encode_image'
    :: Ptr SpngCtx     -- ^ ctx
    -> Ptr ()          -- ^ const img
    -> #{type size_t}  -- ^ len
    -> SpngFormat      -- ^ fmt
    -> SpngEncodeFlags -- ^ flags
    -> IO #type int

foreign import ccall "spng_encode_scanline"
  spng_encode_scanline'
    :: Ptr SpngCtx    -- ^ ctx
    -> Ptr ()         -- ^ const scanline
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import ccall "spng_encode_row"
  spng_encode_row'
    :: Ptr SpngCtx    -- ^ ctx
    -> Ptr ()         -- ^ const row
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import ccall "spng_encode_chunks"
  spng_encode_chunks'
    :: Ptr SpngCtx    -- ^ ctx
    -> IO #type int

foreign import ccall "spng_get_ihdr"
  spng_get_ihdr'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngIhdr -- ^ ihdr
    -> IO #type int

foreign import ccall "spng_get_plte"
  spng_get_plte'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngPlte -- ^ plte
    -> IO #type int

foreign import ccall "spng_get_trns"
  spng_get_trns'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngTrns -- ^ trns
    -> IO #type int

foreign import ccall "spng_get_chrm"
  spng_get_chrm'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngChrm -- ^ chrm
    -> IO #type int

foreign import ccall "spng_get_chrm_int"
  spng_get_chrm_int'
    :: Ptr SpngCtx     -- ^ ctx
    -> Ptr SpngChrmInt -- ^ chrm_int
    -> IO #type int

foreign import ccall "spng_get_gama"
  spng_get_gama'
    :: Ptr SpngCtx        -- ^ ctx
    -> Ptr #{type double} -- ^ gamma
    -> IO #type int

foreign import ccall "spng_get_gama_int"
  spng_get_gama_int'
    :: Ptr SpngCtx          -- ^ ctx
    -> Ptr #{type uint32_t} -- ^ gama_int
    -> IO #type int

foreign import ccall "spng_get_iccp"
  spng_get_iccp'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngIccp -- ^ iccp
    -> IO #type int

foreign import ccall "spng_get_sbit"
  spng_get_sbit'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngSbit -- ^ sbit
    -> IO #type int

foreign import ccall "spng_get_srgb"
  spng_get_srgb'
    :: Ptr SpngCtx         -- ^ ctx
    -> Ptr #{type uint8_t} -- ^ rendering_intent
    -> IO #type int

foreign import ccall "spng_get_text"
  spng_get_text'
    :: Ptr SpngCtx          -- ^ ctx
    -> Ptr SpngText         -- ^ text
    -> Ptr #{type uint32_t} -- ^ n_text
    -> IO #type int

foreign import ccall "spng_get_bkgd"
  spng_get_bkgd'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngBkgd -- ^ bkgd
    -> IO #type int

foreign import ccall "spng_get_hist"
  spng_get_hist'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngHist -- ^ hist
    -> IO #type int

foreign import ccall "spng_get_phys"
  spng_get_phys'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngPhys -- ^ phys
    -> IO #type int

foreign import ccall "spng_get_splt"
  spng_get_splt'
    :: Ptr SpngCtx          -- ^ ctx
    -> Ptr SpngSplt         -- ^ splt
    -> Ptr #{type uint32_t} -- ^ n_splt
    -> IO #type int

foreign import ccall "spng_get_time"
  spng_get_time'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngTime -- ^ time
    -> IO #type int

foreign import ccall "spng_get_unknown_chunks"
  spng_get_unknown_chunks'
    :: Ptr SpngCtx          -- ^ ctx
    -> Ptr SpngUnknownChunk -- ^ chunks
    -> Ptr #{type uint32_t} -- ^ n_chunks
    -> IO #type int

foreign import ccall "spng_get_offs"
  spng_get_offs'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngOffs -- ^ offs
    -> IO #type int

foreign import ccall "spng_get_exif"
  spng_get_exif'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngExif -- ^ exif
    -> IO #type int

foreign import ccall "spng_set_ihdr"
  spng_set_ihdr'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngIhdr -- ^ ihdr
    -> IO #type int

foreign import ccall "spng_set_plte"
  spng_set_plte'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngPlte -- ^ plte
    -> IO #type int

foreign import ccall "spng_set_trns"
  spng_set_trns'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngTrns -- ^ trns
    -> IO #type int

foreign import ccall "spng_set_chrm"
  spng_set_chrm'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngChrm -- ^ chrm
    -> IO #type int

foreign import ccall "spng_set_chrm_int"
  spng_set_chrm_int'
    :: Ptr SpngCtx     -- ^ ctx
    -> Ptr SpngChrmInt -- ^ chrm_int
    -> IO #type int

foreign import ccall "spng_set_gama"
  spng_set_gama'
    :: Ptr SpngCtx    -- ^ ctx
    -> #{type double} -- ^ gamma
    -> IO #type int

foreign import ccall "spng_set_gama_int"
  spng_set_gama_int'
    :: Ptr SpngCtx      -- ^ ctx
    -> #{type uint32_t} -- ^ gamma
    -> IO #type int

foreign import ccall "spng_set_iccp"
  spng_set_iccp'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngIccp -- ^ iccp
    -> IO #type int

foreign import ccall "spng_set_sbit"
  spng_set_sbit'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngSbit -- ^ sbit
    -> IO #type int

foreign import ccall "spng_set_srgb"
  spng_set_srgb'
    :: Ptr SpngCtx     -- ^ ctx
    -> #{type uint8_t} -- ^ rendering_intent
    -> IO #type int

foreign import ccall "spng_set_text"
  spng_set_text'
    :: Ptr SpngCtx      -- ^ ctx
    -> Ptr SpngText     -- ^ text
    -> #{type uint32_t} -- ^ n_text
    -> IO #type int

foreign import ccall "spng_set_bkgd"
  spng_set_bkgd'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngBkgd -- ^ bkgd
    -> IO #type int

foreign import ccall "spng_set_hist"
  spng_set_hist'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngHist -- ^ hist
    -> IO #type int

foreign import ccall "spng_set_phys"
  spng_set_phys'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngPhys -- ^ phys
    -> IO #type int

foreign import ccall "spng_set_splt"
  spng_set_splt'
    :: Ptr SpngCtx      -- ^ ctx
    -> Ptr SpngSplt     -- ^ splt
    -> #{type uint32_t} -- ^ n_splt
    -> IO #type int

foreign import ccall "spng_set_time"
  spng_set_time'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngTime -- ^ time
    -> IO #type int

foreign import ccall "spng_set_unknown_chunks"
  spng_set_unknown_chunks'
    :: Ptr SpngCtx          -- ^ ctx
    -> Ptr SpngUnknownChunk -- ^ chunks
    -> #{type uint32_t}     -- ^ n_chunks
    -> IO #type int

foreign import ccall "spng_set_offs"
  spng_set_offs'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngOffs -- ^ offs
    -> IO #type int

foreign import ccall "spng_set_exif"
  spng_set_exif'
    :: Ptr SpngCtx  -- ^ ctx
    -> Ptr SpngExif -- ^ exif
    -> IO #type int

foreign import ccall unsafe "spng_strerror"
  spng_strerror'
    :: #{type int}           -- ^ err
    -> IO (Ptr #{type char}) -- ^ const

foreign import ccall unsafe "spng_version_string"
  spng_version_string'
    :: IO (Ptr #{type char}) -- ^ const
