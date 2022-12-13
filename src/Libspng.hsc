{-# LANGUAGE CApiFFI
           , CPP
           , DataKinds
           , DuplicateRecordFields
           , EmptyDataDecls
           , FlexibleInstances
           , ForeignFunctionInterface
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE PatternSynonyms
           , TypeApplications #-}

module Libspng
  ( -- ** Version
    pattern SPNG_VERSION_MAJOR
  , pattern SPNG_VERSION_MINOR
  , pattern SPNG_VERSION_PATCH
    -- ** spng_errno
  , Spng_errno
  , pattern SPNG_IO_ERROR
  , pattern SPNG_IO_EOF
  , pattern SPNG_OK
  , pattern SPNG_EINVAL
  , pattern SPNG_EMEM
  , pattern SPNG_EOVERFLOW
  , pattern SPNG_ESIGNATURE
  , pattern SPNG_EWIDTH
  , pattern SPNG_EHEIGHT
  , pattern SPNG_EUSER_WIDTH
  , pattern SPNG_EUSER_HEIGHT
  , pattern SPNG_EBIT_DEPTH
  , pattern SPNG_ECOLOR_TYPE
  , pattern SPNG_ECOMPRESSION_METHOD
  , pattern SPNG_EFILTER_METHOD
  , pattern SPNG_EINTERLACE_METHOD
  , pattern SPNG_EIHDR_SIZE
  , pattern SPNG_ENOIHDR
  , pattern SPNG_ECHUNK_POS
  , pattern SPNG_ECHUNK_SIZE
  , pattern SPNG_ECHUNK_CRC
  , pattern SPNG_ECHUNK_TYPE
  , pattern SPNG_ECHUNK_UNKNOWN_CRITICAL
  , pattern SPNG_EDUP_PLTE
  , pattern SPNG_EDUP_CHRM
  , pattern SPNG_EDUP_GAMA
  , pattern SPNG_EDUP_ICCP
  , pattern SPNG_EDUP_SBIT
  , pattern SPNG_EDUP_SRGB
  , pattern SPNG_EDUP_BKGD
  , pattern SPNG_EDUP_HIST
  , pattern SPNG_EDUP_TRNS
  , pattern SPNG_EDUP_PHYS
  , pattern SPNG_EDUP_TIME
  , pattern SPNG_EDUP_OFFS
  , pattern SPNG_EDUP_EXIF
  , pattern SPNG_ECHRM
  , pattern SPNG_EPLTE_IDX
  , pattern SPNG_ETRNS_COLOR_TYPE
  , pattern SPNG_ETRNS_NO_PLTE
  , pattern SPNG_EGAMA
  , pattern SPNG_EICCP_NAME
  , pattern SPNG_EICCP_COMPRESSION_METHOD
  , pattern SPNG_ESBIT
  , pattern SPNG_ESRGB
  , pattern SPNG_ETEXT
  , pattern SPNG_ETEXT_KEYWORD
  , pattern SPNG_EZTXT
  , pattern SPNG_EZTXT_COMPRESSION_METHOD
  , pattern SPNG_EITXT
  , pattern SPNG_EITXT_COMPRESSION_FLAG
  , pattern SPNG_EITXT_COMPRESSION_METHOD
  , pattern SPNG_EITXT_LANG_TAG
  , pattern SPNG_EITXT_TRANSLATED_KEY
  , pattern SPNG_EBKGD_NO_PLTE
  , pattern SPNG_EBKGD_PLTE_IDX
  , pattern SPNG_EHIST_NO_PLTE
  , pattern SPNG_EPHYS
  , pattern SPNG_ESPLT_NAME
  , pattern SPNG_ESPLT_DUP_NAME
  , pattern SPNG_ESPLT_DEPTH
  , pattern SPNG_ETIME
  , pattern SPNG_EOFFS
  , pattern SPNG_EEXIF
  , pattern SPNG_EIDAT_TOO_SHORT
  , pattern SPNG_EIDAT_STREAM
  , pattern SPNG_EZLIB
  , pattern SPNG_EFILTER
  , pattern SPNG_EBUFSIZ
  , pattern SPNG_EIO
  , pattern SPNG_EOF
  , pattern SPNG_EBUF_SET
  , pattern SPNG_EBADSTATE
  , pattern SPNG_EFMT
  , pattern SPNG_EFLAGS
  , pattern SPNG_ECHUNKAVAIL
  , pattern SPNG_ENCODE_ONLY
  , pattern SPNG_EOI
  , pattern SPNG_ENOPLTE
  , pattern SPNG_ECHUNK_LIMITS
  , pattern SPNG_EZLIB_INIT
  , pattern SPNG_ECHUNK_STDLEN
  , pattern SPNG_EINTERNAL
  , pattern SPNG_ECTXTYPE
  , pattern SPNG_ENOSRC
  , pattern SPNG_ENODST
  , pattern SPNG_EOPSTATE
  , pattern SPNG_ENOTFINAL
    -- ** spng_text_type
  , Spng_text_type
  , pattern SPNG_TEXT
  , pattern SPNG_ZTXT
  , pattern SPNG_ITXT
    -- ** spng_color_type
  , Spng_color_type
  , pattern SPNG_COLOR_TYPE_GRAYSCALE
  , pattern SPNG_COLOR_TYPE_TRUECOLOR
  , pattern SPNG_COLOR_TYPE_INDEXED
  , pattern SPNG_COLOR_TYPE_GRAYSCALE_ALPHA
  , pattern SPNG_COLOR_TYPE_TRUECOLOR_ALPHA
    -- ** spng_filter
  , Spng_filter
  , pattern SPNG_FILTER_NONE
  , pattern SPNG_FILTER_SUB
  , pattern SPNG_FILTER_UP
  , pattern SPNG_FILTER_AVERAGE
  , pattern SPNG_FILTER_PAETH
    -- ** spng_filter_choice
  , Spng_filter_choice
  , pattern SPNG_DISABLE_FILTERING
  , pattern SPNG_FILTER_CHOICE_NONE
  , pattern SPNG_FILTER_CHOICE_SUB
  , pattern SPNG_FILTER_CHOICE_UP
  , pattern SPNG_FILTER_CHOICE_AVG
  , pattern SPNG_FILTER_CHOICE_PAETH
  , pattern SPNG_FILTER_CHOICE_ALL
    -- ** spng_interlace_method
  , Spng_interlace_method
  , pattern SPNG_INTERLACE_NONE
  , pattern SPNG_INTERLACE_ADAM7
    -- ** spng_format
  , Spng_format
  , pattern SPNG_FMT_RGBA8
  , pattern SPNG_FMT_RGBA16
  , pattern SPNG_FMT_RGB8
  , pattern SPNG_FMT_GA8
  , pattern SPNG_FMT_GA16
  , pattern SPNG_FMT_G8
  , pattern SPNG_FMT_PNG
  , pattern SPNG_FMT_RAW
    -- ** spng_ctx_flags
  , Spng_ctx_flags
  , pattern SPNG_CTX_IGNORE_ADLER32
  , pattern SPNG_CTX_ENCODER
    -- ** spng_decode_flags
  , Spng_decode_flags
  , pattern SPNG_DECODE_TRNS
  , pattern SPNG_DECODE_GAMMA
  , pattern SPNG_DECODE_USE_SBIT
  , pattern SPNG_DECODE_PROGRESSIVE
    -- ** spng_crc_action
  , Spng_crc_action
  , pattern SPNG_CRC_ERROR
  , pattern SPNG_CRC_DISCARD
  , pattern SPNG_CRC_USE
    -- ** spng_encode_flags
  , Spng_encode_flags
  , pattern SPNG_ENCODE_PROGRESSIVE
  , pattern SPNG_ENCODE_FINALIZE
    -- ** spng_ihdr
  , Spng_ihdr (..)
    -- ** spng_plte_entry
  , Spng_plte_entry (..)
    -- ** spng_plte
  , Spng_plte (..)
    -- ** spng_trns
  , Spng_trns (..)
    -- ** spng_chrm_int
  , Spng_chrm_int (..)
    -- ** spng_chrm
  , Spng_chrm (..)
    -- ** spng_iccp
  , Spng_iccp (..)
    -- ** spng_sbit
  , Spng_sbit (..)
    -- ** spng_text
  , Spng_text (..)
    -- ** spng_bkgd
  , Spng_bkgd (..)
    -- ** spng_hist
  , Spng_hist (..)
    -- ** spng_phys
  , Spng_phys (..)
    -- ** spng_splt_entry
  , Spng_splt_entry (..)
    -- ** spng_splt
  , Spng_splt (..)
    -- ** spng_time
  , Spng_time (..)
    -- ** spng_offs
  , Spng_offs (..)
    -- ** spng_exif
  , Spng_exif (..)
    -- ** spng_chunk
  , Spng_chunk (..)
    -- ** spng_location
  , Spng_location
  , pattern SPNG_AFTER_IHDR
  , pattern SPNG_AFTER_PLTE
  , pattern SPNG_AFTER_IDAT
    -- ** spng_unknown_chunk
  , Spng_unknown_chunk (..)
    -- ** spng_option
  , Spng_option
  , pattern SPNG_KEEP_UNKNOWN_CHUNKS
  , pattern SPNG_IMG_COMPRESSION_LEVEL
  , pattern SPNG_IMG_WINDOW_BITS
  , pattern SPNG_IMG_MEM_LEVEL
  , pattern SPNG_IMG_COMPRESSION_STRATEGY
  , pattern SPNG_TEXT_COMPRESSION_LEVEL
  , pattern SPNG_TEXT_WINDOW_BITS
  , pattern SPNG_TEXT_MEM_LEVEL
  , pattern SPNG_TEXT_COMPRESSION_STRATEGY
  , pattern SPNG_FILTER_CHOICE
  , pattern SPNG_CHUNK_COUNT_LIMIT
  , pattern SPNG_ENCODE_TO_BUFFER
    -- ** spng_malloc_fn
  , Spng_malloc_fn
    -- ** spng_realloc_fn
  , Spng_realloc_fn
    -- ** spng_calloc_fn
  , Spng_calloc_fn
    -- ** spng_free_fn
  , Spng_free_fn
    -- ** spng_alloc
  , Spng_alloc (..)
    -- ** Spng_row_info
  , Spng_row_info (..)
    -- ** spng_ctx
  , Spng_ctx
    -- ** spng_read_fn
  , Spng_read_fn
    -- ** spng_write_fn
  , Spng_write_fn
    -- ** spng_rw_fn
  , Spng_rw_fn
    -- ** spng_ctx_new
  , spng_ctx_new
    -- ** spng_ctx_new2
  , spng_ctx_new2
    -- ** spng_ctx_free
  , spng_ctx_free
    -- ** spng_set_png_buffer
  , spng_set_png_buffer
    -- ** spng_set_png_stream
  , spng_set_png_stream
    -- ** spng_set_png_file
  , spng_set_png_file
    -- ** spng_get_png_buffer
  , spng_get_png_buffer
    -- ** spng_set_image_limits
  , spng_set_image_limits
    -- ** spng_get_image_limits
  , spng_get_image_limits
    -- ** spng_set_chunk_limits
  , spng_set_chunk_limits
    -- ** spng_get_chunk_limits
  , spng_get_chunk_limits
    -- ** spng_set_crc_action
  , spng_set_crc_action
    -- ** spng_set_option
  , spng_set_option
    -- ** spng_get_option
  , spng_get_option
    -- ** spng_decoded_image_size
  , spng_decoded_image_size
    -- ** spng_decode_image
  , spng_decode_image
    -- ** spng_decode_scanline
  , spng_decode_scanline
    -- ** spng_decode_row
  , spng_decode_row
    -- ** spng_decode_chunks
  , spng_decode_chunks
    -- ** spng_get_row_info
  , spng_get_row_info
    -- ** spng_encode_image
  , spng_encode_image
    -- ** spng_encode_scanline
  , spng_encode_scanline
    -- ** spng_encode_row
  , spng_encode_row
    -- ** spng_encode_chunks
  , spng_encode_chunks
    -- ** spng_get_ihdr
  , spng_get_ihdr
    -- ** spng_get_plte
  , spng_get_plte
    -- ** spng_get_trns
  , spng_get_trns
    -- ** spng_get_chrm
  , spng_get_chrm
    -- ** spng_get_chrm_int
  , spng_get_chrm_int
    -- ** spng_get_gama
  , spng_get_gama
    -- ** spng_get_gama_int
  , spng_get_gama_int
    -- ** spng_get_iccp
  , spng_get_iccp
    -- ** spng_get_sbit
  , spng_get_sbit
    -- ** spng_get_srgb
  , spng_get_srgb
    -- ** spng_get_text
  , spng_get_text
    -- ** spng_get_bkgd
  , spng_get_bkgd
    -- ** spng_get_hist
  , spng_get_hist
    -- ** spng_get_phys
  , spng_get_phys
    -- ** spng_get_splt
  , spng_get_splt
    -- ** spng_get_time
  , spng_get_time
    -- ** spng_get_unknown_chunks
  , spng_get_unknown_chunks
    -- ** spng_get_offs
  , spng_get_offs
    -- ** spng_get_exif
  , spng_get_exif
    -- ** spng_set_ihdr
  , spng_set_ihdr
    -- ** spng_set_plte
  , spng_set_plte
    -- ** spng_set_trns
  , spng_set_trns
    -- ** spng_set_chrm
  , spng_set_chrm
    -- ** spng_set_chrm_int
  , spng_set_chrm_int
    -- ** spng_set_gama
  , spng_set_gama
    -- ** spng_set_gama_int
  , spng_set_gama_int
    -- ** spng_set_iccp
  , spng_set_iccp
    -- ** spng_set_sbit
  , spng_set_sbit
    -- ** spng_set_srgb
  , spng_set_srgb
    -- ** spng_set_text
  , spng_set_text
    -- ** spng_set_bkgd
  , spng_set_bkgd
    -- ** spng_set_hist
  , spng_set_hist
    -- ** spng_set_phys
  , spng_set_phys
    -- ** spng_set_splt
  , spng_set_splt
    -- ** spng_set_time
  , spng_set_time
    -- ** spng_set_unknown_chunks
  , spng_set_unknown_chunks
    -- ** spng_set_offs
  , spng_set_offs
    -- ** spng_set_exif
  , spng_set_exif
    -- ** spng_strerror
  , spng_strerror
    -- ** spng_version_string
  , spng_version_string
  ) where

import           Data.Int
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset
import           GHC.Records

#include "spng.h"

pattern SPNG_VERSION_MAJOR
      , SPNG_VERSION_MINOR
      , SPNG_VERSION_PATCH
     :: (Eq a, Num a) => a
pattern SPNG_VERSION_MAJOR = #const SPNG_VERSION_MAJOR
pattern SPNG_VERSION_MINOR = #const SPNG_VERSION_MINOR
pattern SPNG_VERSION_PATCH = #const SPNG_VERSION_PATCH



type Spng_errno = #type enum spng_errno

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
     :: (Eq a, Num a) => a
pattern SPNG_IO_ERROR                 = #const SPNG_IO_ERROR
pattern SPNG_IO_EOF                   = #const SPNG_IO_EOF
pattern SPNG_OK                       = #const SPNG_OK
pattern SPNG_EINVAL                   = #const SPNG_EINVAL
pattern SPNG_EMEM                     = #const SPNG_EMEM
pattern SPNG_EOVERFLOW                = #const SPNG_EOVERFLOW
pattern SPNG_ESIGNATURE               = #const SPNG_ESIGNATURE
pattern SPNG_EWIDTH                   = #const SPNG_EWIDTH
pattern SPNG_EHEIGHT                  = #const SPNG_EHEIGHT
pattern SPNG_EUSER_WIDTH              = #const SPNG_EUSER_WIDTH
pattern SPNG_EUSER_HEIGHT             = #const SPNG_EUSER_HEIGHT
pattern SPNG_EBIT_DEPTH               = #const SPNG_EBIT_DEPTH
pattern SPNG_ECOLOR_TYPE              = #const SPNG_ECOLOR_TYPE
pattern SPNG_ECOMPRESSION_METHOD      = #const SPNG_ECOMPRESSION_METHOD
pattern SPNG_EFILTER_METHOD           = #const SPNG_EFILTER_METHOD
pattern SPNG_EINTERLACE_METHOD        = #const SPNG_EINTERLACE_METHOD
pattern SPNG_EIHDR_SIZE               = #const SPNG_EIHDR_SIZE
pattern SPNG_ENOIHDR                  = #const SPNG_ENOIHDR
pattern SPNG_ECHUNK_POS               = #const SPNG_ECHUNK_POS
pattern SPNG_ECHUNK_SIZE              = #const SPNG_ECHUNK_SIZE
pattern SPNG_ECHUNK_CRC               = #const SPNG_ECHUNK_CRC
pattern SPNG_ECHUNK_TYPE              = #const SPNG_ECHUNK_TYPE
pattern SPNG_ECHUNK_UNKNOWN_CRITICAL  = #const SPNG_ECHUNK_UNKNOWN_CRITICAL
pattern SPNG_EDUP_PLTE                = #const SPNG_EDUP_PLTE
pattern SPNG_EDUP_CHRM                = #const SPNG_EDUP_CHRM
pattern SPNG_EDUP_GAMA                = #const SPNG_EDUP_GAMA
pattern SPNG_EDUP_ICCP                = #const SPNG_EDUP_ICCP
pattern SPNG_EDUP_SBIT                = #const SPNG_EDUP_SBIT
pattern SPNG_EDUP_SRGB                = #const SPNG_EDUP_SRGB
pattern SPNG_EDUP_BKGD                = #const SPNG_EDUP_BKGD
pattern SPNG_EDUP_HIST                = #const SPNG_EDUP_HIST
pattern SPNG_EDUP_TRNS                = #const SPNG_EDUP_TRNS
pattern SPNG_EDUP_PHYS                = #const SPNG_EDUP_PHYS
pattern SPNG_EDUP_TIME                = #const SPNG_EDUP_TIME
pattern SPNG_EDUP_OFFS                = #const SPNG_EDUP_OFFS
pattern SPNG_EDUP_EXIF                = #const SPNG_EDUP_EXIF
pattern SPNG_ECHRM                    = #const SPNG_ECHRM
pattern SPNG_EPLTE_IDX                = #const SPNG_EPLTE_IDX
pattern SPNG_ETRNS_COLOR_TYPE         = #const SPNG_ETRNS_COLOR_TYPE
pattern SPNG_ETRNS_NO_PLTE            = #const SPNG_ETRNS_NO_PLTE
pattern SPNG_EGAMA                    = #const SPNG_EGAMA
pattern SPNG_EICCP_NAME               = #const SPNG_EICCP_NAME
pattern SPNG_EICCP_COMPRESSION_METHOD = #const SPNG_EICCP_COMPRESSION_METHOD
pattern SPNG_ESBIT                    = #const SPNG_ESBIT
pattern SPNG_ESRGB                    = #const SPNG_ESRGB
pattern SPNG_ETEXT                    = #const SPNG_ETEXT
pattern SPNG_ETEXT_KEYWORD            = #const SPNG_ETEXT_KEYWORD
pattern SPNG_EZTXT                    = #const SPNG_EZTXT
pattern SPNG_EZTXT_COMPRESSION_METHOD = #const SPNG_EZTXT_COMPRESSION_METHOD
pattern SPNG_EITXT                    = #const SPNG_EITXT
pattern SPNG_EITXT_COMPRESSION_FLAG   = #const SPNG_EITXT_COMPRESSION_FLAG
pattern SPNG_EITXT_COMPRESSION_METHOD = #const SPNG_EITXT_COMPRESSION_METHOD
pattern SPNG_EITXT_LANG_TAG           = #const SPNG_EITXT_LANG_TAG
pattern SPNG_EITXT_TRANSLATED_KEY     = #const SPNG_EITXT_TRANSLATED_KEY
pattern SPNG_EBKGD_NO_PLTE            = #const SPNG_EBKGD_NO_PLTE
pattern SPNG_EBKGD_PLTE_IDX           = #const SPNG_EBKGD_PLTE_IDX
pattern SPNG_EHIST_NO_PLTE            = #const SPNG_EHIST_NO_PLTE
pattern SPNG_EPHYS                    = #const SPNG_EPHYS
pattern SPNG_ESPLT_NAME               = #const SPNG_ESPLT_NAME
pattern SPNG_ESPLT_DUP_NAME           = #const SPNG_ESPLT_DUP_NAME
pattern SPNG_ESPLT_DEPTH              = #const SPNG_ESPLT_DEPTH
pattern SPNG_ETIME                    = #const SPNG_ETIME
pattern SPNG_EOFFS                    = #const SPNG_EOFFS
pattern SPNG_EEXIF                    = #const SPNG_EEXIF
pattern SPNG_EIDAT_TOO_SHORT          = #const SPNG_EIDAT_TOO_SHORT
pattern SPNG_EIDAT_STREAM             = #const SPNG_EIDAT_STREAM
pattern SPNG_EZLIB                    = #const SPNG_EZLIB
pattern SPNG_EFILTER                  = #const SPNG_EFILTER
pattern SPNG_EBUFSIZ                  = #const SPNG_EBUFSIZ
pattern SPNG_EIO                      = #const SPNG_EIO
pattern SPNG_EOF                      = #const SPNG_EOF
pattern SPNG_EBUF_SET                 = #const SPNG_EBUF_SET
pattern SPNG_EBADSTATE                = #const SPNG_EBADSTATE
pattern SPNG_EFMT                     = #const SPNG_EFMT
pattern SPNG_EFLAGS                   = #const SPNG_EFLAGS
pattern SPNG_ECHUNKAVAIL              = #const SPNG_ECHUNKAVAIL
pattern SPNG_ENCODE_ONLY              = #const SPNG_ENCODE_ONLY
pattern SPNG_EOI                      = #const SPNG_EOI
pattern SPNG_ENOPLTE                  = #const SPNG_ENOPLTE
pattern SPNG_ECHUNK_LIMITS            = #const SPNG_ECHUNK_LIMITS
pattern SPNG_EZLIB_INIT               = #const SPNG_EZLIB_INIT
pattern SPNG_ECHUNK_STDLEN            = #const SPNG_ECHUNK_STDLEN
pattern SPNG_EINTERNAL                = #const SPNG_EINTERNAL
pattern SPNG_ECTXTYPE                 = #const SPNG_ECTXTYPE
pattern SPNG_ENOSRC                   = #const SPNG_ENOSRC
pattern SPNG_ENODST                   = #const SPNG_ENODST
pattern SPNG_EOPSTATE                 = #const SPNG_EOPSTATE
pattern SPNG_ENOTFINAL                = #const SPNG_ENOTFINAL



type Spng_text_type = #type enum spng_text_type

pattern SPNG_TEXT
      , SPNG_ZTXT
      , SPNG_ITXT
     :: (Eq a, Num a) => a
pattern SPNG_TEXT = #const SPNG_TEXT
pattern SPNG_ZTXT = #const SPNG_ZTXT
pattern SPNG_ITXT = #const SPNG_ITXT



type Spng_color_type = #type enum spng_color_type

pattern SPNG_COLOR_TYPE_GRAYSCALE
      , SPNG_COLOR_TYPE_TRUECOLOR
      , SPNG_COLOR_TYPE_INDEXED
      , SPNG_COLOR_TYPE_GRAYSCALE_ALPHA
      , SPNG_COLOR_TYPE_TRUECOLOR_ALPHA
     :: (Eq a, Num a) => a
pattern SPNG_COLOR_TYPE_GRAYSCALE       = #const SPNG_COLOR_TYPE_GRAYSCALE
pattern SPNG_COLOR_TYPE_TRUECOLOR       = #const SPNG_COLOR_TYPE_TRUECOLOR
pattern SPNG_COLOR_TYPE_INDEXED         = #const SPNG_COLOR_TYPE_INDEXED
pattern SPNG_COLOR_TYPE_GRAYSCALE_ALPHA = #const SPNG_COLOR_TYPE_GRAYSCALE_ALPHA
pattern SPNG_COLOR_TYPE_TRUECOLOR_ALPHA = #const SPNG_COLOR_TYPE_TRUECOLOR_ALPHA



type Spng_filter = #type enum spng_filter

pattern SPNG_FILTER_NONE
      , SPNG_FILTER_SUB
      , SPNG_FILTER_UP
      , SPNG_FILTER_AVERAGE
      , SPNG_FILTER_PAETH
     :: (Eq a, Num a) => a
pattern SPNG_FILTER_NONE    = #const SPNG_FILTER_NONE
pattern SPNG_FILTER_SUB     = #const SPNG_FILTER_SUB
pattern SPNG_FILTER_UP      = #const SPNG_FILTER_UP
pattern SPNG_FILTER_AVERAGE = #const SPNG_FILTER_AVERAGE
pattern SPNG_FILTER_PAETH   = #const SPNG_FILTER_PAETH



type Spng_filter_choice = #type enum spng_filter_choice

pattern SPNG_DISABLE_FILTERING
      , SPNG_FILTER_CHOICE_NONE
      , SPNG_FILTER_CHOICE_SUB
      , SPNG_FILTER_CHOICE_UP
      , SPNG_FILTER_CHOICE_AVG
      , SPNG_FILTER_CHOICE_PAETH
      , SPNG_FILTER_CHOICE_ALL
     :: (Eq a, Num a) => a
pattern SPNG_DISABLE_FILTERING   = #const SPNG_DISABLE_FILTERING
pattern SPNG_FILTER_CHOICE_NONE  = #const SPNG_FILTER_CHOICE_NONE
pattern SPNG_FILTER_CHOICE_SUB   = #const SPNG_FILTER_CHOICE_SUB
pattern SPNG_FILTER_CHOICE_UP    = #const SPNG_FILTER_CHOICE_UP
pattern SPNG_FILTER_CHOICE_AVG   = #const SPNG_FILTER_CHOICE_AVG
pattern SPNG_FILTER_CHOICE_PAETH = #const SPNG_FILTER_CHOICE_PAETH
pattern SPNG_FILTER_CHOICE_ALL   = #const SPNG_FILTER_CHOICE_ALL



type Spng_interlace_method = #type enum spng_interlace_method

pattern SPNG_INTERLACE_NONE
      , SPNG_INTERLACE_ADAM7
     :: (Eq a, Num a) => a
pattern SPNG_INTERLACE_NONE  = #const SPNG_INTERLACE_NONE
pattern SPNG_INTERLACE_ADAM7 = #const SPNG_INTERLACE_ADAM7



type Spng_format = #type enum spng_format

pattern SPNG_FMT_RGBA8
      , SPNG_FMT_RGBA16
      , SPNG_FMT_RGB8
      , SPNG_FMT_GA8
      , SPNG_FMT_GA16
      , SPNG_FMT_G8
      , SPNG_FMT_PNG
      , SPNG_FMT_RAW
     :: (Eq a, Num a) => a
pattern SPNG_FMT_RGBA8  = #const SPNG_FMT_RGBA8
pattern SPNG_FMT_RGBA16 = #const SPNG_FMT_RGBA16
pattern SPNG_FMT_RGB8   = #const SPNG_FMT_RGB8
pattern SPNG_FMT_GA8    = #const SPNG_FMT_GA8
pattern SPNG_FMT_GA16   = #const SPNG_FMT_GA16
pattern SPNG_FMT_G8     = #const SPNG_FMT_G8
pattern SPNG_FMT_PNG    = #const SPNG_FMT_PNG
pattern SPNG_FMT_RAW    = #const SPNG_FMT_RAW



type Spng_ctx_flags = #type enum spng_ctx_flags

pattern SPNG_CTX_IGNORE_ADLER32
      , SPNG_CTX_ENCODER
     :: (Eq a, Num a) => a
pattern SPNG_CTX_IGNORE_ADLER32 = #const SPNG_CTX_IGNORE_ADLER32
pattern SPNG_CTX_ENCODER        = #const SPNG_CTX_ENCODER



type Spng_decode_flags = #type enum spng_decode_flags

pattern SPNG_DECODE_TRNS
      , SPNG_DECODE_GAMMA
      , SPNG_DECODE_USE_SBIT
      , SPNG_DECODE_PROGRESSIVE
     :: (Eq a, Num a) => a
pattern SPNG_DECODE_TRNS        = #const SPNG_DECODE_TRNS
pattern SPNG_DECODE_GAMMA       = #const SPNG_DECODE_GAMMA
pattern SPNG_DECODE_USE_SBIT    = #const SPNG_DECODE_USE_SBIT
pattern SPNG_DECODE_PROGRESSIVE = #const SPNG_DECODE_PROGRESSIVE



type Spng_crc_action = #type enum spng_crc_action

pattern SPNG_CRC_ERROR
      , SPNG_CRC_DISCARD
      , SPNG_CRC_USE
     :: (Eq a, Num a) => a
pattern SPNG_CRC_ERROR   = #const SPNG_CRC_ERROR
pattern SPNG_CRC_DISCARD = #const SPNG_CRC_DISCARD
pattern SPNG_CRC_USE     = #const SPNG_CRC_USE



type Spng_encode_flags = #type enum spng_encode_flags

pattern SPNG_ENCODE_PROGRESSIVE
      , SPNG_ENCODE_FINALIZE
     :: (Eq a, Num a) => a
pattern SPNG_ENCODE_PROGRESSIVE = #const SPNG_ENCODE_PROGRESSIVE
pattern SPNG_ENCODE_FINALIZE    = #const SPNG_ENCODE_FINALIZE



data Spng_ihdr =
       Spng_ihdr
         { width              :: #type uint32_t
         , height             :: #type uint32_t
         , bit_depth          :: #type uint8_t
         , color_type         :: #type uint8_t
         , compression_method :: #type uint8_t
         , filter_method      :: #type uint8_t
         , interlace_method   :: #type uint8_t
         }

instance Offset "width"              Spng_ihdr where rawOffset = #offset struct spng_ihdr, width
instance Offset "height"             Spng_ihdr where rawOffset = #offset struct spng_ihdr, height
instance Offset "bit_depth"          Spng_ihdr where rawOffset = #offset struct spng_ihdr, bit_depth
instance Offset "color_type"         Spng_ihdr where rawOffset = #offset struct spng_ihdr, color_type
instance Offset "compression_method" Spng_ihdr where rawOffset = #offset struct spng_ihdr, compression_method
instance Offset "filter_method"      Spng_ihdr where rawOffset = #offset struct spng_ihdr, filter_method
instance Offset "interlace_method"   Spng_ihdr where rawOffset = #offset struct spng_ihdr, interlace_method

instance Storable Spng_ihdr where
  sizeOf    _ = #size      struct spng_ihdr
  alignment _ = #alignment struct spng_ihdr

  peek ptr =
    Spng_ihdr
      <$> peek (Foreign.Storable.Offset.offset @"width"              ptr)
      <*> peek (Foreign.Storable.Offset.offset @"height"             ptr)
      <*> peek (Foreign.Storable.Offset.offset @"bit_depth"          ptr)
      <*> peek (Foreign.Storable.Offset.offset @"color_type"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"compression_method" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"filter_method"      ptr)
      <*> peek (Foreign.Storable.Offset.offset @"interlace_method"   ptr)

  poke ptr val = do
    pokeField @"width"              ptr val
    pokeField @"height"             ptr val
    pokeField @"bit_depth"          ptr val
    pokeField @"color_type"         ptr val
    pokeField @"compression_method" ptr val
    pokeField @"filter_method"      ptr val
    pokeField @"interlace_method"   ptr val



data Spng_plte_entry =
       Spng_plte_entry
         { red   :: #type uint8_t
         , green :: #type uint8_t
         , blue  :: #type uint8_t
         , alpha :: #{type uint8_t} -- ^ Reserved for internal use
         }

instance Offset "red"   Spng_plte_entry where rawOffset = #offset struct spng_plte_entry, red
instance Offset "green" Spng_plte_entry where rawOffset = #offset struct spng_plte_entry, green
instance Offset "blue"  Spng_plte_entry where rawOffset = #offset struct spng_plte_entry, blue
instance Offset "alpha" Spng_plte_entry where rawOffset = #offset struct spng_plte_entry, alpha

instance Storable Spng_plte_entry where
  sizeOf    _ = #size      struct spng_plte_entry
  alignment _ = #alignment struct spng_plte_entry

  peek ptr =
    Spng_plte_entry
      <$> peek (Foreign.Storable.Offset.offset @"red"   ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue"  ptr)
      <*> peek (Foreign.Storable.Offset.offset @"alpha" ptr)

  poke ptr val = do
    pokeField @"red"   ptr val
    pokeField @"green" ptr val
    pokeField @"blue"  ptr val
    pokeField @"alpha" ptr val



data Spng_plte =
       Spng_plte
         { n_entries :: #type uint32_t
         , entries   :: Ptr Spng_plte_entry -- ^ Array of 256 elements
         }

instance Offset "n_entries" Spng_plte where rawOffset = #offset struct spng_plte, n_entries
instance Offset "entries"   Spng_plte where rawOffset = #offset struct spng_plte, entries

instance Storable Spng_plte where
  sizeOf    _ = #size      struct spng_plte
  alignment _ = #alignment struct spng_plte

  peek ptr =
    Spng_plte
      <$> peek (Foreign.Storable.Offset.offset @"n_entries" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"entries"   ptr)

  poke ptr val = do
    pokeField @"n_entries" ptr val
    pokeField @"entries"   ptr val



data Spng_trns =
       Spng_trns
         { gray            :: #type uint16_t
         , red             :: #type uint16_t
         , green           :: #type uint16_t
         , blue            :: #type uint16_t
         , n_type3_entries :: #type uint32_t
         , type3_alpha     :: Ptr #{type uint8_t} -- ^ Array of 256 elements
         }

instance Offset "gray"            Spng_trns where rawOffset = #offset struct spng_trns, gray
instance Offset "red"             Spng_trns where rawOffset = #offset struct spng_trns, red
instance Offset "green"           Spng_trns where rawOffset = #offset struct spng_trns, green
instance Offset "blue"            Spng_trns where rawOffset = #offset struct spng_trns, blue
instance Offset "n_type3_entries" Spng_trns where rawOffset = #offset struct spng_trns, n_type3_entries
instance Offset "type3_alpha"     Spng_trns where rawOffset = #offset struct spng_trns, type3_alpha

instance Storable Spng_trns where
  sizeOf    _ = #size      struct spng_trns
  alignment _ = #alignment struct spng_trns

  peek ptr =
    Spng_trns
      <$> peek (Foreign.Storable.Offset.offset @"gray"            ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red"             ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green"           ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue"            ptr)
      <*> peek (Foreign.Storable.Offset.offset @"n_type3_entries" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"type3_alpha"     ptr)

  poke ptr val = do
    pokeField @"gray"            ptr val
    pokeField @"red"             ptr val
    pokeField @"green"           ptr val
    pokeField @"blue"            ptr val
    pokeField @"n_type3_entries" ptr val
    pokeField @"type3_alpha"     ptr val



data Spng_chrm_int =
       Spng_chrm_int
         { white_point_x :: #type uint32_t
         , white_point_y :: #type uint32_t
         , red_x         :: #type uint32_t
         , red_y         :: #type uint32_t
         , green_x       :: #type uint32_t
         , green_y       :: #type uint32_t
         , blue_x        :: #type uint32_t
         , blue_y        :: #type uint32_t
         }

instance Offset "white_point_x" Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, white_point_x
instance Offset "white_point_y" Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, white_point_y
instance Offset "red_x"         Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, red_x
instance Offset "red_y"         Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, red_y
instance Offset "green_x"       Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, green_x
instance Offset "green_y"       Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, green_y
instance Offset "blue_x"        Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, blue_x
instance Offset "blue_y"        Spng_chrm_int where rawOffset = #offset struct spng_chrm_int, blue_y

instance Storable Spng_chrm_int where
  sizeOf    _ = #size      struct spng_chrm_int
  alignment _ = #alignment struct spng_chrm_int

  peek ptr =
    Spng_chrm_int
      <$> peek (Foreign.Storable.Offset.offset @"white_point_x" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"white_point_y" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red_x"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red_y"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green_x"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green_y"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue_x"        ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue_y"        ptr)

  poke ptr val = do
    pokeField @"white_point_x" ptr val
    pokeField @"white_point_y" ptr val
    pokeField @"red_x"         ptr val
    pokeField @"red_y"         ptr val
    pokeField @"green_x"       ptr val
    pokeField @"green_y"       ptr val
    pokeField @"blue_x"        ptr val
    pokeField @"blue_y"        ptr val



data Spng_chrm =
       Spng_chrm
         { white_point_x :: #type double
         , white_point_y :: #type double
         , red_x         :: #type double
         , red_y         :: #type double
         , green_x       :: #type double
         , green_y       :: #type double
         , blue_x        :: #type double
         , blue_y        :: #type double
         }

instance Offset "white_point_x" Spng_chrm where rawOffset = #offset struct spng_chrm, white_point_x
instance Offset "white_point_y" Spng_chrm where rawOffset = #offset struct spng_chrm, white_point_y
instance Offset "red_x"         Spng_chrm where rawOffset = #offset struct spng_chrm, red_x
instance Offset "red_y"         Spng_chrm where rawOffset = #offset struct spng_chrm, red_y
instance Offset "green_x"       Spng_chrm where rawOffset = #offset struct spng_chrm, green_x
instance Offset "green_y"       Spng_chrm where rawOffset = #offset struct spng_chrm, green_y
instance Offset "blue_x"        Spng_chrm where rawOffset = #offset struct spng_chrm, blue_x
instance Offset "blue_y"        Spng_chrm where rawOffset = #offset struct spng_chrm, blue_y

instance Storable Spng_chrm where
  sizeOf    _ = #size      struct spng_chrm
  alignment _ = #alignment struct spng_chrm

  peek ptr =
    Spng_chrm
      <$> peek (Foreign.Storable.Offset.offset @"white_point_x" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"white_point_y" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red_x"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red_y"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green_x"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green_y"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue_x"        ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue_y"        ptr)

  poke ptr val = do
    pokeField @"white_point_x" ptr val
    pokeField @"white_point_y" ptr val
    pokeField @"red_x"         ptr val
    pokeField @"red_y"         ptr val
    pokeField @"green_x"       ptr val
    pokeField @"green_y"       ptr val
    pokeField @"blue_x"        ptr val
    pokeField @"blue_y"        ptr val




data Spng_iccp =
       Spng_iccp
         { profile_name :: Ptr #{type char} -- ^ Array of 80 elements
         , profile_len  :: #type size_t
         , profile      :: Ptr #type char
         }

instance Offset "profile_name" Spng_iccp where rawOffset = #offset struct spng_iccp, profile_name
instance Offset "profile_len"  Spng_iccp where rawOffset = #offset struct spng_iccp, profile_len
instance Offset "profile"      Spng_iccp where rawOffset = #offset struct spng_iccp, profile

instance Storable Spng_iccp where
  sizeOf    _ = #size      struct spng_iccp
  alignment _ = #alignment struct spng_iccp

  peek ptr =
    Spng_iccp
      <$> peek (Foreign.Storable.Offset.offset @"profile_name" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"profile_len"  ptr)
      <*> peek (Foreign.Storable.Offset.offset @"profile"      ptr)

  poke ptr val = do
    pokeField @"profile_name" ptr val
    pokeField @"profile_len"  ptr val
    pokeField @"profile"      ptr val



data Spng_sbit =
       Spng_sbit
         { grayscale_bits :: #type uint8_t
         , red_bits       :: #type uint8_t
         , green_bits     :: #type uint8_t
         , blue_bits      :: #type uint8_t
         , alpha_bits     :: #type uint8_t
         }

instance Offset "grayscale_bits" Spng_sbit where rawOffset = #offset struct spng_sbit, grayscale_bits
instance Offset "red_bits"       Spng_sbit where rawOffset = #offset struct spng_sbit, red_bits
instance Offset "green_bits"     Spng_sbit where rawOffset = #offset struct spng_sbit, green_bits
instance Offset "blue_bits"      Spng_sbit where rawOffset = #offset struct spng_sbit, blue_bits
instance Offset "alpha_bits"     Spng_sbit where rawOffset = #offset struct spng_sbit, alpha_bits

instance Storable Spng_sbit where
  sizeOf    _ = #size      struct spng_sbit
  alignment _ = #alignment struct spng_sbit

  peek ptr =
    Spng_sbit
      <$> peek (Foreign.Storable.Offset.offset @"grayscale_bits" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red_bits"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green_bits"     ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue_bits"      ptr)
      <*> peek (Foreign.Storable.Offset.offset @"alpha_bits"     ptr)

  poke ptr val = do
    pokeField @"grayscale_bits" ptr val
    pokeField @"red_bits"       ptr val
    pokeField @"green_bits"     ptr val
    pokeField @"blue_bits"      ptr val
    pokeField @"alpha_bits"     ptr val



data Spng_text =
       Spng_text
         { keyword            :: Ptr #{type char} -- ^ Array of 80 elements
         , type_              :: #type int
         , length             :: #type size_t
         , text               :: Ptr #type char
         , compression_flag   :: #{type uint8_t}  -- ^ iTXt only
         , compression_method :: #{type uint8_t}  -- ^ iTXt, ztXt only
         , language_tag       :: Ptr #{type char} -- ^ iTXt only
         , translated_keyword :: Ptr #{type char} -- ^ iTXt only
         }

instance Offset "keyword"            Spng_text where rawOffset = #offset struct spng_text, keyword
instance Offset "type_"              Spng_text where rawOffset = #offset struct spng_text, type
instance Offset "length"             Spng_text where rawOffset = #offset struct spng_text, length
instance Offset "text"               Spng_text where rawOffset = #offset struct spng_text, text
instance Offset "compression_flag"   Spng_text where rawOffset = #offset struct spng_text, compression_flag
instance Offset "compression_method" Spng_text where rawOffset = #offset struct spng_text, compression_method
instance Offset "language_tag"       Spng_text where rawOffset = #offset struct spng_text, language_tag
instance Offset "translated_keyword" Spng_text where rawOffset = #offset struct spng_text, translated_keyword

instance Offset "type" Spng_text where
  rawOffset = rawOffset @"type_" @Spng_text

instance HasField "type" Spng_text #{type int} where
  getField = getField @"type_"

instance Storable Spng_text where
  sizeOf    _ = #size      struct spng_text
  alignment _ = #alignment struct spng_text

  peek ptr =
    Spng_text
      <$> peek (Foreign.Storable.Offset.offset @"keyword"            ptr)
      <*> peek (Foreign.Storable.Offset.offset @"type"               ptr)
      <*> peek (Foreign.Storable.Offset.offset @"length"             ptr)
      <*> peek (Foreign.Storable.Offset.offset @"text"               ptr)
      <*> peek (Foreign.Storable.Offset.offset @"compression_flag"   ptr)
      <*> peek (Foreign.Storable.Offset.offset @"compression_method" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"language_tag"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"translated_keyword" ptr)

  poke ptr val = do
    pokeField @"keyword"            ptr val
    pokeField @"type"               ptr val
    pokeField @"length"             ptr val
    pokeField @"text"               ptr val
    pokeField @"compression_flag"   ptr val
    pokeField @"compression_method" ptr val
    pokeField @"language_tag"       ptr val
    pokeField @"translated_keyword" ptr val



data Spng_bkgd =
       Spng_bkgd
         { gray       :: #{type uint16_t} -- ^ Only for gray/gray alpha
         , red        :: #type uint16_t
         , green      :: #type uint16_t
         , blue       :: #type uint16_t
         , plte_index :: #{type uint16_t} -- ^ Only for indexed color
         }

instance Offset "gray"       Spng_bkgd where rawOffset = #offset struct spng_bkgd, gray
instance Offset "red"        Spng_bkgd where rawOffset = #offset struct spng_bkgd, red
instance Offset "green"      Spng_bkgd where rawOffset = #offset struct spng_bkgd, green
instance Offset "blue"       Spng_bkgd where rawOffset = #offset struct spng_bkgd, blue
instance Offset "plte_index" Spng_bkgd where rawOffset = #offset struct spng_bkgd, plte_index

instance Storable Spng_bkgd where
  sizeOf    _ = #size      struct spng_bkgd
  alignment _ = #alignment struct spng_bkgd

  peek ptr =
    Spng_bkgd
      <$> peek (Foreign.Storable.Offset.offset @"gray"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"red"        ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green"      ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"plte_index" ptr)

  poke ptr val = do
    pokeField @"gray"       ptr val
    pokeField @"red"        ptr val
    pokeField @"green"      ptr val
    pokeField @"blue"       ptr val
    pokeField @"plte_index" ptr val



newtype Spng_hist =
       Spng_hist
         { frequency :: Ptr #{type uint16_t} -- ^ Array of 256 elements
         }

instance Offset "frequency" Spng_hist where rawOffset = #offset struct spng_hist, frequency

instance Storable Spng_hist where
  sizeOf    _ = #size      struct spng_hist
  alignment _ = #alignment struct spng_hist

  peek ptr =
    Spng_hist
      <$> peek (Foreign.Storable.Offset.offset @"frequency" ptr)

  poke ptr val =
    pokeField @"frequency" ptr val



data Spng_phys =
       Spng_phys
         { ppu_x          :: #type int32_t
         , ppu_y          :: #type int32_t
         , unit_specifier :: #type uint8_t
         }

instance Offset "ppu_x"          Spng_phys where rawOffset = #offset struct spng_phys, ppu_x
instance Offset "ppu_y"          Spng_phys where rawOffset = #offset struct spng_phys, ppu_y
instance Offset "unit_specifier" Spng_phys where rawOffset = #offset struct spng_phys, unit_specifier

instance Storable Spng_phys where
  sizeOf    _ = #size      struct spng_phys
  alignment _ = #alignment struct spng_phys

  peek ptr =
    Spng_phys
      <$> peek (Foreign.Storable.Offset.offset @"ppu_x"          ptr)
      <*> peek (Foreign.Storable.Offset.offset @"ppu_y"          ptr)
      <*> peek (Foreign.Storable.Offset.offset @"unit_specifier" ptr)

  poke ptr val = do
    pokeField @"ppu_x"          ptr val
    pokeField @"ppu_y"          ptr val
    pokeField @"unit_specifier" ptr val



data Spng_splt_entry =
       Spng_splt_entry
         { red       :: #type uint16_t
         , green     :: #type uint16_t
         , blue      :: #type uint16_t
         , alpha     :: #type uint16_t
         , frequency :: #type uint16_t
         }

instance Offset "red"       Spng_splt_entry where rawOffset = #offset struct spng_splt_entry, red
instance Offset "green"     Spng_splt_entry where rawOffset = #offset struct spng_splt_entry, green
instance Offset "blue"      Spng_splt_entry where rawOffset = #offset struct spng_splt_entry, blue
instance Offset "alpha"     Spng_splt_entry where rawOffset = #offset struct spng_splt_entry, alpha
instance Offset "frequency" Spng_splt_entry where rawOffset = #offset struct spng_splt_entry, frequency

instance Storable Spng_splt_entry where
  sizeOf    _ = #size      struct spng_splt_entry
  alignment _ = #alignment struct spng_splt_entry

  peek ptr =
    Spng_splt_entry
      <$> peek (Foreign.Storable.Offset.offset @"red"       ptr)
      <*> peek (Foreign.Storable.Offset.offset @"green"     ptr)
      <*> peek (Foreign.Storable.Offset.offset @"blue"      ptr)
      <*> peek (Foreign.Storable.Offset.offset @"alpha"     ptr)
      <*> peek (Foreign.Storable.Offset.offset @"frequency" ptr)

  poke ptr val = do
    pokeField @"red"       ptr val
    pokeField @"green"     ptr val
    pokeField @"blue"      ptr val
    pokeField @"alpha"     ptr val
    pokeField @"frequency" ptr val



data Spng_splt =
       Spng_splt
         { name         :: Ptr #{type char} -- ^ Array of 80 elements
         , sample_depth :: #type uint8_t
         , n_entries    :: #type uint32_t
         , entries      :: Ptr Spng_splt_entry
         }

instance Offset "name"         Spng_splt where rawOffset = #offset struct spng_splt, name
instance Offset "sample_depth" Spng_splt where rawOffset = #offset struct spng_splt, sample_depth
instance Offset "n_entries"    Spng_splt where rawOffset = #offset struct spng_splt, n_entries
instance Offset "entries"      Spng_splt where rawOffset = #offset struct spng_splt, entries

instance Storable Spng_splt where
  sizeOf    _ = #size      struct spng_splt
  alignment _ = #alignment struct spng_splt

  peek ptr =
    Spng_splt
      <$> peek (Foreign.Storable.Offset.offset @"name"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"sample_depth" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"n_entries"    ptr)
      <*> peek (Foreign.Storable.Offset.offset @"entries"      ptr)

  poke ptr val = do
    pokeField @"name"         ptr val
    pokeField @"sample_depth" ptr val
    pokeField @"n_entries"    ptr val
    pokeField @"entries"      ptr val



data Spng_time =
       Spng_time
         { year   :: #type uint16_t
         , month  :: #type uint8_t
         , day    :: #type uint8_t
         , hour   :: #type uint8_t
         , minute :: #type uint8_t
         , second :: #type uint8_t
         }

instance Offset "year"   Spng_time where rawOffset = #offset struct spng_time, year
instance Offset "month"  Spng_time where rawOffset = #offset struct spng_time, month
instance Offset "day"    Spng_time where rawOffset = #offset struct spng_time, day
instance Offset "hour"   Spng_time where rawOffset = #offset struct spng_time, hour
instance Offset "minute" Spng_time where rawOffset = #offset struct spng_time, minute
instance Offset "second" Spng_time where rawOffset = #offset struct spng_time, second

instance Storable Spng_time where
  sizeOf    _ = #size      struct spng_time
  alignment _ = #alignment struct spng_time

  peek ptr =
    Spng_time
      <$> peek (Foreign.Storable.Offset.offset @"year"   ptr)
      <*> peek (Foreign.Storable.Offset.offset @"month"  ptr)
      <*> peek (Foreign.Storable.Offset.offset @"day"    ptr)
      <*> peek (Foreign.Storable.Offset.offset @"hour"   ptr)
      <*> peek (Foreign.Storable.Offset.offset @"minute" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"second" ptr)

  poke ptr val = do
    pokeField @"year"   ptr val
    pokeField @"month"  ptr val
    pokeField @"day"    ptr val
    pokeField @"hour"   ptr val
    pokeField @"minute" ptr val
    pokeField @"second" ptr val



data Spng_offs =
       Spng_offs
         { x              :: #type int32_t
         , y              :: #type int32_t
         , unit_specifier :: #type uint8_t
         }

instance Offset "x"              Spng_offs where rawOffset = #offset struct spng_offs, x
instance Offset "y"              Spng_offs where rawOffset = #offset struct spng_offs, y
instance Offset "unit_specifier" Spng_offs where rawOffset = #offset struct spng_offs, unit_specifier

instance Storable Spng_offs where
  sizeOf    _ = #size      struct spng_offs
  alignment _ = #alignment struct spng_offs

  peek ptr =
    Spng_offs
      <$> peek (Foreign.Storable.Offset.offset @"x"              ptr)
      <*> peek (Foreign.Storable.Offset.offset @"y"              ptr)
      <*> peek (Foreign.Storable.Offset.offset @"unit_specifier" ptr)

  poke ptr val = do
    pokeField @"x"              ptr val
    pokeField @"y"              ptr val
    pokeField @"unit_specifier" ptr val



data Spng_exif =
       Spng_exif
         { length :: #type size_t
         , data_  :: Ptr #type char
         }

instance Offset "length" Spng_exif where rawOffset = #offset struct spng_exif, length
instance Offset "data_"  Spng_exif where rawOffset = #offset struct spng_exif, data

instance Offset "data" Spng_exif where
  rawOffset = rawOffset @"data_" @Spng_exif

instance HasField "data" Spng_exif (Ptr #{type char}) where
  getField = getField @"data_"

instance Storable Spng_exif where
  sizeOf    _ = #size      struct spng_exif
  alignment _ = #alignment struct spng_exif

  peek ptr =
    Spng_exif
      <$> peek (Foreign.Storable.Offset.offset @"length" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"data"   ptr)

  poke ptr val = do
    pokeField @"length" ptr val
    pokeField @"data"   ptr val



data Spng_chunk =
       Spng_chunk
         { offset :: #type size_t
         , length :: #type uint32_t
         , type_  :: Ptr #{type uint8_t} -- ^ Array of four elements
         , crc    :: #type uint32_t
         }

instance Offset "offset" Spng_chunk where rawOffset = #offset struct spng_chunk, offset
instance Offset "length" Spng_chunk where rawOffset = #offset struct spng_chunk, length
instance Offset "type_"  Spng_chunk where rawOffset = #offset struct spng_chunk, type
instance Offset "crc"    Spng_chunk where rawOffset = #offset struct spng_chunk, crc

instance Offset "type" Spng_chunk where
  rawOffset = rawOffset @"type_" @Spng_chunk

instance HasField "type" Spng_chunk (Ptr #{type uint8_t}) where
  getField = getField @"type_"

instance Storable Spng_chunk where
  sizeOf    _ = #size      struct spng_chunk
  alignment _ = #alignment struct spng_chunk

  peek ptr =
    Spng_chunk
      <$> peek (Foreign.Storable.Offset.offset @"offset" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"length" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"type"   ptr)
      <*> peek (Foreign.Storable.Offset.offset @"crc"    ptr)

  poke ptr val = do
    pokeField @"offset" ptr val
    pokeField @"length" ptr val
    pokeField @"type"   ptr val
    pokeField @"crc"    ptr val



type Spng_location = #type enum spng_location

pattern SPNG_AFTER_IHDR
      , SPNG_AFTER_PLTE
      , SPNG_AFTER_IDAT
     :: (Eq a, Num a) => a
pattern SPNG_AFTER_IHDR = #const SPNG_AFTER_IHDR
pattern SPNG_AFTER_PLTE = #const SPNG_AFTER_PLTE
pattern SPNG_AFTER_IDAT = #const SPNG_AFTER_IDAT



data Spng_unknown_chunk =
       Spng_unknown_chunk
         { type_    :: Ptr #{type uint8_t} -- ^ Array of four elements
         , length   :: #type size_t
         , data_    :: Ptr ()
         , location :: Spng_location
         }

instance Offset "type_"    Spng_unknown_chunk where rawOffset = #offset struct spng_unknown_chunk, type
instance Offset "length"   Spng_unknown_chunk where rawOffset = #offset struct spng_unknown_chunk, length
instance Offset "data_"    Spng_unknown_chunk where rawOffset = #offset struct spng_unknown_chunk, data
instance Offset "location" Spng_unknown_chunk where rawOffset = #offset struct spng_unknown_chunk, location

instance Offset "type" Spng_unknown_chunk where
  rawOffset = rawOffset @"type_" @Spng_unknown_chunk

instance Offset "data" Spng_unknown_chunk where
  rawOffset = rawOffset @"data_" @Spng_unknown_chunk

instance HasField "type" Spng_unknown_chunk (Ptr #{type uint8_t}) where
  getField = getField @"type_"

instance HasField "data" Spng_unknown_chunk (Ptr ()) where
  getField = getField @"data_"

instance Storable Spng_unknown_chunk where
  sizeOf    _ = #size      struct spng_unknown_chunk
  alignment _ = #alignment struct spng_unknown_chunk

  peek ptr =
    Spng_unknown_chunk
      <$> peek (Foreign.Storable.Offset.offset @"type"     ptr)
      <*> peek (Foreign.Storable.Offset.offset @"length"   ptr)
      <*> peek (Foreign.Storable.Offset.offset @"data"     ptr)
      <*> peek (Foreign.Storable.Offset.offset @"location" ptr)

  poke ptr val = do
    pokeField @"type"     ptr val
    pokeField @"length"   ptr val
    pokeField @"data"     ptr val
    pokeField @"location" ptr val



type Spng_option = #type enum spng_option

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
     :: (Eq a, Num a) => a
pattern SPNG_KEEP_UNKNOWN_CHUNKS       = #const SPNG_KEEP_UNKNOWN_CHUNKS
pattern SPNG_IMG_COMPRESSION_LEVEL     = #const SPNG_IMG_COMPRESSION_LEVEL
pattern SPNG_IMG_WINDOW_BITS           = #const SPNG_IMG_WINDOW_BITS
pattern SPNG_IMG_MEM_LEVEL             = #const SPNG_IMG_MEM_LEVEL
pattern SPNG_IMG_COMPRESSION_STRATEGY  = #const SPNG_IMG_COMPRESSION_STRATEGY
pattern SPNG_TEXT_COMPRESSION_LEVEL    = #const SPNG_TEXT_COMPRESSION_LEVEL
pattern SPNG_TEXT_WINDOW_BITS          = #const SPNG_TEXT_WINDOW_BITS
pattern SPNG_TEXT_MEM_LEVEL            = #const SPNG_TEXT_MEM_LEVEL
pattern SPNG_TEXT_COMPRESSION_STRATEGY = #const SPNG_TEXT_COMPRESSION_STRATEGY
pattern SPNG_FILTER_CHOICE             = #const SPNG_FILTER_CHOICE
pattern SPNG_CHUNK_COUNT_LIMIT         = #const SPNG_CHUNK_COUNT_LIMIT
pattern SPNG_ENCODE_TO_BUFFER          = #const SPNG_ENCODE_TO_BUFFER



type Spng_malloc_fn =
          #{type size_t} -- ^ size
       -> IO (Ptr ())

type Spng_realloc_fn =
          Ptr ()         -- ^ ptr
       -> #{type size_t} -- ^ size
       -> IO (Ptr ())

type Spng_calloc_fn =
          #{type size_t} -- ^ count
       -> #{type size_t} -- ^ size
       -> IO (Ptr ())

type Spng_free_fn =
          Ptr ()         -- ^ ptr
       -> IO ()



data Spng_alloc =
       Spng_alloc
         { malloc_fn  :: FunPtr Spng_malloc_fn
         , realloc_fn :: FunPtr Spng_realloc_fn
         , calloc_fn  :: FunPtr Spng_calloc_fn
         , free_fn    :: FunPtr Spng_free_fn
         }

instance Offset "malloc_fn"  Spng_alloc where rawOffset = #offset struct spng_alloc, malloc_fn
instance Offset "realloc_fn" Spng_alloc where rawOffset = #offset struct spng_alloc, realloc_fn
instance Offset "calloc_fn"  Spng_alloc where rawOffset = #offset struct spng_alloc, calloc_fn
instance Offset "free_fn"    Spng_alloc where rawOffset = #offset struct spng_alloc, free_fn

instance Storable Spng_alloc where
  sizeOf    _ = #size      struct spng_alloc
  alignment _ = #alignment struct spng_alloc

  peek ptr =
    Spng_alloc
      <$> peek (Foreign.Storable.Offset.offset @"malloc_fn"  ptr)
      <*> peek (Foreign.Storable.Offset.offset @"realloc_fn" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"calloc_fn"  ptr)
      <*> peek (Foreign.Storable.Offset.offset @"free_fn"    ptr)

  poke ptr val = do
    pokeField @"malloc_fn"  ptr val
    pokeField @"realloc_fn" ptr val
    pokeField @"calloc_fn"  ptr val
    pokeField @"free_fn"    ptr val



data Spng_row_info =
       Spng_row_info
         { scanline_idx :: #type uint32_t
         , row_num      :: #{type uint32_t} -- ^ deinterlaced row index
         , pass         :: #type int
         , filter       :: #type uint8_t
         }

instance Offset "scanline_idx" Spng_row_info where rawOffset = #offset struct spng_row_info, scanline_idx
instance Offset "row_num"      Spng_row_info where rawOffset = #offset struct spng_row_info, row_num
instance Offset "pass"         Spng_row_info where rawOffset = #offset struct spng_row_info, pass
instance Offset "filter"       Spng_row_info where rawOffset = #offset struct spng_row_info, filter

instance Storable Spng_row_info where
  sizeOf    _ = #size      struct spng_row_info
  alignment _ = #alignment struct spng_row_info

  peek ptr =
    Spng_row_info
      <$> peek (Foreign.Storable.Offset.offset @"scanline_idx" ptr)
      <*> peek (Foreign.Storable.Offset.offset @"row_num"      ptr)
      <*> peek (Foreign.Storable.Offset.offset @"pass"         ptr)
      <*> peek (Foreign.Storable.Offset.offset @"filter"       ptr)

  poke ptr val = do
    pokeField @"scanline_idx" ptr val
    pokeField @"row_num"      ptr val
    pokeField @"pass"         ptr val
    pokeField @"filter"       ptr val



data Spng_ctx



type Spng_read_fn =
          Ptr Spng_ctx   -- ^ ctx
       -> Ptr ()         -- ^ user
       -> Ptr ()         -- ^ dest
       -> #{type size_t} -- ^ length
       -> IO Spng_errno

type Spng_write_fn =
          Ptr Spng_ctx   -- ^ ctx
       -> Ptr ()         -- ^ user
       -> Ptr ()         -- ^ src
       -> #{type size_t} -- ^ length
       -> IO Spng_errno

type Spng_rw_fn =
          Ptr Spng_ctx   -- ^ ctx
       -> Ptr ()         -- ^ user
       -> Ptr ()         -- ^ dst_src
       -> #{type size_t} -- ^ length
       -> IO Spng_errno



foreign import CALLCV "spng.h spng_ctx_new"
  spng_ctx_new
    :: #{type int}       -- ^ flags
    -> IO (Ptr Spng_ctx)

foreign import CALLCV "spng.h spng_ctx_new2"
  spng_ctx_new2
    :: Ptr Spng_alloc    -- ^ alloc
    -> #{type int}       -- ^ flags
    -> IO (Ptr Spng_ctx)

foreign import CALLCV "spng.h spng_ctx_free"
  spng_ctx_free
    :: Ptr Spng_ctx -- ^ ctx
    -> IO ()

foreign import CALLCV "spng.h spng_set_png_buffer"
  spng_set_png_buffer
    :: Ptr Spng_ctx   -- ^ ctx
    -> Ptr ()         -- ^ const buf
    -> #{type size_t} -- ^ size
    -> IO #type int

foreign import CALLCV "spng.h spng_set_png_stream"
  spng_set_png_stream
    :: Ptr Spng_ctx      -- ^ ctx
    -> FunPtr Spng_rw_fn -- ^ rw_func
    -> Ptr ()            -- ^ user
    -> IO #type int

foreign import CALLCV "spng.h spng_set_png_file"
  spng_set_png_file
    :: Ptr Spng_ctx -- ^ ctx
    -> Ptr CFile    -- ^ file
    -> IO #type int

foreign import CALLCV "spng.h spng_get_png_buffer"
  spng_get_png_buffer
    :: Ptr Spng_ctx       -- ^ ctx
    -> Ptr #{type size_t} -- ^ len
    -> Ptr Spng_errno     -- ^ error
    -> IO (Ptr ())

foreign import CALLCV "spng.h spng_set_image_limits"
  spng_set_image_limits
    :: Ptr Spng_ctx     -- ^ ctx
    -> #{type uint32_t} -- ^ width
    -> #{type uint32_t} -- ^ height
    -> IO #type int

foreign import CALLCV "spng.h spng_get_image_limits"
  spng_get_image_limits
    :: Ptr Spng_ctx         -- ^ ctx
    -> Ptr #{type uint32_t} -- ^ width
    -> Ptr #{type uint32_t} -- ^ height
    -> IO #type int

foreign import CALLCV "spng.h spng_set_chunk_limits"
  spng_set_chunk_limits
    :: Ptr Spng_ctx   -- ^ ctx
    -> #{type size_t} -- ^ chunk_size
    -> #{type size_t} -- ^ cache_size
    -> IO #type int

foreign import CALLCV "spng.h spng_get_chunk_limits"
  spng_get_chunk_limits
    :: Ptr Spng_ctx       -- ^ ctx
    -> Ptr #{type size_t} -- ^ chunk_size
    -> Ptr #{type size_t} -- ^ cache_size
    -> IO #type int

foreign import CALLCV "spng.h spng_set_crc_action"
  spng_set_crc_action
    :: Ptr Spng_ctx -- ^ ctx
    -> #{type int}  -- ^ critical
    -> #{type int}  -- ^ ancillary
    -> IO #type int

foreign import CALLCV "spng.h spng_set_option"
  spng_set_option
    :: Ptr Spng_ctx -- ^ ctx
    -> Spng_option  -- ^ option
    -> #{type int}  -- ^ value
    -> IO #type int

foreign import CALLCV "spng.h spng_get_option"
  spng_get_option
    :: Ptr Spng_ctx    -- ^ ctx
    -> Spng_option     -- ^ option
    -> Ptr #{type int} -- ^ value
    -> IO #type int

foreign import CALLCV "spng.h spng_decoded_image_size"
  spng_decoded_image_size
    :: Ptr Spng_ctx       -- ^ ctx
    -> Spng_format        -- ^ fmt
    -> Ptr #{type size_t} -- ^ len
    -> IO #type int

foreign import CALLCV "spng.h spng_decode_image"
  spng_decode_image
    :: Ptr Spng_ctx      -- ^ ctx
    -> Ptr ()            -- ^ out
    -> #{type size_t}    -- ^ len
    -> Spng_format       -- ^ fmt
    -> Spng_decode_flags -- ^ flags
    -> IO #type int

foreign import CALLCV "spng.h spng_decode_scanline"
  spng_decode_scanline
    :: Ptr Spng_ctx   -- ^ ctx
    -> Ptr ()         -- ^ out
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import CALLCV "spng.h spng_decode_row"
  spng_decode_row
    :: Ptr Spng_ctx   -- ^ ctx
    -> Ptr ()         -- ^ out
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import CALLCV "spng.h spng_decode_chunks"
  spng_decode_chunks
    :: Ptr Spng_ctx -- ^ ctx
    -> IO #type int

foreign import CALLCV "spng.h spng_get_row_info"
  spng_get_row_info
    :: Ptr Spng_ctx      -- ^ ctx
    -> Ptr Spng_row_info -- ^ row_info
    -> IO #type int

foreign import CALLCV "spng.h spng_encode_image"
  spng_encode_image
    :: Ptr Spng_ctx      -- ^ ctx
    -> Ptr ()            -- ^ const img
    -> #{type size_t}    -- ^ len
    -> Spng_format       -- ^ fmt
    -> Spng_encode_flags -- ^ flags
    -> IO #type int

foreign import CALLCV "spng.h spng_encode_scanline"
  spng_encode_scanline
    :: Ptr Spng_ctx   -- ^ ctx
    -> Ptr ()         -- ^ const scanline
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import CALLCV "spng.h spng_encode_row"
  spng_encode_row
    :: Ptr Spng_ctx   -- ^ ctx
    -> Ptr ()         -- ^ const row
    -> #{type size_t} -- ^ len
    -> IO #type int

foreign import CALLCV "spng.h spng_encode_chunks"
  spng_encode_chunks
    :: Ptr Spng_ctx    -- ^ ctx
    -> IO #type int

foreign import CALLCV "spng.h spng_get_ihdr"
  spng_get_ihdr
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_ihdr -- ^ ihdr
    -> IO #type int

foreign import CALLCV "spng.h spng_get_plte"
  spng_get_plte
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_plte -- ^ plte
    -> IO #type int

foreign import CALLCV "spng.h spng_get_trns"
  spng_get_trns
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_trns -- ^ trns
    -> IO #type int

foreign import CALLCV "spng.h spng_get_chrm"
  spng_get_chrm
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_chrm -- ^ chrm
    -> IO #type int

foreign import CALLCV "spng.h spng_get_chrm_int"
  spng_get_chrm_int
    :: Ptr Spng_ctx      -- ^ ctx
    -> Ptr Spng_chrm_int -- ^ chrm_int
    -> IO #type int

foreign import CALLCV "spng.h spng_get_gama"
  spng_get_gama
    :: Ptr Spng_ctx       -- ^ ctx
    -> Ptr #{type double} -- ^ gamma
    -> IO #type int

foreign import CALLCV "spng.h spng_get_gama_int"
  spng_get_gama_int
    :: Ptr Spng_ctx         -- ^ ctx
    -> Ptr #{type uint32_t} -- ^ gama_int
    -> IO #type int

foreign import CALLCV "spng.h spng_get_iccp"
  spng_get_iccp
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_iccp -- ^ iccp
    -> IO #type int

foreign import CALLCV "spng.h spng_get_sbit"
  spng_get_sbit
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_sbit -- ^ sbit
    -> IO #type int

foreign import CALLCV "spng.h spng_get_srgb"
  spng_get_srgb
    :: Ptr Spng_ctx        -- ^ ctx
    -> Ptr #{type uint8_t} -- ^ rendering_intent
    -> IO #type int

foreign import CALLCV "spng.h spng_get_text"
  spng_get_text
    :: Ptr Spng_ctx         -- ^ ctx
    -> Ptr Spng_text        -- ^ text
    -> Ptr #{type uint32_t} -- ^ n_text
    -> IO #type int

foreign import CALLCV "spng.h spng_get_bkgd"
  spng_get_bkgd
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_bkgd -- ^ bkgd
    -> IO #type int

foreign import CALLCV "spng.h spng_get_hist"
  spng_get_hist
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_hist -- ^ hist
    -> IO #type int

foreign import CALLCV "spng.h spng_get_phys"
  spng_get_phys
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_phys -- ^ phys
    -> IO #type int

foreign import CALLCV "spng.h spng_get_splt"
  spng_get_splt
    :: Ptr Spng_ctx         -- ^ ctx
    -> Ptr Spng_splt        -- ^ splt
    -> Ptr #{type uint32_t} -- ^ n_splt
    -> IO #type int

foreign import CALLCV "spng.h spng_get_time"
  spng_get_time
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_time -- ^ time
    -> IO #type int

foreign import CALLCV "spng.h spng_get_unknown_chunks"
  spng_get_unknown_chunks
    :: Ptr Spng_ctx           -- ^ ctx
    -> Ptr Spng_unknown_chunk -- ^ chunks
    -> Ptr #{type uint32_t}   -- ^ n_chunks
    -> IO #type int

foreign import CALLCV "spng.h spng_get_offs"
  spng_get_offs
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_offs -- ^ offs
    -> IO #type int

foreign import CALLCV "spng.h spng_get_exif"
  spng_get_exif
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_exif -- ^ exif
    -> IO #type int

foreign import CALLCV "spng.h spng_set_ihdr"
  spng_set_ihdr
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_ihdr -- ^ ihdr
    -> IO #type int

foreign import CALLCV "spng.h spng_set_plte"
  spng_set_plte
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_plte -- ^ plte
    -> IO #type int

foreign import CALLCV "spng.h spng_set_trns"
  spng_set_trns
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_trns -- ^ trns
    -> IO #type int

foreign import CALLCV "spng.h spng_set_chrm"
  spng_set_chrm
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_chrm -- ^ chrm
    -> IO #type int

foreign import CALLCV "spng.h spng_set_chrm_int"
  spng_set_chrm_int
    :: Ptr Spng_ctx      -- ^ ctx
    -> Ptr Spng_chrm_int -- ^ chrm_int
    -> IO #type int

foreign import CALLCV "spng.h spng_set_gama"
  spng_set_gama
    :: Ptr Spng_ctx   -- ^ ctx
    -> #{type double} -- ^ gamma
    -> IO #type int

foreign import CALLCV "spng.h spng_set_gama_int"
  spng_set_gama_int
    :: Ptr Spng_ctx     -- ^ ctx
    -> #{type uint32_t} -- ^ gamma
    -> IO #type int

foreign import CALLCV "spng.h spng_set_iccp"
  spng_set_iccp
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_iccp -- ^ iccp
    -> IO #type int

foreign import CALLCV "spng.h spng_set_sbit"
  spng_set_sbit
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_sbit -- ^ sbit
    -> IO #type int

foreign import CALLCV "spng.h spng_set_srgb"
  spng_set_srgb
    :: Ptr Spng_ctx    -- ^ ctx
    -> #{type uint8_t} -- ^ rendering_intent
    -> IO #type int

foreign import CALLCV "spng.h spng_set_text"
  spng_set_text
    :: Ptr Spng_ctx     -- ^ ctx
    -> Ptr Spng_text    -- ^ text
    -> #{type uint32_t} -- ^ n_text
    -> IO #type int

foreign import CALLCV "spng.h spng_set_bkgd"
  spng_set_bkgd
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_bkgd -- ^ bkgd
    -> IO #type int

foreign import CALLCV "spng.h spng_set_hist"
  spng_set_hist
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_hist -- ^ hist
    -> IO #type int

foreign import CALLCV "spng.h spng_set_phys"
  spng_set_phys
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_phys -- ^ phys
    -> IO #type int

foreign import CALLCV "spng.h spng_set_splt"
  spng_set_splt
    :: Ptr Spng_ctx     -- ^ ctx
    -> Ptr Spng_splt    -- ^ splt
    -> #{type uint32_t} -- ^ n_splt
    -> IO #type int

foreign import CALLCV "spng.h spng_set_time"
  spng_set_time
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_time -- ^ time
    -> IO #type int

foreign import CALLCV "spng.h spng_set_unknown_chunks"
  spng_set_unknown_chunks
    :: Ptr Spng_ctx           -- ^ ctx
    -> Ptr Spng_unknown_chunk -- ^ chunks
    -> #{type uint32_t}       -- ^ n_chunks
    -> IO #type int

foreign import CALLCV "spng.h spng_set_offs"
  spng_set_offs
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_offs -- ^ offs
    -> IO #type int

foreign import CALLCV "spng.h spng_set_exif"
  spng_set_exif
    :: Ptr Spng_ctx  -- ^ ctx
    -> Ptr Spng_exif -- ^ exif
    -> IO #type int

foreign import CALLCV "spng.h spng_strerror"
  spng_strerror
    :: #{type int}           -- ^ err
    -> IO (Ptr #{type char}) -- ^ const

foreign import CALLCV "spng.h spng_version_string"
  spng_version_string
    :: IO (Ptr #{type char}) -- ^ const
