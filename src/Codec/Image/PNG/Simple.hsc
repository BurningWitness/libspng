{-# LANGUAGE DataKinds
           , ExplicitNamespaces
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , PatternSynonyms
           , ScopedTypeVariables
           , TypeApplications #-}

{-| Documentation for libspng can be found [here](https://libspng.org/docs/api/).
 
    Basic C example can be viewed [here](https://libspng.org/docs/usage/);
    a more complex Haskell example is
    [here](https://github.com/BurningWitness/libspng/blob/initial/app/example/Main.hsc).

    Helper functions are provided right after 'spng_ctx_new', 'spng_set_png_stream',
    'spng_set_option' and 'spng_get_option'.
 -}

module Codec.Image.PNG.Simple
  ( -- ** Error type
    SpngError (..)
    -- ** Version
  , pattern SPNG_VERSION_MAJOR
  , pattern SPNG_VERSION_MINOR
  , pattern SPNG_VERSION_PATCH
    -- ** spng_errno
  , SpngErrNo
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
  , SpngTextType
  , pattern SPNG_TEXT
  , pattern SPNG_ZTXT
  , pattern SPNG_ITXT
    -- ** spng_color_type
  , SpngColorType
  , pattern SPNG_COLOR_TYPE_GRAYSCALE
  , pattern SPNG_COLOR_TYPE_TRUECOLOR
  , pattern SPNG_COLOR_TYPE_INDEXED
  , pattern SPNG_COLOR_TYPE_GRAYSCALE_ALPHA
  , pattern SPNG_COLOR_TYPE_TRUECOLOR_ALPHA
    -- ** spng_filter
  , SpngFilter
  , pattern SPNG_FILTER_NONE
  , pattern SPNG_FILTER_SUB
  , pattern SPNG_FILTER_UP
  , pattern SPNG_FILTER_AVERAGE
  , pattern SPNG_FILTER_PAETH
    -- ** spng_filter_choice
  , SpngFilterChoice
  , pattern SPNG_DISABLE_FILTERING
  , pattern SPNG_FILTER_CHOICE_NONE
  , pattern SPNG_FILTER_CHOICE_SUB
  , pattern SPNG_FILTER_CHOICE_UP
  , pattern SPNG_FILTER_CHOICE_AVG
  , pattern SPNG_FILTER_CHOICE_PAETH
  , pattern SPNG_FILTER_CHOICE_ALL
    -- ** spng_interlace_method
  , SpngInterlaceMethod
  , pattern SPNG_INTERLACE_NONE
  , pattern SPNG_INTERLACE_ADAM7
    -- ** spng_format
    -- | Channels are always in byte-order
  , SpngFormat
  , pattern SPNG_FMT_RGBA8
  , pattern SPNG_FMT_RGBA16
  , pattern SPNG_FMT_RGB8
    -- | Partially implemented, see documentation
  , pattern SPNG_FMT_GA8
  , pattern SPNG_FMT_GA16
  , pattern SPNG_FMT_G8
    -- | No conversion or scaling
  , pattern SPNG_FMT_PNG
  , pattern SPNG_FMT_RAW
    -- ** spng_ctx_flags
  , SpngCtxFlags
  , pattern SPNG_CTX_IGNORE_ADLER32
  , pattern SPNG_CTX_ENCODER
    -- ** spng_decode_flags
  , SpngDecodeFlags
  , pattern SPNG_DECODE_TRNS
  , pattern SPNG_DECODE_GAMMA
  , pattern SPNG_DECODE_USE_SBIT
  , pattern SPNG_DECODE_PROGRESSIVE
    -- ** spng_crc_action
  , SpngCrcAction
  , pattern SPNG_CRC_ERROR
  , pattern SPNG_CRC_DISCARD
  , pattern SPNG_CRC_USE
    -- ** spng_encode_flags
  , SpngEncodeFlags
  , pattern SPNG_ENCODE_PROGRESSIVE
  , pattern SPNG_ENCODE_FINALIZE
    -- ** spng_ihdr
  , SpngIhdr (..)
    -- ** spng_plte_entry
  , SpngPlteEntry (..)
    -- ** spng_plte
  , SpngPlte (..)
    -- ** spng_trns
  , SpngTrns (..)
    -- ** spng_chrm_int
  , SpngChrmInt (..)
    -- ** spng_chrm
  , SpngChrm (..)
    -- ** spng_iccp
  , SpngIccp (..)
    -- ** spng_sbit
  , SpngSbit (..)
    -- ** spng_text
  , SpngText (..)
    -- ** spng_bkgd
  , SpngBkgd (..)
    -- ** spng_hist
  , SpngHist (..)
    -- ** spng_phys
  , SpngPhys (..)
    -- ** spng_splt_entry
  , SpngSpltEntry (..)
    -- ** spng_splt
  , SpngSplt (..)
    -- ** spng_time
  , SpngTime (..)
    -- ** spng_offs
  , SpngOffs (..)
    -- ** spng_exif
  , SpngExif (..)
    -- ** spng_chunk
  , SpngChunk (..)
    -- ** spng_location
  , SpngLocation
  , pattern SPNG_AFTER_IHDR
  , pattern SPNG_AFTER_PLTE
  , pattern SPNG_AFTER_IDAT
    -- ** spng_unknown_chunk
  , SpngUnknownChunk (..)
    -- ** spng_option
  , SpngOption
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
  , type SpngMallocFn
  , mkSpngMallocFn
    -- ** spng_realloc_fn
  , type SpngReallocFn
  , mkSpngReallocFn
    -- ** spng_calloc_fn
  , type SpngCallocFn
  , mkSpngCallocFn
    -- ** spng_free_fn
  , type SpngFreeFn
  , mkSpngFreeFn
    -- ** spng_alloc
  , SpngAlloc (..)
    -- ** spng_row_info
  , SpngRowInfo (..)
    -- ** spng_ctx
  , SpngCtx
    -- ** spng_read_fn
  , SpngReadFn
  , mkSpngReadFn
    -- ** spng_write_fn
  , SpngWriteFn
  , mkSpngWriteFn
    -- ** spng_rw_fn
  , SpngRwFn
  , mkSpngRwFn
    -- ** spng_ctx_new
  , spng_ctx_new
  , spng_ctx_with
    -- ** spng_ctx_new2
  , spng_ctx_new2
    -- ** spng_ctx_free
  , spng_ctx_free
    -- ** spng_set_png_buffer
  , spng_set_png_buffer
    -- ** spng_set_png_stream
  , spng_set_png_stream
  , hReadFn
  , hWriteFn
  , MemRead (..)
  , mkMemRead
  , memReadFn
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
  , spng_set_keep_unknown_chunks
  , spng_set_filter_choice
  , spng_set_encode_to_buffer
    -- ** spng_get_option
  , spng_get_option
  , spng_get_keep_unknown_chunks
  , spng_get_filter_choice
  , spng_get_encode_to_buffer   
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

import           Codec.Image.PNG.Simple.Error.Internal
import           Codec.Image.PNG.Simple.Internal

import           Control.Exception
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe
import           Data.Int
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset
import           System.IO
import           System.IO.Unsafe

#include "spng.h"

spng_ctx_new :: SpngCtxFlags -> IO (Ptr SpngCtx)
spng_ctx_new (SpngCtxFlags flags) = spng_ctx_new' $ fromIntegral flags

-- | @'spng_ctx_with' flags f@ creates a new context and frees it after @f@ completes.
--   The passed pointer therefore must not be used outside of @f@.
spng_ctx_with :: SpngCtxFlags -> (Ptr SpngCtx -> IO a) -> IO a
spng_ctx_with flags = bracket (spng_ctx_new flags) spng_ctx_free



spng_set_png_buffer
  :: Ptr SpngCtx    -- ^ ctx
  -> Ptr ()         -- ^ const buf
  -> #{type size_t} -- ^ size
  -> IO ()
spng_set_png_buffer ctx buf siz = wrapError $ spng_set_png_buffer' ctx buf siz

-- | 'SpngReadFn', 'SpngWriteFn' and 'SpngRwFn' are interchangeable.
spng_set_png_stream
  :: Ptr SpngCtx     -- ^ ctx
  -> FunPtr SpngRwFn -- ^ rw_func
  -> Ptr ()          -- ^ user
  -> IO ()
spng_set_png_stream ctx rw user = wrapError $ spng_set_png_stream' ctx rw user

-- | Creates 'SpngReadFn' from a 'Handle'.
--
--   Resulting function never accesses passed 'SpngCtx' or the user pointer.
hReadFn :: Handle -> SpngReadFn
hReadFn h = \_ _ dest len ->
  handle (\(_ :: IOException) -> return SPNG_IO_ERROR) $ do
    n <- hGetBuf h dest (fromIntegral len)
    case n of
      0 -> return SPNG_IO_EOF
      _ -> return SPNG_OK

-- | Creates 'SpngWriteFn' from a 'Handle'.
--
--   Resulting function never accesses passed 'SpngCtx' or the user pointer.
hWriteFn :: Handle -> SpngWriteFn
hWriteFn h = \_ _ src len ->
  handle (\(_ :: IOException) -> return SPNG_IO_ERROR) $ do
    hPutBuf h src (fromIntegral len)
    return SPNG_OK

-- | Intermediate data structure for reading PNG files stored in memory.
data MemRead =
       MemRead
         { mrPtr    :: Ptr ()         -- ^ points to the start of the image
         , mrLen    :: #{type size_t} -- ^ total length of the image
         , mrOffset :: #{type size_t} -- ^ current offset
         }

instance Offset "mrPtr"    MemRead where rawOffset = 0
instance Offset "mrLen"    MemRead where rawOffset = sizeOf (undefined :: Ptr ())
instance Offset "mrOffset" MemRead where
  rawOffset = sizeOf (undefined :: Ptr ()) + sizeOf (undefined :: #{type size_t})

instance Storable MemRead where
  sizeOf _    = sizeOf (undefined :: Ptr ()) + 2 * sizeOf (undefined :: #{type size_t})
  alignment _ = alignment (undefined :: Ptr ())

  peek ptr =
    MemRead
      <$> peek (offset @"mrPtr"    ptr)
      <*> peek (offset @"mrLen"    ptr)
      <*> peek (offset @"mrOffset" ptr)

  poke ptr val = do
    pokeField @"mrPtr"    ptr val
    pokeField @"mrLen"    ptr val
    pokeField @"mrOffset" ptr val

-- | Smart constructor for 'MemRead'.
mkMemRead :: Ptr a -> #{type size_t} -> MemRead
mkMemRead ptr len = MemRead (castPtr ptr) len 0

-- | Creates 'SpngWriteFn' from a pointer to 'MemRead'.
--
--   Resulting function never accesses passed 'SpngCtx' or the user pointer.
memReadFn :: Ptr MemRead -> SpngReadFn
memReadFn mem = \_ _ dest len -> do
  handle (\(_ :: IOException) -> return SPNG_IO_ERROR) $ do
    MemRead src slen off <- peek mem
    case () of
      () | off >= slen      -> return SPNG_IO_EOF
         | off + len > slen -> do
             copyBytes dest (plusPtr src $ fromIntegral off) (fromIntegral $ off - len)
             poke mem $ MemRead src slen slen
             return SPNG_OK
         | otherwise        -> do
             copyBytes dest (plusPtr src $ fromIntegral off) (fromIntegral len)
             poke mem $ MemRead src slen (off + len)
             return SPNG_OK

spng_set_png_file
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr CFile    -- ^ file
  -> IO ()
spng_set_png_file ctx file = wrapError $ spng_set_png_file' ctx file

spng_get_png_buffer
  :: Ptr SpngCtx                                    -- ^ ctx
  -> IO (SpngErrNo, Maybe (Ptr (), #{type size_t})) -- ^ (error, Maybe (buf, len))
spng_get_png_buffer ctx =
  allocaArray 2 $ \lenptr -> do
    let errptr = castPtr $ advancePtr lenptr 1
    bufptr <- spng_get_png_buffer' ctx lenptr errptr
    err <- peek errptr
    if bufptr == nullPtr
      then return (err, Nothing)
      else do len <- peek lenptr
              return (err, Just (bufptr, len))

spng_set_image_limits
  :: Ptr SpngCtx      -- ^ ctx
  -> #{type uint32_t} -- ^ width
  -> #{type uint32_t} -- ^ height
  -> IO ()
spng_set_image_limits ctx width height =
  wrapError $ spng_set_image_limits' ctx width height

spng_get_image_limits
  :: Ptr SpngCtx                             -- ^ ctx
  -> IO (#{type uint32_t}, #{type uint32_t}) -- ^ (width, height)
spng_get_image_limits ctx = do
  allocaArray 2 $ \ptrX -> do
    let ptrY = advancePtr ptrX 1
    wrapError $ spng_get_image_limits' ctx ptrX ptrY
    (,) <$> peek ptrX <*> peek ptrY

spng_set_chunk_limits
  :: Ptr SpngCtx    -- ^ ctx
  -> #{type size_t} -- ^ chunk_size
  -> #{type size_t} -- ^ cache_size
  -> IO ()
spng_set_chunk_limits ctx chunk cache =
  wrapError $ spng_set_chunk_limits' ctx chunk cache

spng_get_chunk_limits
  :: Ptr SpngCtx                         -- ^ ctx
  -> IO (#{type size_t}, #{type size_t}) -- ^ (chunk_size, cache_size)
spng_get_chunk_limits ctx = do
  allocaArray 2 $ \ptrX -> do
    let ptrY = advancePtr ptrX 1
    wrapError $ spng_get_chunk_limits' ctx ptrX ptrY
    (,) <$> peek ptrX <*> peek ptrY

spng_set_crc_action
  :: Ptr SpngCtx -- ^ ctx
  -> SpngCrcAction -- ^ critical
  -> SpngCrcAction -- ^ ancillary
  -> IO ()
spng_set_crc_action ctx (SpngCrcAction critical) (SpngCrcAction ancillary) =
  wrapError $ spng_set_crc_action' ctx (fromIntegral critical) (fromIntegral ancillary)

spng_set_option
  :: Ptr SpngCtx -- ^ ctx
  -> SpngOption  -- ^ option
  -> #{type int} -- ^ value
  -> IO ()
spng_set_option ctx option = wrapError . spng_set_option' ctx option

-- | 'spng_set_option' applied to 'SPNG_KEEP_UNKNOWN_CHUNKS'.
spng_set_keep_unknown_chunks :: Ptr SpngCtx -> Bool -> IO ()
spng_set_keep_unknown_chunks ctx val =
  spng_set_option ctx SPNG_KEEP_UNKNOWN_CHUNKS $ if val then 1 else 0

-- | 'spng_set_option' applied to 'SPNG_FILTER_CHOICE'.
spng_set_filter_choice :: Ptr SpngCtx -> SpngFilterChoice -> IO ()
spng_set_filter_choice ctx (SpngFilterChoice flags) =
  spng_set_option ctx SPNG_FILTER_CHOICE $ fromIntegral flags

-- | 'spng_set_option' applied to 'SPNG_ENCODE_TO_BUFFER'.
spng_set_encode_to_buffer :: Ptr SpngCtx -> Bool -> IO ()
spng_set_encode_to_buffer ctx val =
  spng_set_option ctx SPNG_ENCODE_TO_BUFFER $ if val then 1 else 0

spng_get_option
  :: Ptr SpngCtx    -- ^ ctx
  -> SpngOption     -- ^ option
  -> IO #{type int} -- ^ value
spng_get_option ctx option =
  alloca $ \ptr -> do
    wrapError $ spng_get_option' ctx option ptr
    peek ptr

-- | 'spng_get_option' applied to 'SPNG_KEEP_UNKNOWN_CHUNKS'.
spng_get_keep_unknown_chunks :: Ptr SpngCtx -> IO Bool
spng_get_keep_unknown_chunks ctx = do
  res <- spng_get_option ctx SPNG_KEEP_UNKNOWN_CHUNKS
  return $ if res == 0 then False else True

-- | 'spng_get_option' applied to 'SPNG_FILTER_CHOICE'.
spng_get_filter_choice :: Ptr SpngCtx -> IO SpngFilterChoice
spng_get_filter_choice ctx =
  SpngFilterChoice . fromIntegral <$> spng_get_option ctx SPNG_FILTER_CHOICE

-- | 'spng_get_option' applied to 'SPNG_ENCODE_TO_BUFFER'.
spng_get_encode_to_buffer :: Ptr SpngCtx -> IO Bool
spng_get_encode_to_buffer ctx = do
  res <- spng_get_option ctx SPNG_ENCODE_TO_BUFFER
  return $ if res == 0 then False else True

spng_decoded_image_size
  :: Ptr SpngCtx       -- ^ ctx
  -> SpngFormat        -- ^ fmt
  -> IO #{type size_t} -- ^ len
spng_decoded_image_size ctx fmt =
  alloca $ \ptr -> do
    wrapError $ spng_decoded_image_size' ctx fmt ptr
    peek ptr

spng_decode_image
  :: Ptr SpngCtx     -- ^ ctx
  -> Ptr ()          -- ^ out
  -> #{type size_t}  -- ^ len
  -> SpngFormat      -- ^ fmt
  -> SpngDecodeFlags -- ^ flags
  -> IO ()
spng_decode_image ctx out len fmt flags =
  wrapError $ spng_decode_image' ctx out len fmt flags

spng_decode_scanline
  :: Ptr SpngCtx    -- ^ ctx
  -> Ptr ()         -- ^ out
  -> #{type size_t} -- ^ len
  -> IO ()
spng_decode_scanline ctx out len = wrapError $ spng_decode_scanline' ctx out len

spng_decode_row
  :: Ptr SpngCtx    -- ^ ctx
  -> Ptr ()         -- ^ out
  -> #{type size_t} -- ^ len
  -> IO ()
spng_decode_row ctx out len = wrapError $ spng_decode_row' ctx out len

spng_decode_chunks
  :: Ptr SpngCtx -- ^ ctx
  -> IO ()
spng_decode_chunks ctx = wrapError $ spng_decode_chunks' ctx

spng_get_row_info
  :: Ptr SpngCtx     -- ^ ctx
  -> Ptr SpngRowInfo -- ^ row_info
  -> IO ()
spng_get_row_info ctx ptr = wrapError $ spng_get_row_info' ctx ptr

spng_encode_image
  :: Ptr SpngCtx     -- ^ ctx
  -> Ptr ()          -- ^ const img
  -> #{type size_t}  -- ^ len
  -> SpngFormat      -- ^ fmt
  -> SpngEncodeFlags -- ^ flags
  -> IO ()
spng_encode_image ctx img len fmt flags =
  wrapError $ spng_encode_image' ctx img len fmt flags

spng_encode_scanline
  :: Ptr SpngCtx    -- ^ ctx
  -> Ptr ()         -- ^ const scanline
  -> #{type size_t} -- ^ len
  -> IO ()
spng_encode_scanline ctx scanline len = wrapError $ spng_encode_scanline' ctx scanline len

spng_encode_row
  :: Ptr SpngCtx    -- ^ ctx
  -> Ptr ()         -- ^ const row
  -> #{type size_t} -- ^ len
  -> IO ()
spng_encode_row ctx row len = wrapError $ spng_encode_row' ctx row len

spng_encode_chunks
  :: Ptr SpngCtx    -- ^ ctx
  -> IO ()
spng_encode_chunks ctx = wrapError $ spng_encode_chunks' ctx

spng_get_ihdr
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngIhdr -- ^ ihdr
  -> IO ()
spng_get_ihdr ctx ihdr = wrapError $ spng_get_ihdr' ctx ihdr

spng_get_plte
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngPlte -- ^ plte
  -> IO ()
spng_get_plte ctx plte = wrapError $ spng_get_plte' ctx plte

spng_get_trns
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngTrns -- ^ trns
  -> IO ()
spng_get_trns ctx trns = wrapError $ spng_get_trns' ctx trns

spng_get_chrm
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngChrm -- ^ chrm
  -> IO ()
spng_get_chrm ctx chrm = wrapError $ spng_get_chrm' ctx chrm

spng_get_chrm_int
  :: Ptr SpngCtx     -- ^ ctx
  -> Ptr SpngChrmInt -- ^ chrm_int
  -> IO ()
spng_get_chrm_int ctx chrm_int = wrapError $ spng_get_chrm_int' ctx chrm_int

spng_get_gama
  :: Ptr SpngCtx       -- ^ ctx
  -> IO #{type double} -- ^ gamma
spng_get_gama ctx =
  alloca $ \ptr -> do
    wrapError $ spng_get_gama' ctx ptr
    peek ptr

spng_get_gama_int
  :: Ptr SpngCtx         -- ^ ctx
  -> IO #{type uint32_t} -- ^ gama_int
spng_get_gama_int ctx =
  alloca $ \ptr -> do
    wrapError $ spng_get_gama_int' ctx ptr
    peek ptr

spng_get_iccp
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngIccp -- ^ iccp
  -> IO ()
spng_get_iccp ctx iccp = wrapError $ spng_get_iccp' ctx iccp

spng_get_sbit
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngSbit -- ^ sbit
  -> IO ()
spng_get_sbit ctx sbit = wrapError $ spng_get_sbit' ctx sbit

spng_get_srgb
  :: Ptr SpngCtx         -- ^ ctx
  -> Ptr #{type uint8_t} -- ^ rendering_intent
  -> IO ()
spng_get_srgb ctx srgb = wrapError $ spng_get_srgb' ctx srgb

spng_get_text
  :: Ptr SpngCtx          -- ^ ctx
  -> Ptr SpngText         -- ^ text
  -> Ptr #{type uint32_t} -- ^ n_text
  -> IO ()
spng_get_text ctx text n = wrapError $ spng_get_text' ctx text n

spng_get_bkgd
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngBkgd -- ^ bkgd
  -> IO ()
spng_get_bkgd ctx bkgd = wrapError $ spng_get_bkgd' ctx bkgd

spng_get_hist
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngHist -- ^ hist
  -> IO ()
spng_get_hist ctx hist = wrapError $ spng_get_hist' ctx hist

spng_get_phys
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngPhys -- ^ phys
  -> IO ()
spng_get_phys ctx phys = wrapError $ spng_get_phys' ctx phys

spng_get_splt
  :: Ptr SpngCtx          -- ^ ctx
  -> Ptr SpngSplt         -- ^ splt
  -> Ptr #{type uint32_t} -- ^ n_splt
  -> IO ()
spng_get_splt ctx splt n = wrapError $ spng_get_splt' ctx splt n

spng_get_time
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngTime -- ^ time
  -> IO ()
spng_get_time ctx time = wrapError $ spng_get_time' ctx time

spng_get_unknown_chunks
  :: Ptr SpngCtx          -- ^ ctx
  -> Ptr SpngUnknownChunk -- ^ chunks
  -> Ptr #{type uint32_t} -- ^ n_chunks
  -> IO ()
spng_get_unknown_chunks ctx chunks n = wrapError $ spng_get_unknown_chunks' ctx chunks n

spng_get_offs
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngOffs -- ^ offs
  -> IO ()
spng_get_offs ctx offs = wrapError $ spng_get_offs' ctx offs

spng_get_exif
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngExif -- ^ exif
  -> IO ()
spng_get_exif ctx exif = wrapError $ spng_get_exif' ctx exif

spng_set_ihdr
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngIhdr -- ^ ihdr
  -> IO ()
spng_set_ihdr ctx ihdr = wrapError $ spng_set_ihdr' ctx ihdr

spng_set_plte
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngPlte -- ^ plte
  -> IO ()
spng_set_plte ctx plte = wrapError $ spng_set_plte' ctx plte

spng_set_trns
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngTrns -- ^ trns
  -> IO ()
spng_set_trns ctx trns = wrapError $ spng_set_trns' ctx trns

spng_set_chrm
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngChrm -- ^ chrm
  -> IO ()
spng_set_chrm ctx chrm = wrapError $ spng_set_chrm' ctx chrm

spng_set_chrm_int
  :: Ptr SpngCtx     -- ^ ctx
  -> Ptr SpngChrmInt -- ^ chrm_int
  -> IO ()
spng_set_chrm_int ctx chrm_int = wrapError $ spng_set_chrm_int' ctx chrm_int

spng_set_gama
  :: Ptr SpngCtx    -- ^ ctx
  -> #{type double} -- ^ gamma
  -> IO ()
spng_set_gama ctx gama = wrapError $ spng_set_gama' ctx gama

spng_set_gama_int
  :: Ptr SpngCtx      -- ^ ctx
  -> #{type uint32_t} -- ^ gamma
  -> IO ()
spng_set_gama_int ctx gama_int = wrapError $ spng_set_gama_int' ctx gama_int

spng_set_iccp
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngIccp -- ^ iccp
  -> IO ()
spng_set_iccp ctx iccp = wrapError $ spng_set_iccp' ctx iccp

spng_set_sbit
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngSbit -- ^ sbit
  -> IO ()
spng_set_sbit ctx sbit = wrapError $ spng_set_sbit' ctx sbit

spng_set_srgb
  :: Ptr SpngCtx     -- ^ ctx
  -> #{type uint8_t} -- ^ rendering_intent
  -> IO ()
spng_set_srgb ctx srgb = wrapError $ spng_set_srgb' ctx srgb

spng_set_text
  :: Ptr SpngCtx      -- ^ ctx
  -> Ptr SpngText     -- ^ text
  -> #{type uint32_t} -- ^ n_text
  -> IO ()
spng_set_text ctx text n = wrapError $ spng_set_text' ctx text n

spng_set_bkgd
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngBkgd -- ^ bkgd
  -> IO ()
spng_set_bkgd ctx bkgd = wrapError $ spng_set_bkgd' ctx bkgd

spng_set_hist
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngHist -- ^ hist
  -> IO ()
spng_set_hist ctx hist = wrapError $ spng_set_hist' ctx hist

spng_set_phys
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngPhys -- ^ phys
  -> IO ()
spng_set_phys ctx phys = wrapError $ spng_set_phys' ctx phys

spng_set_splt
  :: Ptr SpngCtx      -- ^ ctx
  -> Ptr SpngSplt     -- ^ splt
  -> #{type uint32_t} -- ^ n_splt
  -> IO ()
spng_set_splt ctx splt n = wrapError $ spng_set_splt' ctx splt n

spng_set_time
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngTime -- ^ time
  -> IO ()
spng_set_time ctx time = wrapError $ spng_set_time' ctx time

spng_set_unknown_chunks
  :: Ptr SpngCtx          -- ^ ctx
  -> Ptr SpngUnknownChunk -- ^ chunks
  -> #{type uint32_t}     -- ^ n_chunks
  -> IO ()
spng_set_unknown_chunks ctx chunks n = wrapError $ spng_set_unknown_chunks' ctx chunks n

spng_set_offs
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngOffs -- ^ offs
  -> IO ()
spng_set_offs ctx offs = wrapError $ spng_set_offs' ctx offs

spng_set_exif
  :: Ptr SpngCtx  -- ^ ctx
  -> Ptr SpngExif -- ^ exif
  -> IO ()
spng_set_exif ctx exif = wrapError $ spng_set_exif' ctx exif

{-# NOINLINE spng_version_string #-}
spng_version_string :: ByteString
spng_version_string = unsafePerformIO $ do ver <- spng_version_string'
                                           unsafePackCString $ castPtr ver
