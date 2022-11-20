module Main where

import           Bind

import           Libspng


main :: IO ()
main
  | () == ()  = return ()
  | otherwise = do

      _ <- bind spng_ctx_new
      _ <- bind spng_ctx_new2
      _ <- bind spng_ctx_free
      _ <- bind spng_set_png_buffer
      _ <- bind spng_set_png_stream
      _ <- bind spng_set_png_file
      _ <- bind spng_get_png_buffer
      _ <- bind spng_set_image_limits
      _ <- bind spng_get_image_limits
      _ <- bind spng_set_chunk_limits
      _ <- bind spng_get_chunk_limits
      _ <- bind spng_set_crc_action
      _ <- bind spng_set_option
      _ <- bind spng_get_option
      _ <- bind spng_decoded_image_size
      _ <- bind spng_decode_image
      _ <- bind spng_decode_scanline
      _ <- bind spng_decode_row
      _ <- bind spng_decode_chunks
      _ <- bind spng_get_row_info
      _ <- bind spng_encode_image
      _ <- bind spng_encode_scanline
      _ <- bind spng_encode_row
      _ <- bind spng_encode_chunks
      _ <- bind spng_get_ihdr
      _ <- bind spng_get_plte
      _ <- bind spng_get_trns
      _ <- bind spng_get_chrm
      _ <- bind spng_get_chrm_int
      _ <- bind spng_get_gama
      _ <- bind spng_get_gama_int
      _ <- bind spng_get_iccp
      _ <- bind spng_get_sbit
      _ <- bind spng_get_srgb
      _ <- bind spng_get_text
      _ <- bind spng_get_bkgd
      _ <- bind spng_get_hist
      _ <- bind spng_get_phys
      _ <- bind spng_get_splt
      _ <- bind spng_get_time
      _ <- bind spng_get_unknown_chunks
      _ <- bind spng_get_offs
      _ <- bind spng_get_exif
      _ <- bind spng_set_ihdr
      _ <- bind spng_set_plte
      _ <- bind spng_set_trns
      _ <- bind spng_set_chrm
      _ <- bind spng_set_chrm_int
      _ <- bind spng_set_gama
      _ <- bind spng_set_gama_int
      _ <- bind spng_set_iccp
      _ <- bind spng_set_sbit
      _ <- bind spng_set_srgb
      _ <- bind spng_set_text
      _ <- bind spng_set_bkgd
      _ <- bind spng_set_hist
      _ <- bind spng_set_phys
      _ <- bind spng_set_splt
      _ <- bind spng_set_time
      _ <- bind spng_set_unknown_chunks
      _ <- bind spng_set_offs
      _ <- bind spng_set_exif
      _ <- bind spng_strerror
      _ <- bind spng_version_string

      return ()
