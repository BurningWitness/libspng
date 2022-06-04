{-# LANGUAGE DataKinds
           , Rank2Types
           , ScopedTypeVariables
           , TypeApplications #-}

module Main where

import           Codec.Image.PNG.Simple

import           Control.Exception
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.List (intercalate)
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset
import           Options.Applicative
import           System.Console.ANSI
import           System.IO



data Opts =
       Opts
         { oMemory :: Bool
         , oPath   :: FilePath
         }

opts :: Parser Opts
opts = Opts
         <$> do switch $ mconcat
                           [ long "memory"
                           , short 'm'
                           , help "Instead of reading from the file directly, \
                                  \dump it into memory first"
                           ]
         <*> strArgument ( metavar "FILE" <> help "PNG file" )



data Vec4 a = Vec4 a a a a

instance Storable a => Storable (Vec4 a) where
  sizeOf    _ = sizeOf    (undefined :: a)
  alignment _ = alignment (undefined :: a)

  peek ptr = Vec4
               <$> peek (castPtr ptr)
               <*> peekElemOff (castPtr ptr) 1
               <*> peekElemOff (castPtr ptr) 2
               <*> peekElemOff (castPtr ptr) 3

  poke ptr (Vec4 a b c d) = do
    poke (castPtr ptr) a
    pokeElemOff (castPtr ptr) 1 b
    pokeElemOff (castPtr ptr) 2 c
    pokeElemOff (castPtr ptr) 3 d

draw :: Vec4 Word8 -> String
draw (Vec4 r g b a) =
  let colour = blend (fromIntegral a / 256) (sRGB24 r g b) white
  in setSGRCode [ SetRGBColor Foreground colour ] <> "█"



-- | Direct file read. File is kept open for the entire duration.
direct :: Ptr SpngCtx -> FilePath -> IO a -> IO a
direct ctx path io = do
  bracket (openBinaryFile path ReadMode) hClose $ \file -> do
    readFn <- mkSpngReadFn $ hReadFn file
    spng_set_png_stream ctx readFn nullPtr
    io

-- | File read from memory. File is first copied to memory, then processed.
memory :: Ptr SpngCtx -> FilePath -> IO a -> IO a
memory ctx path io = do
  (fptr, len) <- bracket (openBinaryFile path ReadMode) hClose $ \file -> do
                   filelen <- hFileSize file
                   fptr <- mallocForeignPtrBytes $ fromIntegral filelen
                   _ <- withForeignPtr fptr $ \ptr ->
                          hGetBuf file ptr (fromIntegral filelen)
                   return (fptr, fromIntegral filelen)

  withForeignPtr fptr $ \ptr -> do
    let memRead = mkMemRead ptr len
    alloca $ \memptr -> do
      poke memptr memRead
      readFn <- mkSpngReadFn $ memReadFn memptr
      spng_set_png_stream ctx readFn nullPtr
  io



core :: (forall a. Ptr SpngCtx -> FilePath -> IO a -> IO a) -> FilePath -> IO ()
core mode path = do
  (fptr, x, y) <- spng_ctx_with mempty $ \ctx ->
                    mode ctx path $ do
                      len <- spng_decoded_image_size ctx SPNG_FMT_RGBA8
                      (x, y) <- alloca $ \ihdr -> do
                                  spng_get_ihdr ctx ihdr
                                  (,) <$> peek (offset @"siWidth" ihdr)
                                      <*> peek (offset @"siHeight" ihdr)
                      fptr <- mallocForeignPtrBytes $ fromIntegral len
                      withForeignPtr fptr $ \ptr -> do
                        spng_decode_image ctx ptr len SPNG_FMT_RGBA8 mempty
                      return (fptr, fromIntegral x, fromIntegral y :: Int)

  withForeignPtr fptr $ \ptr -> do
    let offs = flip fmap [0 .. y - 1] $ \h ->
                 flip fmap [0 .. x - 1] $ \w ->
                   (w + h * x) * 4

    parts <- flip traverse offs $
                    traverse $ \n -> do
                      el <- peek $ plusPtr ptr n
                      return $ draw el

    putStrLn $ (intercalate "\n" $ mconcat <$> parts) <> setSGRCode [Reset]



main :: IO ()
main = do
  Opts mem path <- execParser . info (opts <**> helper) $
                                  mconcat
                                    [ fullDesc
                                    , progDesc "Displays the PNG file on stdout"
                                    ]
  core (if mem then memory else direct) path
