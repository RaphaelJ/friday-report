{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables #-}

import Prelude hiding (map)
import System.Environment (getArgs)

import Vision.Image (
     Grey, GreyDelayed, RGB, InterpolMethod (Bilinear)
    , convert, compute, computeP, delayed, shape, resize
    )
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (Z (..), (:.) (..), ix2)

-- Reads an image from a file, applies a composition of transformations to
-- create a centred and squared miniature and then writes the result to a file:
--
-- usage: ./delayed input.png output.png
main :: IO ()
main = do
    [inputPath, outputPath] <- getArgs

    -- Loads the image. Automatically infers the format.
    io <- load Autodetect inputPath

    case io of
        Left err           -> do
            putStrLn "Unable to load the image:"
            print err
        Right (input :: RGB) -> do
            let -- Gets the size of the image.
                Z :. h :. w = shape input

                -- Gets a grey scale delayed image from the input image.
                grey :: GreyDelayed
                grey    = delayed (convert input)

                resized = delayed (resize Bilinear (ix2 (h * 2) (w * 2)) grey)

--             let output = compute resized :: Grey
            output <- computeP resized :: IO Grey

            mErr <- save Autodetect outputPath output
            case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err

