{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables #-}

import Prelude hiding (map)
import System.Environment (getArgs)

import Vision.Image (
     RGB, InterpolMethod (Bilinear)
    , compute, computeP, crop, delayed, shape, resize
    )
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (Z (..), (:.) (..), Rect (..), ix2)

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

                -- Creates a Rect object which will be used to define how we
                -- will crop our image. The rectangle is centered on the largest
                -- side of the image.
                rect | w > h     = Rect ((w - h) `quot` 2) 0 h h
                     | otherwise = Rect 0 ((h - w) `quot` 2) w w

                -- Crops the image. Doesn't compute the image into a "real"
                -- image: by using a delayed representation, this intermediate
                -- image will not exist in the computer memory as a large array.
                cropped = delayed (crop rect input)

                -- Resizes the image. By using the delayed representation of the
                -- cropped image, our compiler should be able to fuse these two
                -- transformations into a single loop.
                resized = delayed (resize Bilinear (ix2 1000 1000) cropped)

--             let output = compute resized :: RGB
            output <- computeP resized :: IO RGB

            mErr <- save Autodetect outputPath output
            case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err
