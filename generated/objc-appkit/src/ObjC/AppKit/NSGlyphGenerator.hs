{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGlyphGenerator@.
module ObjC.AppKit.NSGlyphGenerator
  ( NSGlyphGenerator
  , IsNSGlyphGenerator(..)
  , generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex
  , sharedGlyphGenerator
  , generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndexSelector
  , sharedGlyphGeneratorSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:@
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex :: IsNSGlyphGenerator nsGlyphGenerator => nsGlyphGenerator -> RawId -> CULong -> Ptr CULong -> Ptr CULong -> IO ()
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex nsGlyphGenerator  glyphStorage nChars glyphIndex charIndex =
  sendMsg nsGlyphGenerator (mkSelector "generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:") retVoid [argPtr (castPtr (unRawId glyphStorage) :: Ptr ()), argCULong (fromIntegral nChars), argPtr glyphIndex, argPtr charIndex]

-- | @+ sharedGlyphGenerator@
sharedGlyphGenerator :: IO (Id NSGlyphGenerator)
sharedGlyphGenerator  =
  do
    cls' <- getRequiredClass "NSGlyphGenerator"
    sendClassMsg cls' (mkSelector "sharedGlyphGenerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:@
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndexSelector :: Selector
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndexSelector = mkSelector "generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:"

-- | @Selector@ for @sharedGlyphGenerator@
sharedGlyphGeneratorSelector :: Selector
sharedGlyphGeneratorSelector = mkSelector "sharedGlyphGenerator"

