{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:@
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex :: IsNSGlyphGenerator nsGlyphGenerator => nsGlyphGenerator -> RawId -> CULong -> Ptr CULong -> Ptr CULong -> IO ()
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex nsGlyphGenerator glyphStorage nChars glyphIndex charIndex =
  sendMessage nsGlyphGenerator generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndexSelector glyphStorage nChars glyphIndex charIndex

-- | @+ sharedGlyphGenerator@
sharedGlyphGenerator :: IO (Id NSGlyphGenerator)
sharedGlyphGenerator  =
  do
    cls' <- getRequiredClass "NSGlyphGenerator"
    sendClassMessage cls' sharedGlyphGeneratorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:@
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndexSelector :: Selector '[RawId, CULong, Ptr CULong, Ptr CULong] ()
generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndexSelector = mkSelector "generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:"

-- | @Selector@ for @sharedGlyphGenerator@
sharedGlyphGeneratorSelector :: Selector '[] (Id NSGlyphGenerator)
sharedGlyphGeneratorSelector = mkSelector "sharedGlyphGenerator"

