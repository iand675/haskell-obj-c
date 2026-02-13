{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CATextLayer@.
module ObjC.QuartzCore.CATextLayer
  ( CATextLayer
  , IsCATextLayer(..)
  , string
  , setString
  , font
  , setFont
  , fontSize
  , setFontSize
  , foregroundColor
  , setForegroundColor
  , wrapped
  , setWrapped
  , truncationMode
  , setTruncationMode
  , alignmentMode
  , setAlignmentMode
  , allowsFontSubpixelQuantization
  , setAllowsFontSubpixelQuantization
  , alignmentModeSelector
  , allowsFontSubpixelQuantizationSelector
  , fontSelector
  , fontSizeSelector
  , foregroundColorSelector
  , setAlignmentModeSelector
  , setAllowsFontSubpixelQuantizationSelector
  , setFontSelector
  , setFontSizeSelector
  , setForegroundColorSelector
  , setStringSelector
  , setTruncationModeSelector
  , setWrappedSelector
  , stringSelector
  , truncationModeSelector
  , wrappedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- string@
string :: IsCATextLayer caTextLayer => caTextLayer -> IO RawId
string caTextLayer =
  sendMessage caTextLayer stringSelector

-- | @- setString:@
setString :: IsCATextLayer caTextLayer => caTextLayer -> RawId -> IO ()
setString caTextLayer value =
  sendMessage caTextLayer setStringSelector value

-- | @- font@
font :: IsCATextLayer caTextLayer => caTextLayer -> IO RawId
font caTextLayer =
  sendMessage caTextLayer fontSelector

-- | @- setFont:@
setFont :: IsCATextLayer caTextLayer => caTextLayer -> RawId -> IO ()
setFont caTextLayer value =
  sendMessage caTextLayer setFontSelector value

-- | @- fontSize@
fontSize :: IsCATextLayer caTextLayer => caTextLayer -> IO CDouble
fontSize caTextLayer =
  sendMessage caTextLayer fontSizeSelector

-- | @- setFontSize:@
setFontSize :: IsCATextLayer caTextLayer => caTextLayer -> CDouble -> IO ()
setFontSize caTextLayer value =
  sendMessage caTextLayer setFontSizeSelector value

-- | @- foregroundColor@
foregroundColor :: IsCATextLayer caTextLayer => caTextLayer -> IO (Ptr ())
foregroundColor caTextLayer =
  sendMessage caTextLayer foregroundColorSelector

-- | @- setForegroundColor:@
setForegroundColor :: IsCATextLayer caTextLayer => caTextLayer -> Ptr () -> IO ()
setForegroundColor caTextLayer value =
  sendMessage caTextLayer setForegroundColorSelector value

-- | @- wrapped@
wrapped :: IsCATextLayer caTextLayer => caTextLayer -> IO Bool
wrapped caTextLayer =
  sendMessage caTextLayer wrappedSelector

-- | @- setWrapped:@
setWrapped :: IsCATextLayer caTextLayer => caTextLayer -> Bool -> IO ()
setWrapped caTextLayer value =
  sendMessage caTextLayer setWrappedSelector value

-- | @- truncationMode@
truncationMode :: IsCATextLayer caTextLayer => caTextLayer -> IO (Id NSString)
truncationMode caTextLayer =
  sendMessage caTextLayer truncationModeSelector

-- | @- setTruncationMode:@
setTruncationMode :: (IsCATextLayer caTextLayer, IsNSString value) => caTextLayer -> value -> IO ()
setTruncationMode caTextLayer value =
  sendMessage caTextLayer setTruncationModeSelector (toNSString value)

-- | @- alignmentMode@
alignmentMode :: IsCATextLayer caTextLayer => caTextLayer -> IO (Id NSString)
alignmentMode caTextLayer =
  sendMessage caTextLayer alignmentModeSelector

-- | @- setAlignmentMode:@
setAlignmentMode :: (IsCATextLayer caTextLayer, IsNSString value) => caTextLayer -> value -> IO ()
setAlignmentMode caTextLayer value =
  sendMessage caTextLayer setAlignmentModeSelector (toNSString value)

-- | @- allowsFontSubpixelQuantization@
allowsFontSubpixelQuantization :: IsCATextLayer caTextLayer => caTextLayer -> IO Bool
allowsFontSubpixelQuantization caTextLayer =
  sendMessage caTextLayer allowsFontSubpixelQuantizationSelector

-- | @- setAllowsFontSubpixelQuantization:@
setAllowsFontSubpixelQuantization :: IsCATextLayer caTextLayer => caTextLayer -> Bool -> IO ()
setAllowsFontSubpixelQuantization caTextLayer value =
  sendMessage caTextLayer setAllowsFontSubpixelQuantizationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @string@
stringSelector :: Selector '[] RawId
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[RawId] ()
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] RawId
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[RawId] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontSize@
fontSizeSelector :: Selector '[] CDouble
fontSizeSelector = mkSelector "fontSize"

-- | @Selector@ for @setFontSize:@
setFontSizeSelector :: Selector '[CDouble] ()
setFontSizeSelector = mkSelector "setFontSize:"

-- | @Selector@ for @foregroundColor@
foregroundColorSelector :: Selector '[] (Ptr ())
foregroundColorSelector = mkSelector "foregroundColor"

-- | @Selector@ for @setForegroundColor:@
setForegroundColorSelector :: Selector '[Ptr ()] ()
setForegroundColorSelector = mkSelector "setForegroundColor:"

-- | @Selector@ for @wrapped@
wrappedSelector :: Selector '[] Bool
wrappedSelector = mkSelector "wrapped"

-- | @Selector@ for @setWrapped:@
setWrappedSelector :: Selector '[Bool] ()
setWrappedSelector = mkSelector "setWrapped:"

-- | @Selector@ for @truncationMode@
truncationModeSelector :: Selector '[] (Id NSString)
truncationModeSelector = mkSelector "truncationMode"

-- | @Selector@ for @setTruncationMode:@
setTruncationModeSelector :: Selector '[Id NSString] ()
setTruncationModeSelector = mkSelector "setTruncationMode:"

-- | @Selector@ for @alignmentMode@
alignmentModeSelector :: Selector '[] (Id NSString)
alignmentModeSelector = mkSelector "alignmentMode"

-- | @Selector@ for @setAlignmentMode:@
setAlignmentModeSelector :: Selector '[Id NSString] ()
setAlignmentModeSelector = mkSelector "setAlignmentMode:"

-- | @Selector@ for @allowsFontSubpixelQuantization@
allowsFontSubpixelQuantizationSelector :: Selector '[] Bool
allowsFontSubpixelQuantizationSelector = mkSelector "allowsFontSubpixelQuantization"

-- | @Selector@ for @setAllowsFontSubpixelQuantization:@
setAllowsFontSubpixelQuantizationSelector :: Selector '[Bool] ()
setAllowsFontSubpixelQuantizationSelector = mkSelector "setAllowsFontSubpixelQuantization:"

