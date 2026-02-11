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
  , stringSelector
  , setStringSelector
  , fontSelector
  , setFontSelector
  , fontSizeSelector
  , setFontSizeSelector
  , foregroundColorSelector
  , setForegroundColorSelector
  , wrappedSelector
  , setWrappedSelector
  , truncationModeSelector
  , setTruncationModeSelector
  , alignmentModeSelector
  , setAlignmentModeSelector
  , allowsFontSubpixelQuantizationSelector
  , setAllowsFontSubpixelQuantizationSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- string@
string :: IsCATextLayer caTextLayer => caTextLayer -> IO RawId
string caTextLayer  =
  fmap (RawId . castPtr) $ sendMsg caTextLayer (mkSelector "string") (retPtr retVoid) []

-- | @- setString:@
setString :: IsCATextLayer caTextLayer => caTextLayer -> RawId -> IO ()
setString caTextLayer  value =
  sendMsg caTextLayer (mkSelector "setString:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- font@
font :: IsCATextLayer caTextLayer => caTextLayer -> IO RawId
font caTextLayer  =
  fmap (RawId . castPtr) $ sendMsg caTextLayer (mkSelector "font") (retPtr retVoid) []

-- | @- setFont:@
setFont :: IsCATextLayer caTextLayer => caTextLayer -> RawId -> IO ()
setFont caTextLayer  value =
  sendMsg caTextLayer (mkSelector "setFont:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- fontSize@
fontSize :: IsCATextLayer caTextLayer => caTextLayer -> IO CDouble
fontSize caTextLayer  =
  sendMsg caTextLayer (mkSelector "fontSize") retCDouble []

-- | @- setFontSize:@
setFontSize :: IsCATextLayer caTextLayer => caTextLayer -> CDouble -> IO ()
setFontSize caTextLayer  value =
  sendMsg caTextLayer (mkSelector "setFontSize:") retVoid [argCDouble (fromIntegral value)]

-- | @- foregroundColor@
foregroundColor :: IsCATextLayer caTextLayer => caTextLayer -> IO (Ptr ())
foregroundColor caTextLayer  =
  fmap castPtr $ sendMsg caTextLayer (mkSelector "foregroundColor") (retPtr retVoid) []

-- | @- setForegroundColor:@
setForegroundColor :: IsCATextLayer caTextLayer => caTextLayer -> Ptr () -> IO ()
setForegroundColor caTextLayer  value =
  sendMsg caTextLayer (mkSelector "setForegroundColor:") retVoid [argPtr value]

-- | @- wrapped@
wrapped :: IsCATextLayer caTextLayer => caTextLayer -> IO Bool
wrapped caTextLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caTextLayer (mkSelector "wrapped") retCULong []

-- | @- setWrapped:@
setWrapped :: IsCATextLayer caTextLayer => caTextLayer -> Bool -> IO ()
setWrapped caTextLayer  value =
  sendMsg caTextLayer (mkSelector "setWrapped:") retVoid [argCULong (if value then 1 else 0)]

-- | @- truncationMode@
truncationMode :: IsCATextLayer caTextLayer => caTextLayer -> IO (Id NSString)
truncationMode caTextLayer  =
  sendMsg caTextLayer (mkSelector "truncationMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTruncationMode:@
setTruncationMode :: (IsCATextLayer caTextLayer, IsNSString value) => caTextLayer -> value -> IO ()
setTruncationMode caTextLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caTextLayer (mkSelector "setTruncationMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alignmentMode@
alignmentMode :: IsCATextLayer caTextLayer => caTextLayer -> IO (Id NSString)
alignmentMode caTextLayer  =
  sendMsg caTextLayer (mkSelector "alignmentMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlignmentMode:@
setAlignmentMode :: (IsCATextLayer caTextLayer, IsNSString value) => caTextLayer -> value -> IO ()
setAlignmentMode caTextLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caTextLayer (mkSelector "setAlignmentMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsFontSubpixelQuantization@
allowsFontSubpixelQuantization :: IsCATextLayer caTextLayer => caTextLayer -> IO Bool
allowsFontSubpixelQuantization caTextLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caTextLayer (mkSelector "allowsFontSubpixelQuantization") retCULong []

-- | @- setAllowsFontSubpixelQuantization:@
setAllowsFontSubpixelQuantization :: IsCATextLayer caTextLayer => caTextLayer -> Bool -> IO ()
setAllowsFontSubpixelQuantization caTextLayer  value =
  sendMsg caTextLayer (mkSelector "setAllowsFontSubpixelQuantization:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontSize@
fontSizeSelector :: Selector
fontSizeSelector = mkSelector "fontSize"

-- | @Selector@ for @setFontSize:@
setFontSizeSelector :: Selector
setFontSizeSelector = mkSelector "setFontSize:"

-- | @Selector@ for @foregroundColor@
foregroundColorSelector :: Selector
foregroundColorSelector = mkSelector "foregroundColor"

-- | @Selector@ for @setForegroundColor:@
setForegroundColorSelector :: Selector
setForegroundColorSelector = mkSelector "setForegroundColor:"

-- | @Selector@ for @wrapped@
wrappedSelector :: Selector
wrappedSelector = mkSelector "wrapped"

-- | @Selector@ for @setWrapped:@
setWrappedSelector :: Selector
setWrappedSelector = mkSelector "setWrapped:"

-- | @Selector@ for @truncationMode@
truncationModeSelector :: Selector
truncationModeSelector = mkSelector "truncationMode"

-- | @Selector@ for @setTruncationMode:@
setTruncationModeSelector :: Selector
setTruncationModeSelector = mkSelector "setTruncationMode:"

-- | @Selector@ for @alignmentMode@
alignmentModeSelector :: Selector
alignmentModeSelector = mkSelector "alignmentMode"

-- | @Selector@ for @setAlignmentMode:@
setAlignmentModeSelector :: Selector
setAlignmentModeSelector = mkSelector "setAlignmentMode:"

-- | @Selector@ for @allowsFontSubpixelQuantization@
allowsFontSubpixelQuantizationSelector :: Selector
allowsFontSubpixelQuantizationSelector = mkSelector "allowsFontSubpixelQuantization"

-- | @Selector@ for @setAllowsFontSubpixelQuantization:@
setAllowsFontSubpixelQuantizationSelector :: Selector
setAllowsFontSubpixelQuantizationSelector = mkSelector "setAllowsFontSubpixelQuantization:"

