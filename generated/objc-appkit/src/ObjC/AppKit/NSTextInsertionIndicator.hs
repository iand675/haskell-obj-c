{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextInsertionIndicator@.
module ObjC.AppKit.NSTextInsertionIndicator
  ( NSTextInsertionIndicator
  , IsNSTextInsertionIndicator(..)
  , displayMode
  , setDisplayMode
  , color
  , setColor
  , automaticModeOptions
  , setAutomaticModeOptions
  , effectsViewInserter
  , setEffectsViewInserter
  , displayModeSelector
  , setDisplayModeSelector
  , colorSelector
  , setColorSelector
  , automaticModeOptionsSelector
  , setAutomaticModeOptionsSelector
  , effectsViewInserterSelector
  , setEffectsViewInserterSelector

  -- * Enum types
  , NSTextInsertionIndicatorAutomaticModeOptions(NSTextInsertionIndicatorAutomaticModeOptions)
  , pattern NSTextInsertionIndicatorAutomaticModeOptionsShowEffectsView
  , pattern NSTextInsertionIndicatorAutomaticModeOptionsShowWhileTracking
  , NSTextInsertionIndicatorDisplayMode(NSTextInsertionIndicatorDisplayMode)
  , pattern NSTextInsertionIndicatorDisplayModeAutomatic
  , pattern NSTextInsertionIndicatorDisplayModeHidden
  , pattern NSTextInsertionIndicatorDisplayModeVisible

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Sets-returns the indicator's display mode.
--
-- ObjC selector: @- displayMode@
displayMode :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO NSTextInsertionIndicatorDisplayMode
displayMode nsTextInsertionIndicator  =
  fmap (coerce :: CLong -> NSTextInsertionIndicatorDisplayMode) $ sendMsg nsTextInsertionIndicator (mkSelector "displayMode") retCLong []

-- | Sets-returns the indicator's display mode.
--
-- ObjC selector: @- setDisplayMode:@
setDisplayMode :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> NSTextInsertionIndicatorDisplayMode -> IO ()
setDisplayMode nsTextInsertionIndicator  value =
  sendMsg nsTextInsertionIndicator (mkSelector "setDisplayMode:") retVoid [argCLong (coerce value)]

-- | The color of the indicator.
--
-- Defaults to NSColor.textInsertionPointColor.
--
-- Note: If set to @nil,@ uses NSColor.textInsertionPointColor.
--
-- ObjC selector: @- color@
color :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO (Id NSColor)
color nsTextInsertionIndicator  =
  sendMsg nsTextInsertionIndicator (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The color of the indicator.
--
-- Defaults to NSColor.textInsertionPointColor.
--
-- Note: If set to @nil,@ uses NSColor.textInsertionPointColor.
--
-- ObjC selector: @- setColor:@
setColor :: (IsNSTextInsertionIndicator nsTextInsertionIndicator, IsNSColor value) => nsTextInsertionIndicator -> value -> IO ()
setColor nsTextInsertionIndicator  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextInsertionIndicator (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Options for the NSTextInsertionIndicatorDisplayModeAutomatic display mode. Defaults to NSTextInsertionIndicatorAutomaticModeOptionsShowEffectsView.
--
-- ObjC selector: @- automaticModeOptions@
automaticModeOptions :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO NSTextInsertionIndicatorAutomaticModeOptions
automaticModeOptions nsTextInsertionIndicator  =
  fmap (coerce :: CLong -> NSTextInsertionIndicatorAutomaticModeOptions) $ sendMsg nsTextInsertionIndicator (mkSelector "automaticModeOptions") retCLong []

-- | Options for the NSTextInsertionIndicatorDisplayModeAutomatic display mode. Defaults to NSTextInsertionIndicatorAutomaticModeOptionsShowEffectsView.
--
-- ObjC selector: @- setAutomaticModeOptions:@
setAutomaticModeOptions :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> NSTextInsertionIndicatorAutomaticModeOptions -> IO ()
setAutomaticModeOptions nsTextInsertionIndicator  value =
  sendMsg nsTextInsertionIndicator (mkSelector "setAutomaticModeOptions:") retVoid [argCLong (coerce value)]

-- | Sets-returns a block that inserts a view into the view hierarchy.
--
-- During dictation the NSTextInsertionIndicator displays a glow effect by inserting a view below the text view. If an application needs to insert the view in a different way, the application can specify a block of code that will be called when the glow effect needs to be displayed.
--
-- ObjC selector: @- effectsViewInserter@
effectsViewInserter :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO (Ptr ())
effectsViewInserter nsTextInsertionIndicator  =
  fmap castPtr $ sendMsg nsTextInsertionIndicator (mkSelector "effectsViewInserter") (retPtr retVoid) []

-- | Sets-returns a block that inserts a view into the view hierarchy.
--
-- During dictation the NSTextInsertionIndicator displays a glow effect by inserting a view below the text view. If an application needs to insert the view in a different way, the application can specify a block of code that will be called when the glow effect needs to be displayed.
--
-- ObjC selector: @- setEffectsViewInserter:@
setEffectsViewInserter :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> Ptr () -> IO ()
setEffectsViewInserter nsTextInsertionIndicator  value =
  sendMsg nsTextInsertionIndicator (mkSelector "setEffectsViewInserter:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayMode@
displayModeSelector :: Selector
displayModeSelector = mkSelector "displayMode"

-- | @Selector@ for @setDisplayMode:@
setDisplayModeSelector :: Selector
setDisplayModeSelector = mkSelector "setDisplayMode:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @automaticModeOptions@
automaticModeOptionsSelector :: Selector
automaticModeOptionsSelector = mkSelector "automaticModeOptions"

-- | @Selector@ for @setAutomaticModeOptions:@
setAutomaticModeOptionsSelector :: Selector
setAutomaticModeOptionsSelector = mkSelector "setAutomaticModeOptions:"

-- | @Selector@ for @effectsViewInserter@
effectsViewInserterSelector :: Selector
effectsViewInserterSelector = mkSelector "effectsViewInserter"

-- | @Selector@ for @setEffectsViewInserter:@
setEffectsViewInserterSelector :: Selector
setEffectsViewInserterSelector = mkSelector "setEffectsViewInserter:"

