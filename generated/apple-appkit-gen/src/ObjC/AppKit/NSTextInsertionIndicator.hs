{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , automaticModeOptionsSelector
  , colorSelector
  , displayModeSelector
  , effectsViewInserterSelector
  , setAutomaticModeOptionsSelector
  , setColorSelector
  , setDisplayModeSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Sets-returns the indicator's display mode.
--
-- ObjC selector: @- displayMode@
displayMode :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO NSTextInsertionIndicatorDisplayMode
displayMode nsTextInsertionIndicator =
  sendMessage nsTextInsertionIndicator displayModeSelector

-- | Sets-returns the indicator's display mode.
--
-- ObjC selector: @- setDisplayMode:@
setDisplayMode :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> NSTextInsertionIndicatorDisplayMode -> IO ()
setDisplayMode nsTextInsertionIndicator value =
  sendMessage nsTextInsertionIndicator setDisplayModeSelector value

-- | The color of the indicator.
--
-- Defaults to NSColor.textInsertionPointColor.
--
-- Note: If set to @nil,@ uses NSColor.textInsertionPointColor.
--
-- ObjC selector: @- color@
color :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO (Id NSColor)
color nsTextInsertionIndicator =
  sendMessage nsTextInsertionIndicator colorSelector

-- | The color of the indicator.
--
-- Defaults to NSColor.textInsertionPointColor.
--
-- Note: If set to @nil,@ uses NSColor.textInsertionPointColor.
--
-- ObjC selector: @- setColor:@
setColor :: (IsNSTextInsertionIndicator nsTextInsertionIndicator, IsNSColor value) => nsTextInsertionIndicator -> value -> IO ()
setColor nsTextInsertionIndicator value =
  sendMessage nsTextInsertionIndicator setColorSelector (toNSColor value)

-- | Options for the NSTextInsertionIndicatorDisplayModeAutomatic display mode. Defaults to NSTextInsertionIndicatorAutomaticModeOptionsShowEffectsView.
--
-- ObjC selector: @- automaticModeOptions@
automaticModeOptions :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO NSTextInsertionIndicatorAutomaticModeOptions
automaticModeOptions nsTextInsertionIndicator =
  sendMessage nsTextInsertionIndicator automaticModeOptionsSelector

-- | Options for the NSTextInsertionIndicatorDisplayModeAutomatic display mode. Defaults to NSTextInsertionIndicatorAutomaticModeOptionsShowEffectsView.
--
-- ObjC selector: @- setAutomaticModeOptions:@
setAutomaticModeOptions :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> NSTextInsertionIndicatorAutomaticModeOptions -> IO ()
setAutomaticModeOptions nsTextInsertionIndicator value =
  sendMessage nsTextInsertionIndicator setAutomaticModeOptionsSelector value

-- | Sets-returns a block that inserts a view into the view hierarchy.
--
-- During dictation the NSTextInsertionIndicator displays a glow effect by inserting a view below the text view. If an application needs to insert the view in a different way, the application can specify a block of code that will be called when the glow effect needs to be displayed.
--
-- ObjC selector: @- effectsViewInserter@
effectsViewInserter :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> IO (Ptr ())
effectsViewInserter nsTextInsertionIndicator =
  sendMessage nsTextInsertionIndicator effectsViewInserterSelector

-- | Sets-returns a block that inserts a view into the view hierarchy.
--
-- During dictation the NSTextInsertionIndicator displays a glow effect by inserting a view below the text view. If an application needs to insert the view in a different way, the application can specify a block of code that will be called when the glow effect needs to be displayed.
--
-- ObjC selector: @- setEffectsViewInserter:@
setEffectsViewInserter :: IsNSTextInsertionIndicator nsTextInsertionIndicator => nsTextInsertionIndicator -> Ptr () -> IO ()
setEffectsViewInserter nsTextInsertionIndicator value =
  sendMessage nsTextInsertionIndicator setEffectsViewInserterSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayMode@
displayModeSelector :: Selector '[] NSTextInsertionIndicatorDisplayMode
displayModeSelector = mkSelector "displayMode"

-- | @Selector@ for @setDisplayMode:@
setDisplayModeSelector :: Selector '[NSTextInsertionIndicatorDisplayMode] ()
setDisplayModeSelector = mkSelector "setDisplayMode:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @automaticModeOptions@
automaticModeOptionsSelector :: Selector '[] NSTextInsertionIndicatorAutomaticModeOptions
automaticModeOptionsSelector = mkSelector "automaticModeOptions"

-- | @Selector@ for @setAutomaticModeOptions:@
setAutomaticModeOptionsSelector :: Selector '[NSTextInsertionIndicatorAutomaticModeOptions] ()
setAutomaticModeOptionsSelector = mkSelector "setAutomaticModeOptions:"

-- | @Selector@ for @effectsViewInserter@
effectsViewInserterSelector :: Selector '[] (Ptr ())
effectsViewInserterSelector = mkSelector "effectsViewInserter"

-- | @Selector@ for @setEffectsViewInserter:@
setEffectsViewInserterSelector :: Selector '[Ptr ()] ()
setEffectsViewInserterSelector = mkSelector "setEffectsViewInserter:"

