{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTintConfiguration@.
module ObjC.AppKit.NSTintConfiguration
  ( NSTintConfiguration
  , IsNSTintConfiguration(..)
  , tintConfigurationWithPreferredColor
  , tintConfigurationWithFixedColor
  , defaultTintConfiguration
  , monochromeTintConfiguration
  , baseTintColor
  , equivalentContentTintColor
  , adaptsToUserAccentColor
  , adaptsToUserAccentColorSelector
  , baseTintColorSelector
  , defaultTintConfigurationSelector
  , equivalentContentTintColorSelector
  , monochromeTintConfigurationSelector
  , tintConfigurationWithFixedColorSelector
  , tintConfigurationWithPreferredColorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies that content should be tinted with a particular color whenever the app's preferred Accent Color is in use, i.e. when the system Accent Color is configured to "Multicolor". If the system Accent Color is configured to any other color, this tint configuration defers to the Accent Color.
--
-- This type of configuration should be used for custom colors that are designed to match an app-specific Accent Color, but would mismatch a user-selected color.
--
-- ObjC selector: @+ tintConfigurationWithPreferredColor:@
tintConfigurationWithPreferredColor :: IsNSColor color => color -> IO (Id NSTintConfiguration)
tintConfigurationWithPreferredColor color =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    sendClassMessage cls' tintConfigurationWithPreferredColorSelector (toNSColor color)

-- | Specifies that content should be tinted with a specific color value. The specified color value is used regardless of the system Accent Color.
--
-- ObjC selector: @+ tintConfigurationWithFixedColor:@
tintConfigurationWithFixedColor :: IsNSColor color => color -> IO (Id NSTintConfiguration)
tintConfigurationWithFixedColor color =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    sendClassMessage cls' tintConfigurationWithFixedColorSelector (toNSColor color)

-- | Specifies that content should be tinted using the system default for its context. For example, a source list icon's default tint matches the active Accent Color.
--
-- ObjC selector: @+ defaultTintConfiguration@
defaultTintConfiguration :: IO (Id NSTintConfiguration)
defaultTintConfiguration  =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    sendClassMessage cls' defaultTintConfigurationSelector

-- | Specifies that content should prefer a monochrome appearance. Monochrome content remains monochrome regardless of the system Accent Color.
--
-- ObjC selector: @+ monochromeTintConfiguration@
monochromeTintConfiguration :: IO (Id NSTintConfiguration)
monochromeTintConfiguration  =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    sendClassMessage cls' monochromeTintConfigurationSelector

-- | The base NSColor supplied when creating the tint configuration object. If the receiver wasn't created using a base NSColor, this property returns nil.
--
-- ObjC selector: @- baseTintColor@
baseTintColor :: IsNSTintConfiguration nsTintConfiguration => nsTintConfiguration -> IO (Id NSColor)
baseTintColor nsTintConfiguration =
  sendMessage nsTintConfiguration baseTintColorSelector

-- | An equivalent NSColor matching the effective content tint of the receiver. If the receiver can't be represented as a NSColor, this property returns nil.
--
-- ObjC selector: @- equivalentContentTintColor@
equivalentContentTintColor :: IsNSTintConfiguration nsTintConfiguration => nsTintConfiguration -> IO (Id NSColor)
equivalentContentTintColor nsTintConfiguration =
  sendMessage nsTintConfiguration equivalentContentTintColorSelector

-- | If YES, the tint configuration alters its effect based on the user's preferred Accent Color. Otherwise, the tint configuration produces a constant effect regardless of the Accent Color preference.
--
-- ObjC selector: @- adaptsToUserAccentColor@
adaptsToUserAccentColor :: IsNSTintConfiguration nsTintConfiguration => nsTintConfiguration -> IO Bool
adaptsToUserAccentColor nsTintConfiguration =
  sendMessage nsTintConfiguration adaptsToUserAccentColorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tintConfigurationWithPreferredColor:@
tintConfigurationWithPreferredColorSelector :: Selector '[Id NSColor] (Id NSTintConfiguration)
tintConfigurationWithPreferredColorSelector = mkSelector "tintConfigurationWithPreferredColor:"

-- | @Selector@ for @tintConfigurationWithFixedColor:@
tintConfigurationWithFixedColorSelector :: Selector '[Id NSColor] (Id NSTintConfiguration)
tintConfigurationWithFixedColorSelector = mkSelector "tintConfigurationWithFixedColor:"

-- | @Selector@ for @defaultTintConfiguration@
defaultTintConfigurationSelector :: Selector '[] (Id NSTintConfiguration)
defaultTintConfigurationSelector = mkSelector "defaultTintConfiguration"

-- | @Selector@ for @monochromeTintConfiguration@
monochromeTintConfigurationSelector :: Selector '[] (Id NSTintConfiguration)
monochromeTintConfigurationSelector = mkSelector "monochromeTintConfiguration"

-- | @Selector@ for @baseTintColor@
baseTintColorSelector :: Selector '[] (Id NSColor)
baseTintColorSelector = mkSelector "baseTintColor"

-- | @Selector@ for @equivalentContentTintColor@
equivalentContentTintColorSelector :: Selector '[] (Id NSColor)
equivalentContentTintColorSelector = mkSelector "equivalentContentTintColor"

-- | @Selector@ for @adaptsToUserAccentColor@
adaptsToUserAccentColorSelector :: Selector '[] Bool
adaptsToUserAccentColorSelector = mkSelector "adaptsToUserAccentColor"

