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
  , tintConfigurationWithPreferredColorSelector
  , tintConfigurationWithFixedColorSelector
  , defaultTintConfigurationSelector
  , monochromeTintConfigurationSelector
  , baseTintColorSelector
  , equivalentContentTintColorSelector
  , adaptsToUserAccentColorSelector


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

-- | Specifies that content should be tinted with a particular color whenever the app's preferred Accent Color is in use, i.e. when the system Accent Color is configured to "Multicolor". If the system Accent Color is configured to any other color, this tint configuration defers to the Accent Color.
--
-- This type of configuration should be used for custom colors that are designed to match an app-specific Accent Color, but would mismatch a user-selected color.
--
-- ObjC selector: @+ tintConfigurationWithPreferredColor:@
tintConfigurationWithPreferredColor :: IsNSColor color => color -> IO (Id NSTintConfiguration)
tintConfigurationWithPreferredColor color =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    withObjCPtr color $ \raw_color ->
      sendClassMsg cls' (mkSelector "tintConfigurationWithPreferredColor:") (retPtr retVoid) [argPtr (castPtr raw_color :: Ptr ())] >>= retainedObject . castPtr

-- | Specifies that content should be tinted with a specific color value. The specified color value is used regardless of the system Accent Color.
--
-- ObjC selector: @+ tintConfigurationWithFixedColor:@
tintConfigurationWithFixedColor :: IsNSColor color => color -> IO (Id NSTintConfiguration)
tintConfigurationWithFixedColor color =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    withObjCPtr color $ \raw_color ->
      sendClassMsg cls' (mkSelector "tintConfigurationWithFixedColor:") (retPtr retVoid) [argPtr (castPtr raw_color :: Ptr ())] >>= retainedObject . castPtr

-- | Specifies that content should be tinted using the system default for its context. For example, a source list icon's default tint matches the active Accent Color.
--
-- ObjC selector: @+ defaultTintConfiguration@
defaultTintConfiguration :: IO (Id NSTintConfiguration)
defaultTintConfiguration  =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    sendClassMsg cls' (mkSelector "defaultTintConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies that content should prefer a monochrome appearance. Monochrome content remains monochrome regardless of the system Accent Color.
--
-- ObjC selector: @+ monochromeTintConfiguration@
monochromeTintConfiguration :: IO (Id NSTintConfiguration)
monochromeTintConfiguration  =
  do
    cls' <- getRequiredClass "NSTintConfiguration"
    sendClassMsg cls' (mkSelector "monochromeTintConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The base NSColor supplied when creating the tint configuration object. If the receiver wasn't created using a base NSColor, this property returns nil.
--
-- ObjC selector: @- baseTintColor@
baseTintColor :: IsNSTintConfiguration nsTintConfiguration => nsTintConfiguration -> IO (Id NSColor)
baseTintColor nsTintConfiguration  =
  sendMsg nsTintConfiguration (mkSelector "baseTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An equivalent NSColor matching the effective content tint of the receiver. If the receiver can't be represented as a NSColor, this property returns nil.
--
-- ObjC selector: @- equivalentContentTintColor@
equivalentContentTintColor :: IsNSTintConfiguration nsTintConfiguration => nsTintConfiguration -> IO (Id NSColor)
equivalentContentTintColor nsTintConfiguration  =
  sendMsg nsTintConfiguration (mkSelector "equivalentContentTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If YES, the tint configuration alters its effect based on the user's preferred Accent Color. Otherwise, the tint configuration produces a constant effect regardless of the Accent Color preference.
--
-- ObjC selector: @- adaptsToUserAccentColor@
adaptsToUserAccentColor :: IsNSTintConfiguration nsTintConfiguration => nsTintConfiguration -> IO Bool
adaptsToUserAccentColor nsTintConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTintConfiguration (mkSelector "adaptsToUserAccentColor") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tintConfigurationWithPreferredColor:@
tintConfigurationWithPreferredColorSelector :: Selector
tintConfigurationWithPreferredColorSelector = mkSelector "tintConfigurationWithPreferredColor:"

-- | @Selector@ for @tintConfigurationWithFixedColor:@
tintConfigurationWithFixedColorSelector :: Selector
tintConfigurationWithFixedColorSelector = mkSelector "tintConfigurationWithFixedColor:"

-- | @Selector@ for @defaultTintConfiguration@
defaultTintConfigurationSelector :: Selector
defaultTintConfigurationSelector = mkSelector "defaultTintConfiguration"

-- | @Selector@ for @monochromeTintConfiguration@
monochromeTintConfigurationSelector :: Selector
monochromeTintConfigurationSelector = mkSelector "monochromeTintConfiguration"

-- | @Selector@ for @baseTintColor@
baseTintColorSelector :: Selector
baseTintColorSelector = mkSelector "baseTintColor"

-- | @Selector@ for @equivalentContentTintColor@
equivalentContentTintColorSelector :: Selector
equivalentContentTintColorSelector = mkSelector "equivalentContentTintColor"

-- | @Selector@ for @adaptsToUserAccentColor@
adaptsToUserAccentColorSelector :: Selector
adaptsToUserAccentColorSelector = mkSelector "adaptsToUserAccentColor"

