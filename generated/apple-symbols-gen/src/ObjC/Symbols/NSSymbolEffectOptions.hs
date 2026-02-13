{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Options configuring how symbol effects apply to symbol views.
--
-- Generated bindings for @NSSymbolEffectOptions@.
module ObjC.Symbols.NSSymbolEffectOptions
  ( NSSymbolEffectOptions
  , IsNSSymbolEffectOptions(..)
  , new
  , init_
  , options
  , nsSymbolEffectOptionsOptionsWithRepeating
  , optionsWithRepeating
  , nsSymbolEffectOptionsOptionsWithNonRepeating
  , optionsWithNonRepeating
  , nsSymbolEffectOptionsOptionsWithRepeatCount
  , optionsWithRepeatCount
  , nsSymbolEffectOptionsOptionsWithSpeed
  , optionsWithSpeed
  , nsSymbolEffectOptionsOptionsWithRepeatBehavior
  , optionsWithRepeatBehavior
  , initSelector
  , newSelector
  , nsSymbolEffectOptionsOptionsWithNonRepeatingSelector
  , nsSymbolEffectOptionsOptionsWithRepeatBehaviorSelector
  , nsSymbolEffectOptionsOptionsWithRepeatCountSelector
  , nsSymbolEffectOptionsOptionsWithRepeatingSelector
  , nsSymbolEffectOptionsOptionsWithSpeedSelector
  , optionsSelector
  , optionsWithNonRepeatingSelector
  , optionsWithRepeatBehaviorSelector
  , optionsWithRepeatCountSelector
  , optionsWithRepeatingSelector
  , optionsWithSpeedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSSymbolEffectOptions)
new  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> IO (Id NSSymbolEffectOptions)
init_ nsSymbolEffectOptions =
  sendOwnedMessage nsSymbolEffectOptions initSelector

-- | The default options.
--
-- ObjC selector: @+ options@
options :: IO (Id NSSymbolEffectOptions)
options  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMessage cls' optionsSelector

-- | Convenience initializer that prefers to repeat indefinitely.
--
-- ObjC selector: @+ optionsWithRepeating@
nsSymbolEffectOptionsOptionsWithRepeating :: IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeating  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMessage cls' nsSymbolEffectOptionsOptionsWithRepeatingSelector

-- | Return a copy of the options that prefers to repeat indefinitely.
--
-- ObjC selector: @- optionsWithRepeating@
optionsWithRepeating :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> IO (Id NSSymbolEffectOptions)
optionsWithRepeating nsSymbolEffectOptions =
  sendMessage nsSymbolEffectOptions optionsWithRepeatingSelector

-- | Convenience initializer that prefers not to repeat.
--
-- ObjC selector: @+ optionsWithNonRepeating@
nsSymbolEffectOptionsOptionsWithNonRepeating :: IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithNonRepeating  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMessage cls' nsSymbolEffectOptionsOptionsWithNonRepeatingSelector

-- | Return a copy of the options that prefers not to repeat.
--
-- ObjC selector: @- optionsWithNonRepeating@
optionsWithNonRepeating :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> IO (Id NSSymbolEffectOptions)
optionsWithNonRepeating nsSymbolEffectOptions =
  sendMessage nsSymbolEffectOptions optionsWithNonRepeatingSelector

-- | Convenience initializer setting a preferred repeat count.
--
-- - Parameter count: The preferred number of times to play the   effect. Very large or small values may be clamped.
--
-- - Returns: A new options object with the preferred repeat count.
--
-- ObjC selector: @+ optionsWithRepeatCount:@
nsSymbolEffectOptionsOptionsWithRepeatCount :: CLong -> IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeatCount count =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMessage cls' nsSymbolEffectOptionsOptionsWithRepeatCountSelector count

-- | Return a copy of the options setting a preferred repeat count.
--
-- - Parameter count: The preferred number of times to play the   effect. Very large or small values may be clamped.
--
-- - Returns: A new options object with the preferred repeat count.
--
-- ObjC selector: @- optionsWithRepeatCount:@
optionsWithRepeatCount :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> CLong -> IO (Id NSSymbolEffectOptions)
optionsWithRepeatCount nsSymbolEffectOptions count =
  sendMessage nsSymbolEffectOptions optionsWithRepeatCountSelector count

-- | Convenience initializer setting the preferred speed multiplier.
--
-- - Parameter speed: the preferred speed multiplier to play the effect with. The default multiplier is @1.0@. Very large or small values may be clamped.
--
-- - Returns: A new instance with the preferred speed multiplier.
--
-- ObjC selector: @+ optionsWithSpeed:@
nsSymbolEffectOptionsOptionsWithSpeed :: CDouble -> IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithSpeed speed =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMessage cls' nsSymbolEffectOptionsOptionsWithSpeedSelector speed

-- | Return a copy of the options setting the preferred speed multiplier.
--
-- - Parameter speed: The preferred speed multiplier to play the effect with. The default multiplier is @1.0@. Very large or small values may be clamped.
--
-- - Returns: A new instance with the preferred speed multiplier.
--
-- ObjC selector: @- optionsWithSpeed:@
optionsWithSpeed :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> CDouble -> IO (Id NSSymbolEffectOptions)
optionsWithSpeed nsSymbolEffectOptions speed =
  sendMessage nsSymbolEffectOptions optionsWithSpeedSelector speed

-- | Convenience initializer setting a preferred repeat behavior.
--
-- - Parameter behavior: The preferred behavior when the effect is repeated.
--
-- - Returns: A new options object with the preferred repeat behavior.
--
-- ObjC selector: @+ optionsWithRepeatBehavior:@
nsSymbolEffectOptionsOptionsWithRepeatBehavior :: IsNSSymbolEffectOptionsRepeatBehavior behavior => behavior -> IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeatBehavior behavior =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMessage cls' nsSymbolEffectOptionsOptionsWithRepeatBehaviorSelector (toNSSymbolEffectOptionsRepeatBehavior behavior)

-- | Return a copy of the options setting a preferred repeat behavior.
--
-- - Parameter behavior: The preferred behavior when the effect is repeated.
--
-- - Returns: A new options object with the preferred repeat behavior.
--
-- ObjC selector: @- optionsWithRepeatBehavior:@
optionsWithRepeatBehavior :: (IsNSSymbolEffectOptions nsSymbolEffectOptions, IsNSSymbolEffectOptionsRepeatBehavior behavior) => nsSymbolEffectOptions -> behavior -> IO (Id NSSymbolEffectOptions)
optionsWithRepeatBehavior nsSymbolEffectOptions behavior =
  sendMessage nsSymbolEffectOptions optionsWithRepeatBehaviorSelector (toNSSymbolEffectOptionsRepeatBehavior behavior)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSSymbolEffectOptions)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSymbolEffectOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSSymbolEffectOptions)
optionsSelector = mkSelector "options"

-- | @Selector@ for @optionsWithRepeating@
nsSymbolEffectOptionsOptionsWithRepeatingSelector :: Selector '[] (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeatingSelector = mkSelector "optionsWithRepeating"

-- | @Selector@ for @optionsWithRepeating@
optionsWithRepeatingSelector :: Selector '[] (Id NSSymbolEffectOptions)
optionsWithRepeatingSelector = mkSelector "optionsWithRepeating"

-- | @Selector@ for @optionsWithNonRepeating@
nsSymbolEffectOptionsOptionsWithNonRepeatingSelector :: Selector '[] (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithNonRepeatingSelector = mkSelector "optionsWithNonRepeating"

-- | @Selector@ for @optionsWithNonRepeating@
optionsWithNonRepeatingSelector :: Selector '[] (Id NSSymbolEffectOptions)
optionsWithNonRepeatingSelector = mkSelector "optionsWithNonRepeating"

-- | @Selector@ for @optionsWithRepeatCount:@
nsSymbolEffectOptionsOptionsWithRepeatCountSelector :: Selector '[CLong] (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeatCountSelector = mkSelector "optionsWithRepeatCount:"

-- | @Selector@ for @optionsWithRepeatCount:@
optionsWithRepeatCountSelector :: Selector '[CLong] (Id NSSymbolEffectOptions)
optionsWithRepeatCountSelector = mkSelector "optionsWithRepeatCount:"

-- | @Selector@ for @optionsWithSpeed:@
nsSymbolEffectOptionsOptionsWithSpeedSelector :: Selector '[CDouble] (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithSpeedSelector = mkSelector "optionsWithSpeed:"

-- | @Selector@ for @optionsWithSpeed:@
optionsWithSpeedSelector :: Selector '[CDouble] (Id NSSymbolEffectOptions)
optionsWithSpeedSelector = mkSelector "optionsWithSpeed:"

-- | @Selector@ for @optionsWithRepeatBehavior:@
nsSymbolEffectOptionsOptionsWithRepeatBehaviorSelector :: Selector '[Id NSSymbolEffectOptionsRepeatBehavior] (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeatBehaviorSelector = mkSelector "optionsWithRepeatBehavior:"

-- | @Selector@ for @optionsWithRepeatBehavior:@
optionsWithRepeatBehaviorSelector :: Selector '[Id NSSymbolEffectOptionsRepeatBehavior] (Id NSSymbolEffectOptions)
optionsWithRepeatBehaviorSelector = mkSelector "optionsWithRepeatBehavior:"

