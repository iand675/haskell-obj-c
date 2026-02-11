{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Options configuring how symbol effects apply to symbol views.
--
-- Generated bindings for @NSSymbolEffectOptions@.
module ObjC.AppKit.NSSymbolEffectOptions
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
  , newSelector
  , initSelector
  , optionsSelector
  , optionsWithRepeatingSelector
  , optionsWithNonRepeatingSelector
  , optionsWithRepeatCountSelector
  , optionsWithSpeedSelector
  , optionsWithRepeatBehaviorSelector


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
import ObjC.Symbols.Internal.Classes

-- | @+ new@
new :: IO (Id NSSymbolEffectOptions)
new  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> IO (Id NSSymbolEffectOptions)
init_ nsSymbolEffectOptions  =
  sendMsg nsSymbolEffectOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The default options.
--
-- ObjC selector: @+ options@
options :: IO (Id NSSymbolEffectOptions)
options  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMsg cls' (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer that prefers to repeat indefinitely.
--
-- ObjC selector: @+ optionsWithRepeating@
nsSymbolEffectOptionsOptionsWithRepeating :: IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithRepeating  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMsg cls' (mkSelector "optionsWithRepeating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return a copy of the options that prefers to repeat indefinitely.
--
-- ObjC selector: @- optionsWithRepeating@
optionsWithRepeating :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> IO (Id NSSymbolEffectOptions)
optionsWithRepeating nsSymbolEffectOptions  =
  sendMsg nsSymbolEffectOptions (mkSelector "optionsWithRepeating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer that prefers not to repeat.
--
-- ObjC selector: @+ optionsWithNonRepeating@
nsSymbolEffectOptionsOptionsWithNonRepeating :: IO (Id NSSymbolEffectOptions)
nsSymbolEffectOptionsOptionsWithNonRepeating  =
  do
    cls' <- getRequiredClass "NSSymbolEffectOptions"
    sendClassMsg cls' (mkSelector "optionsWithNonRepeating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return a copy of the options that prefers not to repeat.
--
-- ObjC selector: @- optionsWithNonRepeating@
optionsWithNonRepeating :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> IO (Id NSSymbolEffectOptions)
optionsWithNonRepeating nsSymbolEffectOptions  =
  sendMsg nsSymbolEffectOptions (mkSelector "optionsWithNonRepeating") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "optionsWithRepeatCount:") (retPtr retVoid) [argCLong (fromIntegral count)] >>= retainedObject . castPtr

-- | Return a copy of the options setting a preferred repeat count.
--
-- - Parameter count: The preferred number of times to play the   effect. Very large or small values may be clamped.
--
-- - Returns: A new options object with the preferred repeat count.
--
-- ObjC selector: @- optionsWithRepeatCount:@
optionsWithRepeatCount :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> CLong -> IO (Id NSSymbolEffectOptions)
optionsWithRepeatCount nsSymbolEffectOptions  count =
  sendMsg nsSymbolEffectOptions (mkSelector "optionsWithRepeatCount:") (retPtr retVoid) [argCLong (fromIntegral count)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "optionsWithSpeed:") (retPtr retVoid) [argCDouble (fromIntegral speed)] >>= retainedObject . castPtr

-- | Return a copy of the options setting the preferred speed multiplier.
--
-- - Parameter speed: The preferred speed multiplier to play the effect with. The default multiplier is @1.0@. Very large or small values may be clamped.
--
-- - Returns: A new instance with the preferred speed multiplier.
--
-- ObjC selector: @- optionsWithSpeed:@
optionsWithSpeed :: IsNSSymbolEffectOptions nsSymbolEffectOptions => nsSymbolEffectOptions -> CDouble -> IO (Id NSSymbolEffectOptions)
optionsWithSpeed nsSymbolEffectOptions  speed =
  sendMsg nsSymbolEffectOptions (mkSelector "optionsWithSpeed:") (retPtr retVoid) [argCDouble (fromIntegral speed)] >>= retainedObject . castPtr

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
    withObjCPtr behavior $ \raw_behavior ->
      sendClassMsg cls' (mkSelector "optionsWithRepeatBehavior:") (retPtr retVoid) [argPtr (castPtr raw_behavior :: Ptr ())] >>= retainedObject . castPtr

-- | Return a copy of the options setting a preferred repeat behavior.
--
-- - Parameter behavior: The preferred behavior when the effect is repeated.
--
-- - Returns: A new options object with the preferred repeat behavior.
--
-- ObjC selector: @- optionsWithRepeatBehavior:@
optionsWithRepeatBehavior :: (IsNSSymbolEffectOptions nsSymbolEffectOptions, IsNSSymbolEffectOptionsRepeatBehavior behavior) => nsSymbolEffectOptions -> behavior -> IO (Id NSSymbolEffectOptions)
optionsWithRepeatBehavior nsSymbolEffectOptions  behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg nsSymbolEffectOptions (mkSelector "optionsWithRepeatBehavior:") (retPtr retVoid) [argPtr (castPtr raw_behavior :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @optionsWithRepeating@
optionsWithRepeatingSelector :: Selector
optionsWithRepeatingSelector = mkSelector "optionsWithRepeating"

-- | @Selector@ for @optionsWithNonRepeating@
optionsWithNonRepeatingSelector :: Selector
optionsWithNonRepeatingSelector = mkSelector "optionsWithNonRepeating"

-- | @Selector@ for @optionsWithRepeatCount:@
optionsWithRepeatCountSelector :: Selector
optionsWithRepeatCountSelector = mkSelector "optionsWithRepeatCount:"

-- | @Selector@ for @optionsWithSpeed:@
optionsWithSpeedSelector :: Selector
optionsWithSpeedSelector = mkSelector "optionsWithSpeed:"

-- | @Selector@ for @optionsWithRepeatBehavior:@
optionsWithRepeatBehaviorSelector :: Selector
optionsWithRepeatBehaviorSelector = mkSelector "optionsWithRepeatBehavior:"

