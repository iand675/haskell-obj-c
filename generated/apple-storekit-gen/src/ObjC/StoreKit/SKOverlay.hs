{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKOverlay@.
module ObjC.StoreKit.SKOverlay
  ( SKOverlay
  , IsSKOverlay(..)
  , init_
  , new
  , initWithConfiguration
  , delegate
  , setDelegate
  , configuration
  , initSelector
  , newSelector
  , initWithConfigurationSelector
  , delegateSelector
  , setDelegateSelector
  , configurationSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlay skOverlay => skOverlay -> IO (Id SKOverlay)
init_ skOverlay  =
    sendMsg skOverlay (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SKOverlay)
new  =
  do
    cls' <- getRequiredClass "SKOverlay"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates an overlay with the provided configuration.
--
-- @configuration@ — the configuration for the overlay.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsSKOverlay skOverlay, IsSKOverlayConfiguration configuration) => skOverlay -> configuration -> IO (Id SKOverlay)
initWithConfiguration skOverlay  configuration =
  withObjCPtr configuration $ \raw_configuration ->
      sendMsg skOverlay (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | A delegate for overlay events.
--
-- ObjC selector: @- delegate@
delegate :: IsSKOverlay skOverlay => skOverlay -> IO RawId
delegate skOverlay  =
    fmap (RawId . castPtr) $ sendMsg skOverlay (mkSelector "delegate") (retPtr retVoid) []

-- | A delegate for overlay events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSKOverlay skOverlay => skOverlay -> RawId -> IO ()
setDelegate skOverlay  value =
    sendMsg skOverlay (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The overlay configuration.
--
-- ObjC selector: @- configuration@
configuration :: IsSKOverlay skOverlay => skOverlay -> IO (Id SKOverlayConfiguration)
configuration skOverlay  =
    sendMsg skOverlay (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

