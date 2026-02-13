{-# LANGUAGE DataKinds #-}
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
  , configurationSelector
  , delegateSelector
  , initSelector
  , initWithConfigurationSelector
  , newSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKOverlay skOverlay => skOverlay -> IO (Id SKOverlay)
init_ skOverlay =
  sendOwnedMessage skOverlay initSelector

-- | @+ new@
new :: IO (Id SKOverlay)
new  =
  do
    cls' <- getRequiredClass "SKOverlay"
    sendOwnedClassMessage cls' newSelector

-- | Creates an overlay with the provided configuration.
--
-- @configuration@ — the configuration for the overlay.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsSKOverlay skOverlay, IsSKOverlayConfiguration configuration) => skOverlay -> configuration -> IO (Id SKOverlay)
initWithConfiguration skOverlay configuration =
  sendOwnedMessage skOverlay initWithConfigurationSelector (toSKOverlayConfiguration configuration)

-- | A delegate for overlay events.
--
-- ObjC selector: @- delegate@
delegate :: IsSKOverlay skOverlay => skOverlay -> IO RawId
delegate skOverlay =
  sendMessage skOverlay delegateSelector

-- | A delegate for overlay events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSKOverlay skOverlay => skOverlay -> RawId -> IO ()
setDelegate skOverlay value =
  sendMessage skOverlay setDelegateSelector value

-- | The overlay configuration.
--
-- ObjC selector: @- configuration@
configuration :: IsSKOverlay skOverlay => skOverlay -> IO (Id SKOverlayConfiguration)
configuration skOverlay =
  sendMessage skOverlay configurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKOverlay)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SKOverlay)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector '[Id SKOverlayConfiguration] (Id SKOverlay)
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id SKOverlayConfiguration)
configurationSelector = mkSelector "configuration"

