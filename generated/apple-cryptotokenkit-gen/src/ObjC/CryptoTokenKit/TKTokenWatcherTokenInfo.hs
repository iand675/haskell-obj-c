{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @TKTokenWatcherTokenInfo@.
module ObjC.CryptoTokenKit.TKTokenWatcherTokenInfo
  ( TKTokenWatcherTokenInfo
  , IsTKTokenWatcherTokenInfo(..)
  , init_
  , new
  , tokenID
  , slotName
  , driverName
  , driverNameSelector
  , initSelector
  , newSelector
  , slotNameSelector
  , tokenIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id TKTokenWatcherTokenInfo)
init_ tkTokenWatcherTokenInfo =
  sendOwnedMessage tkTokenWatcherTokenInfo initSelector

-- | @+ new@
new :: IO (Id TKTokenWatcherTokenInfo)
new  =
  do
    cls' <- getRequiredClass "TKTokenWatcherTokenInfo"
    sendOwnedClassMessage cls' newSelector

-- | TokenID
--
-- ObjC selector: @- tokenID@
tokenID :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id NSString)
tokenID tkTokenWatcherTokenInfo =
  sendMessage tkTokenWatcherTokenInfo tokenIDSelector

-- | The slot name (if available)
--
-- ObjC selector: @- slotName@
slotName :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id NSString)
slotName tkTokenWatcherTokenInfo =
  sendMessage tkTokenWatcherTokenInfo slotNameSelector

-- | Localized driver name (if available)
--
-- ObjC selector: @- driverName@
driverName :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id NSString)
driverName tkTokenWatcherTokenInfo =
  sendMessage tkTokenWatcherTokenInfo driverNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenWatcherTokenInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id TKTokenWatcherTokenInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @tokenID@
tokenIDSelector :: Selector '[] (Id NSString)
tokenIDSelector = mkSelector "tokenID"

-- | @Selector@ for @slotName@
slotNameSelector :: Selector '[] (Id NSString)
slotNameSelector = mkSelector "slotName"

-- | @Selector@ for @driverName@
driverNameSelector :: Selector '[] (Id NSString)
driverNameSelector = mkSelector "driverName"

