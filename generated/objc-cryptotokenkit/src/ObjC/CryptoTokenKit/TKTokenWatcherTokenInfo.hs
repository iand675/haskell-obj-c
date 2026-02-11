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
  , initSelector
  , newSelector
  , tokenIDSelector
  , slotNameSelector
  , driverNameSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id TKTokenWatcherTokenInfo)
init_ tkTokenWatcherTokenInfo  =
  sendMsg tkTokenWatcherTokenInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id TKTokenWatcherTokenInfo)
new  =
  do
    cls' <- getRequiredClass "TKTokenWatcherTokenInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | TokenID
--
-- ObjC selector: @- tokenID@
tokenID :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id NSString)
tokenID tkTokenWatcherTokenInfo  =
  sendMsg tkTokenWatcherTokenInfo (mkSelector "tokenID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The slot name (if available)
--
-- ObjC selector: @- slotName@
slotName :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id NSString)
slotName tkTokenWatcherTokenInfo  =
  sendMsg tkTokenWatcherTokenInfo (mkSelector "slotName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized driver name (if available)
--
-- ObjC selector: @- driverName@
driverName :: IsTKTokenWatcherTokenInfo tkTokenWatcherTokenInfo => tkTokenWatcherTokenInfo -> IO (Id NSString)
driverName tkTokenWatcherTokenInfo  =
  sendMsg tkTokenWatcherTokenInfo (mkSelector "driverName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @tokenID@
tokenIDSelector :: Selector
tokenIDSelector = mkSelector "tokenID"

-- | @Selector@ for @slotName@
slotNameSelector :: Selector
slotNameSelector = mkSelector "slotName"

-- | @Selector@ for @driverName@
driverNameSelector :: Selector
driverNameSelector = mkSelector "driverName"

