{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKArcadeService@.
module ObjC.StoreKit.SKArcadeService
  ( SKArcadeService
  , IsSKArcadeService(..)
  , registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandler
  , arcadeSubscriptionStatusWithNonce_resultHandler
  , repairArcadeApp
  , arcadeSubscriptionStatusWithNonce_resultHandlerSelector
  , registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector
  , repairArcadeAppSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:@
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandler :: IsNSData randomFromLib => randomFromLib -> CUInt -> Ptr () -> IO ()
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandler randomFromLib randomFromLibLength resultHandler =
  do
    cls' <- getRequiredClass "SKArcadeService"
    sendClassMessage cls' registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector (toNSData randomFromLib) randomFromLibLength resultHandler

-- | @+ arcadeSubscriptionStatusWithNonce:resultHandler:@
arcadeSubscriptionStatusWithNonce_resultHandler :: CULong -> Ptr () -> IO ()
arcadeSubscriptionStatusWithNonce_resultHandler nonce resultHandler =
  do
    cls' <- getRequiredClass "SKArcadeService"
    sendClassMessage cls' arcadeSubscriptionStatusWithNonce_resultHandlerSelector nonce resultHandler

-- | @+ repairArcadeApp@
repairArcadeApp :: IO ()
repairArcadeApp  =
  do
    cls' <- getRequiredClass "SKArcadeService"
    sendClassMessage cls' repairArcadeAppSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:@
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector :: Selector '[Id NSData, CUInt, Ptr ()] ()
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector = mkSelector "registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:"

-- | @Selector@ for @arcadeSubscriptionStatusWithNonce:resultHandler:@
arcadeSubscriptionStatusWithNonce_resultHandlerSelector :: Selector '[CULong, Ptr ()] ()
arcadeSubscriptionStatusWithNonce_resultHandlerSelector = mkSelector "arcadeSubscriptionStatusWithNonce:resultHandler:"

-- | @Selector@ for @repairArcadeApp@
repairArcadeAppSelector :: Selector '[] ()
repairArcadeAppSelector = mkSelector "repairArcadeApp"

