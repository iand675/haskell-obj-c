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
  , registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector
  , arcadeSubscriptionStatusWithNonce_resultHandlerSelector
  , repairArcadeAppSelector


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

-- | @+ registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:@
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandler :: IsNSData randomFromLib => randomFromLib -> CUInt -> Ptr () -> IO ()
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandler randomFromLib randomFromLibLength resultHandler =
  do
    cls' <- getRequiredClass "SKArcadeService"
    withObjCPtr randomFromLib $ \raw_randomFromLib ->
      sendClassMsg cls' (mkSelector "registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:") retVoid [argPtr (castPtr raw_randomFromLib :: Ptr ()), argCUInt (fromIntegral randomFromLibLength), argPtr (castPtr resultHandler :: Ptr ())]

-- | @+ arcadeSubscriptionStatusWithNonce:resultHandler:@
arcadeSubscriptionStatusWithNonce_resultHandler :: CULong -> Ptr () -> IO ()
arcadeSubscriptionStatusWithNonce_resultHandler nonce resultHandler =
  do
    cls' <- getRequiredClass "SKArcadeService"
    sendClassMsg cls' (mkSelector "arcadeSubscriptionStatusWithNonce:resultHandler:") retVoid [argCULong (fromIntegral nonce), argPtr (castPtr resultHandler :: Ptr ())]

-- | @+ repairArcadeApp@
repairArcadeApp :: IO ()
repairArcadeApp  =
  do
    cls' <- getRequiredClass "SKArcadeService"
    sendClassMsg cls' (mkSelector "repairArcadeApp") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:@
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector :: Selector
registerArcadeAppWithRandomFromLib_randomFromLibLength_resultHandlerSelector = mkSelector "registerArcadeAppWithRandomFromLib:randomFromLibLength:resultHandler:"

-- | @Selector@ for @arcadeSubscriptionStatusWithNonce:resultHandler:@
arcadeSubscriptionStatusWithNonce_resultHandlerSelector :: Selector
arcadeSubscriptionStatusWithNonce_resultHandlerSelector = mkSelector "arcadeSubscriptionStatusWithNonce:resultHandler:"

-- | @Selector@ for @repairArcadeApp@
repairArcadeAppSelector :: Selector
repairArcadeAppSelector = mkSelector "repairArcadeApp"

