{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKSmartCardToken base class for implementing SmartCard based token.
--
-- When implementing SmartCard token extension, subclass TKSmartCardToken and implement TKTokenDelegate on it.
--
-- Generated bindings for @TKSmartCardToken@.
module ObjC.CryptoTokenKit.TKSmartCardToken
  ( TKSmartCardToken
  , IsTKSmartCardToken(..)
  , initWithSmartCard_AID_instanceID_tokenDriver
  , initWithTokenDriver_instanceID
  , aid
  , initWithSmartCard_AID_instanceID_tokenDriverSelector
  , initWithTokenDriver_instanceIDSelector
  , aidSelector


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

-- | Initializes token instance with specified attributes.
--
-- @smartCard@ — TKSmartCard instance representing connection to SmartCard on which the intance should operate.
--
-- @AID@ — ISO7816-4 application ID which is preselected on the card.
--
-- @instanceID@ — Unique, persistent identifier of this token.  This is typically implemented by some kind of SmartCard serial number.
--
-- @tokenDriver@ — associated driver which initiated creation of this token.
--
-- ObjC selector: @- initWithSmartCard:AID:instanceID:tokenDriver:@
initWithSmartCard_AID_instanceID_tokenDriver :: (IsTKSmartCardToken tkSmartCardToken, IsTKSmartCard smartCard, IsNSData aid, IsNSString instanceID, IsTKSmartCardTokenDriver tokenDriver) => tkSmartCardToken -> smartCard -> aid -> instanceID -> tokenDriver -> IO (Id TKSmartCardToken)
initWithSmartCard_AID_instanceID_tokenDriver tkSmartCardToken  smartCard aid instanceID tokenDriver =
withObjCPtr smartCard $ \raw_smartCard ->
  withObjCPtr aid $ \raw_aid ->
    withObjCPtr instanceID $ \raw_instanceID ->
      withObjCPtr tokenDriver $ \raw_tokenDriver ->
          sendMsg tkSmartCardToken (mkSelector "initWithSmartCard:AID:instanceID:tokenDriver:") (retPtr retVoid) [argPtr (castPtr raw_smartCard :: Ptr ()), argPtr (castPtr raw_aid :: Ptr ()), argPtr (castPtr raw_instanceID :: Ptr ()), argPtr (castPtr raw_tokenDriver :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithTokenDriver:instanceID:@
initWithTokenDriver_instanceID :: (IsTKSmartCardToken tkSmartCardToken, IsTKTokenDriver tokenDriver, IsNSString instanceID) => tkSmartCardToken -> tokenDriver -> instanceID -> IO (Id TKSmartCardToken)
initWithTokenDriver_instanceID tkSmartCardToken  tokenDriver instanceID =
withObjCPtr tokenDriver $ \raw_tokenDriver ->
  withObjCPtr instanceID $ \raw_instanceID ->
      sendMsg tkSmartCardToken (mkSelector "initWithTokenDriver:instanceID:") (retPtr retVoid) [argPtr (castPtr raw_tokenDriver :: Ptr ()), argPtr (castPtr raw_instanceID :: Ptr ())] >>= ownedObject . castPtr

-- | This is AID which is specified in extension's plist NSExtensionAttributes as @com.apple.ctk.aid@ attribute. If the attribute specifies array of multiple AIDs, this parameter represents AID which was found on the card and is already preselected.  If @com.apple.ctk.aid@ is not present, no application is automatically preselected and value of this property is nil.
--
-- ObjC selector: @- AID@
aid :: IsTKSmartCardToken tkSmartCardToken => tkSmartCardToken -> IO (Id NSData)
aid tkSmartCardToken  =
  sendMsg tkSmartCardToken (mkSelector "AID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSmartCard:AID:instanceID:tokenDriver:@
initWithSmartCard_AID_instanceID_tokenDriverSelector :: Selector
initWithSmartCard_AID_instanceID_tokenDriverSelector = mkSelector "initWithSmartCard:AID:instanceID:tokenDriver:"

-- | @Selector@ for @initWithTokenDriver:instanceID:@
initWithTokenDriver_instanceIDSelector :: Selector
initWithTokenDriver_instanceIDSelector = mkSelector "initWithTokenDriver:instanceID:"

-- | @Selector@ for @AID@
aidSelector :: Selector
aidSelector = mkSelector "AID"

