{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKSmartCardTokenSession represents token session based on SmartCard token.
--
-- When implementing SmartCard token extension, subclass TKSmartCardTokenSession and implement TKTokenSessionDelegate on it.  Use #token property to get access and send APDUs to the underlying SmartCard.
--
-- Generated bindings for @TKSmartCardTokenSession@.
module ObjC.CryptoTokenKit.TKSmartCardTokenSession
  ( TKSmartCardTokenSession
  , IsTKSmartCardTokenSession(..)
  , getSmartCardWithError
  , smartCard
  , getSmartCardWithErrorSelector
  , smartCardSelector


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

-- | Returns a TKSmartCard instance with an active exclusive session and the SmartCard application selected. Replaces the deprecated @smartCard@ property.
--
-- The TKSmartCard object is only accessible within the methods of the TKTokenSessionDelegate protocol. If the associated token has an AID set, the returned card will have an exclusive session already opened and the specified application selected. In this scenario: Do not call -[TKSmartCard beginSessionWithReply:]) on the returned SmartCard instance. The system manages the session lifecycle and will terminate it automatically when the current token request servicing is finished. Do not call -[TKSmartCard endSession]. You can use the @smartCard.context@ property to store any context-specific state information related to the card. This property is automatically set to @nil@ if the card is reset or accessed by a different TKSmartCard instance (potentially in another process). Before performing an operation, check the @TKSmartCard.context@ property for a previously stored value. This can help you avoid potentially costly restoration of the SmartCard state if it's already available.
--
-- @error@ — An NSError object containing details if the operation fails.
--
-- ObjC selector: @- getSmartCardWithError:@
getSmartCardWithError :: (IsTKSmartCardTokenSession tkSmartCardTokenSession, IsNSError error_) => tkSmartCardTokenSession -> error_ -> IO (Id TKSmartCard)
getSmartCardWithError tkSmartCardTokenSession  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg tkSmartCardTokenSession (mkSelector "getSmartCardWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | use -[getSmartCardWithError:] instead
--
-- contains TKSmartCard instance with active exclusive session and SmartCard application selected.
--
-- This property can be accessed only when handling one of the methods of TKTokenSessionDelegate protocol.  If associated token has set AID property, then the returned card has opened exclusive session to the card and the application is already selected.  Therefore there is no need to call -[TKSmartCard beginSessionWithReply:]) on returned SmartCard instance in such case and system will take care of terminating session when current token request servicing is finished,  -[TKSmartCard endSession] must not be called either.
--
-- You can store any kind of context state information representing state of the card into smartCard.context property.  This property will be automatically set to nil if the card is reset or accessed by different TKSmartCard instance (possibly in another process).  Checking TKSmartCard.context property for previously stored value can be used to avoid potentially costly restoring of SmartCard state before performing the operation.
--
-- ObjC selector: @- smartCard@
smartCard :: IsTKSmartCardTokenSession tkSmartCardTokenSession => tkSmartCardTokenSession -> IO (Id TKSmartCard)
smartCard tkSmartCardTokenSession  =
  sendMsg tkSmartCardTokenSession (mkSelector "smartCard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSmartCardWithError:@
getSmartCardWithErrorSelector :: Selector
getSmartCardWithErrorSelector = mkSelector "getSmartCardWithError:"

-- | @Selector@ for @smartCard@
smartCardSelector :: Selector
smartCardSelector = mkSelector "smartCard"

