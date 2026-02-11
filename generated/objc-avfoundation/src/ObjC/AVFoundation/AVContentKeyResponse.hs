{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVContentKeyResponse is used to represent the data returned from the key server when requesting a key for decrypting content.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVContentKeyResponse@.
module ObjC.AVFoundation.AVContentKeyResponse
  ( AVContentKeyResponse
  , IsAVContentKeyResponse(..)
  , contentKeyResponseWithFairPlayStreamingKeyResponseData
  , contentKeyResponseWithClearKeyData_initializationVector
  , contentKeyResponseWithAuthorizationTokenData
  , contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector
  , contentKeyResponseWithClearKeyData_initializationVectorSelector
  , contentKeyResponseWithAuthorizationTokenDataSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create an AVContentKeyResponse from the server response to a key request made when using FairPlayStreaming (FPS) as the method of key delivery.
--
-- The object created by this method is typically used with an AVContentKeyRequest created by an AVContentKeySession using keySystem AVContentKeySystemFairPlayStreaming. It is passed to AVContentKeyRequest -processContentKeyResponse: in order to supply the decryptor with key data
--
-- - Parameter keyResponseData: The response from the FairPlayStreaming key server
--
-- - Returns: A new AVContentKeyResponse holding data from a FairPlayStreaming key server that is used to decrypt the content
--
-- ObjC selector: @+ contentKeyResponseWithFairPlayStreamingKeyResponseData:@
contentKeyResponseWithFairPlayStreamingKeyResponseData :: IsNSData keyResponseData => keyResponseData -> IO (Id AVContentKeyResponse)
contentKeyResponseWithFairPlayStreamingKeyResponseData keyResponseData =
  do
    cls' <- getRequiredClass "AVContentKeyResponse"
    withObjCPtr keyResponseData $ \raw_keyResponseData ->
      sendClassMsg cls' (mkSelector "contentKeyResponseWithFairPlayStreamingKeyResponseData:") (retPtr retVoid) [argPtr (castPtr raw_keyResponseData :: Ptr ())] >>= retainedObject . castPtr

-- | Create an AVContentKeyResponse from the key and IV when using AVContentKeySystemClearKey as the key system
--
-- The object created by this method is typically used with an AVContentKeyRequest created by an AVContentKeySession using keySystem AVContentKeySystemClearKey. It is passed to AVContentKeyRequest -processContentKeyResponse: in order to supply the decryptor with key data.
--
-- - Parameter keyData: The key used for decrypting content. - Parameter initializationVector: The initialization vector used for decrypting content, or nil if initialization vector is available in the media to be decrypted
--
-- - Returns: A new AVContentKeyResponse holding Clear Key data.
--
-- ObjC selector: @+ contentKeyResponseWithClearKeyData:initializationVector:@
contentKeyResponseWithClearKeyData_initializationVector :: (IsNSData keyData, IsNSData initializationVector) => keyData -> initializationVector -> IO (Id AVContentKeyResponse)
contentKeyResponseWithClearKeyData_initializationVector keyData initializationVector =
  do
    cls' <- getRequiredClass "AVContentKeyResponse"
    withObjCPtr keyData $ \raw_keyData ->
      withObjCPtr initializationVector $ \raw_initializationVector ->
        sendClassMsg cls' (mkSelector "contentKeyResponseWithClearKeyData:initializationVector:") (retPtr retVoid) [argPtr (castPtr raw_keyData :: Ptr ()), argPtr (castPtr raw_initializationVector :: Ptr ())] >>= retainedObject . castPtr

-- | Create an AVContentKeyResponse from authorization token data when using AVContentKeySystemAuthorizationToken key system.
--
-- The object created by this method is typically used with an AVContentKeyRequest created by an AVContentKeySession using keySystem AVContentKeySystemAuthorizationToken. It is passed to AVContentKeyRequest -processContentKeyResponse: in order to supply the authorization token data.
--
-- - Parameter authorizationTokenData: Data blob containing the authorization token.
--
-- - Returns: A new AVContentKeyResponse holding the authorization token data.
--
-- ObjC selector: @+ contentKeyResponseWithAuthorizationTokenData:@
contentKeyResponseWithAuthorizationTokenData :: IsNSData authorizationTokenData => authorizationTokenData -> IO (Id AVContentKeyResponse)
contentKeyResponseWithAuthorizationTokenData authorizationTokenData =
  do
    cls' <- getRequiredClass "AVContentKeyResponse"
    withObjCPtr authorizationTokenData $ \raw_authorizationTokenData ->
      sendClassMsg cls' (mkSelector "contentKeyResponseWithAuthorizationTokenData:") (retPtr retVoid) [argPtr (castPtr raw_authorizationTokenData :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentKeyResponseWithFairPlayStreamingKeyResponseData:@
contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector :: Selector
contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector = mkSelector "contentKeyResponseWithFairPlayStreamingKeyResponseData:"

-- | @Selector@ for @contentKeyResponseWithClearKeyData:initializationVector:@
contentKeyResponseWithClearKeyData_initializationVectorSelector :: Selector
contentKeyResponseWithClearKeyData_initializationVectorSelector = mkSelector "contentKeyResponseWithClearKeyData:initializationVector:"

-- | @Selector@ for @contentKeyResponseWithAuthorizationTokenData:@
contentKeyResponseWithAuthorizationTokenDataSelector :: Selector
contentKeyResponseWithAuthorizationTokenDataSelector = mkSelector "contentKeyResponseWithAuthorizationTokenData:"

