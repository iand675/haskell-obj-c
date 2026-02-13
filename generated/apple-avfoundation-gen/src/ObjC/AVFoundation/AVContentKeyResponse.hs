{-# LANGUAGE DataKinds #-}
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
  , contentKeyResponseWithAuthorizationTokenDataSelector
  , contentKeyResponseWithClearKeyData_initializationVectorSelector
  , contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector (toNSData keyResponseData)

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
    sendClassMessage cls' contentKeyResponseWithClearKeyData_initializationVectorSelector (toNSData keyData) (toNSData initializationVector)

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
    sendClassMessage cls' contentKeyResponseWithAuthorizationTokenDataSelector (toNSData authorizationTokenData)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentKeyResponseWithFairPlayStreamingKeyResponseData:@
contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector :: Selector '[Id NSData] (Id AVContentKeyResponse)
contentKeyResponseWithFairPlayStreamingKeyResponseDataSelector = mkSelector "contentKeyResponseWithFairPlayStreamingKeyResponseData:"

-- | @Selector@ for @contentKeyResponseWithClearKeyData:initializationVector:@
contentKeyResponseWithClearKeyData_initializationVectorSelector :: Selector '[Id NSData, Id NSData] (Id AVContentKeyResponse)
contentKeyResponseWithClearKeyData_initializationVectorSelector = mkSelector "contentKeyResponseWithClearKeyData:initializationVector:"

-- | @Selector@ for @contentKeyResponseWithAuthorizationTokenData:@
contentKeyResponseWithAuthorizationTokenDataSelector :: Selector '[Id NSData] (Id AVContentKeyResponse)
contentKeyResponseWithAuthorizationTokenDataSelector = mkSelector "contentKeyResponseWithAuthorizationTokenData:"

