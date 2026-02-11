{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLAuthenticationChallenge
--
-- This class represents an authentication challenge. It    provides all the information about the challenge, and has a method    to indicate when it's done.
--
-- Generated bindings for @NSURLAuthenticationChallenge@.
module ObjC.Foundation.NSURLAuthenticationChallenge
  ( NSURLAuthenticationChallenge
  , IsNSURLAuthenticationChallenge(..)
  , initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_sender
  , initWithAuthenticationChallenge_sender
  , protectionSpace
  , proposedCredential
  , previousFailureCount
  , failureResponse
  , error_
  , sender
  , initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_senderSelector
  , initWithAuthenticationChallenge_senderSelector
  , protectionSpaceSelector
  , proposedCredentialSelector
  , previousFailureCountSelector
  , failureResponseSelector
  , errorSelector
  , senderSelector


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

import ObjC.Foundation.Internal.Classes

-- | initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:
--
-- Initialize an authentication challenge
--
-- @space@ — The NSURLProtectionSpace to use
--
-- @credential@ — The proposed NSURLCredential for this challenge, or nil
--
-- @previousFailureCount@ — A count of previous failures attempting access.
--
-- @response@ — The NSURLResponse for the authentication failure, if applicable, else nil
--
-- @error@ — The NSError for the authentication failure, if applicable, else nil
--
-- Returns: An authentication challenge initialized with the specified parameters
--
-- ObjC selector: @- initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:@
initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_sender :: (IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge, IsNSURLProtectionSpace space, IsNSURLCredential credential, IsNSURLResponse response, IsNSError error_) => nsurlAuthenticationChallenge -> space -> credential -> CLong -> response -> error_ -> RawId -> IO (Id NSURLAuthenticationChallenge)
initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_sender nsurlAuthenticationChallenge  space credential previousFailureCount response error_ sender =
  withObjCPtr space $ \raw_space ->
    withObjCPtr credential $ \raw_credential ->
      withObjCPtr response $ \raw_response ->
        withObjCPtr error_ $ \raw_error_ ->
            sendMsg nsurlAuthenticationChallenge (mkSelector "initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ()), argPtr (castPtr raw_credential :: Ptr ()), argCLong previousFailureCount, argPtr (castPtr raw_response :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr (unRawId sender) :: Ptr ())] >>= ownedObject . castPtr

-- | initWithAuthenticationChallenge:
--
-- Initialize an authentication challenge copying all parameters from another one.
--
-- Returns: A new challenge initialized with the parameters from the passed in challenge
--
-- This initializer may be useful to subclassers that want to proxy    one type of authentication challenge to look like another type.
--
-- ObjC selector: @- initWithAuthenticationChallenge:sender:@
initWithAuthenticationChallenge_sender :: (IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge, IsNSURLAuthenticationChallenge challenge) => nsurlAuthenticationChallenge -> challenge -> RawId -> IO (Id NSURLAuthenticationChallenge)
initWithAuthenticationChallenge_sender nsurlAuthenticationChallenge  challenge sender =
  withObjCPtr challenge $ \raw_challenge ->
      sendMsg nsurlAuthenticationChallenge (mkSelector "initWithAuthenticationChallenge:sender:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr (unRawId sender) :: Ptr ())] >>= ownedObject . castPtr

-- | Get a description of the protection space that requires authentication
--
-- Returns: The protection space that needs authentication
--
-- ObjC selector: @- protectionSpace@
protectionSpace :: IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge => nsurlAuthenticationChallenge -> IO (Id NSURLProtectionSpace)
protectionSpace nsurlAuthenticationChallenge  =
    sendMsg nsurlAuthenticationChallenge (mkSelector "protectionSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the proposed credential for this challenge
--
-- Returns: The proposed credential
--
-- proposedCredential may be nil, if there is no default    credential to use for this challenge (either stored or in the    URL). If the credential is not nil and returns YES for    hasPassword, this means the NSURLConnection thinks the credential    is ready to use as-is. If it returns NO for hasPassword, then the    credential is not ready to use as-is, but provides a default    username the client could use when prompting.
--
-- ObjC selector: @- proposedCredential@
proposedCredential :: IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge => nsurlAuthenticationChallenge -> IO (Id NSURLCredential)
proposedCredential nsurlAuthenticationChallenge  =
    sendMsg nsurlAuthenticationChallenge (mkSelector "proposedCredential") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get count of previous failed authentication attempts
--
-- Returns: The count of previous failures
--
-- ObjC selector: @- previousFailureCount@
previousFailureCount :: IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge => nsurlAuthenticationChallenge -> IO CLong
previousFailureCount nsurlAuthenticationChallenge  =
    sendMsg nsurlAuthenticationChallenge (mkSelector "previousFailureCount") retCLong []

-- | Get the response representing authentication failure.
--
-- Returns: The failure response or nil
--
-- If there was a previous authentication failure, and    this protocol uses responses to indicate authentication failure,    then this method will return the response. Otherwise it will    return nil.
--
-- ObjC selector: @- failureResponse@
failureResponse :: IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge => nsurlAuthenticationChallenge -> IO (Id NSURLResponse)
failureResponse nsurlAuthenticationChallenge  =
    sendMsg nsurlAuthenticationChallenge (mkSelector "failureResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the error representing authentication failure.
--
-- If there was a previous authentication failure, and    this protocol uses errors to indicate authentication failure,    then this method will return the error. Otherwise it will    return nil.
--
-- ObjC selector: @- error@
error_ :: IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge => nsurlAuthenticationChallenge -> IO (Id NSError)
error_ nsurlAuthenticationChallenge  =
    sendMsg nsurlAuthenticationChallenge (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the sender of this challenge
--
-- Returns: The sender of the challenge
--
-- The sender is the object you should reply to when done processing the challenge.
--
-- ObjC selector: @- sender@
sender :: IsNSURLAuthenticationChallenge nsurlAuthenticationChallenge => nsurlAuthenticationChallenge -> IO RawId
sender nsurlAuthenticationChallenge  =
    fmap (RawId . castPtr) $ sendMsg nsurlAuthenticationChallenge (mkSelector "sender") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:@
initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_senderSelector :: Selector
initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_senderSelector = mkSelector "initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:"

-- | @Selector@ for @initWithAuthenticationChallenge:sender:@
initWithAuthenticationChallenge_senderSelector :: Selector
initWithAuthenticationChallenge_senderSelector = mkSelector "initWithAuthenticationChallenge:sender:"

-- | @Selector@ for @protectionSpace@
protectionSpaceSelector :: Selector
protectionSpaceSelector = mkSelector "protectionSpace"

-- | @Selector@ for @proposedCredential@
proposedCredentialSelector :: Selector
proposedCredentialSelector = mkSelector "proposedCredential"

-- | @Selector@ for @previousFailureCount@
previousFailureCountSelector :: Selector
previousFailureCountSelector = mkSelector "previousFailureCount"

-- | @Selector@ for @failureResponse@
failureResponseSelector :: Selector
failureResponseSelector = mkSelector "failureResponse"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @sender@
senderSelector :: Selector
senderSelector = mkSelector "sender"

