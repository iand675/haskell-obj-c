{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This object represents the client data for a public key credential request, as defined in the WebAuthentication standard.
--
-- Generated bindings for @ASPublicKeyCredentialClientData@.
module ObjC.AuthenticationServices.ASPublicKeyCredentialClientData
  ( ASPublicKeyCredentialClientData
  , IsASPublicKeyCredentialClientData(..)
  , new
  , init_
  , initWithChallenge_origin
  , challenge
  , setChallenge
  , origin
  , setOrigin
  , topOrigin
  , setTopOrigin
  , crossOrigin
  , setCrossOrigin
  , challengeSelector
  , crossOriginSelector
  , initSelector
  , initWithChallenge_originSelector
  , newSelector
  , originSelector
  , setChallengeSelector
  , setCrossOriginSelector
  , setOriginSelector
  , setTopOriginSelector
  , topOriginSelector

  -- * Enum types
  , ASPublicKeyCredentialClientDataCrossOriginValue(ASPublicKeyCredentialClientDataCrossOriginValue)
  , pattern ASPublicKeyCredentialClientDataCrossOriginValueNotSet
  , pattern ASPublicKeyCredentialClientDataCrossOriginValueCrossOrigin
  , pattern ASPublicKeyCredentialClientDataCrossOriginValueSameOriginWithAncestors

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASPublicKeyCredentialClientData)
new  =
  do
    cls' <- getRequiredClass "ASPublicKeyCredentialClientData"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id ASPublicKeyCredentialClientData)
init_ asPublicKeyCredentialClientData =
  sendOwnedMessage asPublicKeyCredentialClientData initSelector

-- | @- initWithChallenge:origin:@
initWithChallenge_origin :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSData challenge, IsNSString origin) => asPublicKeyCredentialClientData -> challenge -> origin -> IO (Id ASPublicKeyCredentialClientData)
initWithChallenge_origin asPublicKeyCredentialClientData challenge origin =
  sendOwnedMessage asPublicKeyCredentialClientData initWithChallenge_originSelector (toNSData challenge) (toNSString origin)

-- | The challenge to be signed during the operation.
--
-- ObjC selector: @- challenge@
challenge :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id NSData)
challenge asPublicKeyCredentialClientData =
  sendMessage asPublicKeyCredentialClientData challengeSelector

-- | The challenge to be signed during the operation.
--
-- ObjC selector: @- setChallenge:@
setChallenge :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSData value) => asPublicKeyCredentialClientData -> value -> IO ()
setChallenge asPublicKeyCredentialClientData value =
  sendMessage asPublicKeyCredentialClientData setChallengeSelector (toNSData value)

-- | The origin for where the request was performed.
--
-- ObjC selector: @- origin@
origin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id NSString)
origin asPublicKeyCredentialClientData =
  sendMessage asPublicKeyCredentialClientData originSelector

-- | The origin for where the request was performed.
--
-- ObjC selector: @- setOrigin:@
setOrigin :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSString value) => asPublicKeyCredentialClientData -> value -> IO ()
setOrigin asPublicKeyCredentialClientData value =
  sendMessage asPublicKeyCredentialClientData setOriginSelector (toNSString value)

-- | The top-level origin, if applicable.
--
-- ObjC selector: @- topOrigin@
topOrigin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id NSString)
topOrigin asPublicKeyCredentialClientData =
  sendMessage asPublicKeyCredentialClientData topOriginSelector

-- | The top-level origin, if applicable.
--
-- ObjC selector: @- setTopOrigin:@
setTopOrigin :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSString value) => asPublicKeyCredentialClientData -> value -> IO ()
setTopOrigin asPublicKeyCredentialClientData value =
  sendMessage asPublicKeyCredentialClientData setTopOriginSelector (toNSString value)

-- | Indicates whether this is a cross-origin request, if applicable.
--
-- ObjC selector: @- crossOrigin@
crossOrigin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO ASPublicKeyCredentialClientDataCrossOriginValue
crossOrigin asPublicKeyCredentialClientData =
  sendMessage asPublicKeyCredentialClientData crossOriginSelector

-- | Indicates whether this is a cross-origin request, if applicable.
--
-- ObjC selector: @- setCrossOrigin:@
setCrossOrigin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> ASPublicKeyCredentialClientDataCrossOriginValue -> IO ()
setCrossOrigin asPublicKeyCredentialClientData value =
  sendMessage asPublicKeyCredentialClientData setCrossOriginSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASPublicKeyCredentialClientData)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPublicKeyCredentialClientData)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithChallenge:origin:@
initWithChallenge_originSelector :: Selector '[Id NSData, Id NSString] (Id ASPublicKeyCredentialClientData)
initWithChallenge_originSelector = mkSelector "initWithChallenge:origin:"

-- | @Selector@ for @challenge@
challengeSelector :: Selector '[] (Id NSData)
challengeSelector = mkSelector "challenge"

-- | @Selector@ for @setChallenge:@
setChallengeSelector :: Selector '[Id NSData] ()
setChallengeSelector = mkSelector "setChallenge:"

-- | @Selector@ for @origin@
originSelector :: Selector '[] (Id NSString)
originSelector = mkSelector "origin"

-- | @Selector@ for @setOrigin:@
setOriginSelector :: Selector '[Id NSString] ()
setOriginSelector = mkSelector "setOrigin:"

-- | @Selector@ for @topOrigin@
topOriginSelector :: Selector '[] (Id NSString)
topOriginSelector = mkSelector "topOrigin"

-- | @Selector@ for @setTopOrigin:@
setTopOriginSelector :: Selector '[Id NSString] ()
setTopOriginSelector = mkSelector "setTopOrigin:"

-- | @Selector@ for @crossOrigin@
crossOriginSelector :: Selector '[] ASPublicKeyCredentialClientDataCrossOriginValue
crossOriginSelector = mkSelector "crossOrigin"

-- | @Selector@ for @setCrossOrigin:@
setCrossOriginSelector :: Selector '[ASPublicKeyCredentialClientDataCrossOriginValue] ()
setCrossOriginSelector = mkSelector "setCrossOrigin:"

