{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , initWithChallenge_originSelector
  , challengeSelector
  , setChallengeSelector
  , originSelector
  , setOriginSelector
  , topOriginSelector
  , setTopOriginSelector
  , crossOriginSelector
  , setCrossOriginSelector

  -- * Enum types
  , ASPublicKeyCredentialClientDataCrossOriginValue(ASPublicKeyCredentialClientDataCrossOriginValue)
  , pattern ASPublicKeyCredentialClientDataCrossOriginValueNotSet
  , pattern ASPublicKeyCredentialClientDataCrossOriginValueCrossOrigin
  , pattern ASPublicKeyCredentialClientDataCrossOriginValueSameOriginWithAncestors

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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASPublicKeyCredentialClientData)
new  =
  do
    cls' <- getRequiredClass "ASPublicKeyCredentialClientData"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id ASPublicKeyCredentialClientData)
init_ asPublicKeyCredentialClientData  =
  sendMsg asPublicKeyCredentialClientData (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithChallenge:origin:@
initWithChallenge_origin :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSData challenge, IsNSString origin) => asPublicKeyCredentialClientData -> challenge -> origin -> IO (Id ASPublicKeyCredentialClientData)
initWithChallenge_origin asPublicKeyCredentialClientData  challenge origin =
withObjCPtr challenge $ \raw_challenge ->
  withObjCPtr origin $ \raw_origin ->
      sendMsg asPublicKeyCredentialClientData (mkSelector "initWithChallenge:origin:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr raw_origin :: Ptr ())] >>= ownedObject . castPtr

-- | The challenge to be signed during the operation.
--
-- ObjC selector: @- challenge@
challenge :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id NSData)
challenge asPublicKeyCredentialClientData  =
  sendMsg asPublicKeyCredentialClientData (mkSelector "challenge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The challenge to be signed during the operation.
--
-- ObjC selector: @- setChallenge:@
setChallenge :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSData value) => asPublicKeyCredentialClientData -> value -> IO ()
setChallenge asPublicKeyCredentialClientData  value =
withObjCPtr value $ \raw_value ->
    sendMsg asPublicKeyCredentialClientData (mkSelector "setChallenge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The origin for where the request was performed.
--
-- ObjC selector: @- origin@
origin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id NSString)
origin asPublicKeyCredentialClientData  =
  sendMsg asPublicKeyCredentialClientData (mkSelector "origin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The origin for where the request was performed.
--
-- ObjC selector: @- setOrigin:@
setOrigin :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSString value) => asPublicKeyCredentialClientData -> value -> IO ()
setOrigin asPublicKeyCredentialClientData  value =
withObjCPtr value $ \raw_value ->
    sendMsg asPublicKeyCredentialClientData (mkSelector "setOrigin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The top-level origin, if applicable.
--
-- ObjC selector: @- topOrigin@
topOrigin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO (Id NSString)
topOrigin asPublicKeyCredentialClientData  =
  sendMsg asPublicKeyCredentialClientData (mkSelector "topOrigin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The top-level origin, if applicable.
--
-- ObjC selector: @- setTopOrigin:@
setTopOrigin :: (IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData, IsNSString value) => asPublicKeyCredentialClientData -> value -> IO ()
setTopOrigin asPublicKeyCredentialClientData  value =
withObjCPtr value $ \raw_value ->
    sendMsg asPublicKeyCredentialClientData (mkSelector "setTopOrigin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether this is a cross-origin request, if applicable.
--
-- ObjC selector: @- crossOrigin@
crossOrigin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> IO ASPublicKeyCredentialClientDataCrossOriginValue
crossOrigin asPublicKeyCredentialClientData  =
  fmap (coerce :: CLong -> ASPublicKeyCredentialClientDataCrossOriginValue) $ sendMsg asPublicKeyCredentialClientData (mkSelector "crossOrigin") retCLong []

-- | Indicates whether this is a cross-origin request, if applicable.
--
-- ObjC selector: @- setCrossOrigin:@
setCrossOrigin :: IsASPublicKeyCredentialClientData asPublicKeyCredentialClientData => asPublicKeyCredentialClientData -> ASPublicKeyCredentialClientDataCrossOriginValue -> IO ()
setCrossOrigin asPublicKeyCredentialClientData  value =
  sendMsg asPublicKeyCredentialClientData (mkSelector "setCrossOrigin:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithChallenge:origin:@
initWithChallenge_originSelector :: Selector
initWithChallenge_originSelector = mkSelector "initWithChallenge:origin:"

-- | @Selector@ for @challenge@
challengeSelector :: Selector
challengeSelector = mkSelector "challenge"

-- | @Selector@ for @setChallenge:@
setChallengeSelector :: Selector
setChallengeSelector = mkSelector "setChallenge:"

-- | @Selector@ for @origin@
originSelector :: Selector
originSelector = mkSelector "origin"

-- | @Selector@ for @setOrigin:@
setOriginSelector :: Selector
setOriginSelector = mkSelector "setOrigin:"

-- | @Selector@ for @topOrigin@
topOriginSelector :: Selector
topOriginSelector = mkSelector "topOrigin"

-- | @Selector@ for @setTopOrigin:@
setTopOriginSelector :: Selector
setTopOriginSelector = mkSelector "setTopOrigin:"

-- | @Selector@ for @crossOrigin@
crossOriginSelector :: Selector
crossOriginSelector = mkSelector "crossOrigin"

-- | @Selector@ for @setCrossOrigin:@
setCrossOriginSelector :: Selector
setCrossOriginSelector = mkSelector "setCrossOrigin:"

