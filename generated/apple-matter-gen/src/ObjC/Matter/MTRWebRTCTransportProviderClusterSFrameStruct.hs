{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterSFrameStruct@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterSFrameStruct
  ( MTRWebRTCTransportProviderClusterSFrameStruct
  , IsMTRWebRTCTransportProviderClusterSFrameStruct(..)
  , cipherSuite
  , setCipherSuite
  , baseKey
  , setBaseKey
  , kid
  , setKid
  , baseKeySelector
  , cipherSuiteSelector
  , kidSelector
  , setBaseKeySelector
  , setCipherSuiteSelector
  , setKidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cipherSuite@
cipherSuite :: IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct => mtrWebRTCTransportProviderClusterSFrameStruct -> IO (Id NSNumber)
cipherSuite mtrWebRTCTransportProviderClusterSFrameStruct =
  sendMessage mtrWebRTCTransportProviderClusterSFrameStruct cipherSuiteSelector

-- | @- setCipherSuite:@
setCipherSuite :: (IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct, IsNSNumber value) => mtrWebRTCTransportProviderClusterSFrameStruct -> value -> IO ()
setCipherSuite mtrWebRTCTransportProviderClusterSFrameStruct value =
  sendMessage mtrWebRTCTransportProviderClusterSFrameStruct setCipherSuiteSelector (toNSNumber value)

-- | @- baseKey@
baseKey :: IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct => mtrWebRTCTransportProviderClusterSFrameStruct -> IO (Id NSData)
baseKey mtrWebRTCTransportProviderClusterSFrameStruct =
  sendMessage mtrWebRTCTransportProviderClusterSFrameStruct baseKeySelector

-- | @- setBaseKey:@
setBaseKey :: (IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct, IsNSData value) => mtrWebRTCTransportProviderClusterSFrameStruct -> value -> IO ()
setBaseKey mtrWebRTCTransportProviderClusterSFrameStruct value =
  sendMessage mtrWebRTCTransportProviderClusterSFrameStruct setBaseKeySelector (toNSData value)

-- | @- kid@
kid :: IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct => mtrWebRTCTransportProviderClusterSFrameStruct -> IO (Id NSData)
kid mtrWebRTCTransportProviderClusterSFrameStruct =
  sendMessage mtrWebRTCTransportProviderClusterSFrameStruct kidSelector

-- | @- setKid:@
setKid :: (IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct, IsNSData value) => mtrWebRTCTransportProviderClusterSFrameStruct -> value -> IO ()
setKid mtrWebRTCTransportProviderClusterSFrameStruct value =
  sendMessage mtrWebRTCTransportProviderClusterSFrameStruct setKidSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cipherSuite@
cipherSuiteSelector :: Selector '[] (Id NSNumber)
cipherSuiteSelector = mkSelector "cipherSuite"

-- | @Selector@ for @setCipherSuite:@
setCipherSuiteSelector :: Selector '[Id NSNumber] ()
setCipherSuiteSelector = mkSelector "setCipherSuite:"

-- | @Selector@ for @baseKey@
baseKeySelector :: Selector '[] (Id NSData)
baseKeySelector = mkSelector "baseKey"

-- | @Selector@ for @setBaseKey:@
setBaseKeySelector :: Selector '[Id NSData] ()
setBaseKeySelector = mkSelector "setBaseKey:"

-- | @Selector@ for @kid@
kidSelector :: Selector '[] (Id NSData)
kidSelector = mkSelector "kid"

-- | @Selector@ for @setKid:@
setKidSelector :: Selector '[Id NSData] ()
setKidSelector = mkSelector "setKid:"

