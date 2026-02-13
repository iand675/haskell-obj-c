{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChannelInfo@.
module ObjC.Matter.MTRChannelClusterChannelInfo
  ( MTRChannelClusterChannelInfo
  , IsMTRChannelClusterChannelInfo(..)
  , majorNumber
  , setMajorNumber
  , minorNumber
  , setMinorNumber
  , name
  , setName
  , callSign
  , setCallSign
  , affiliateCallSign
  , setAffiliateCallSign
  , affiliateCallSignSelector
  , callSignSelector
  , majorNumberSelector
  , minorNumberSelector
  , nameSelector
  , setAffiliateCallSignSelector
  , setCallSignSelector
  , setMajorNumberSelector
  , setMinorNumberSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- majorNumber@
majorNumber :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSNumber)
majorNumber mtrChannelClusterChannelInfo =
  sendMessage mtrChannelClusterChannelInfo majorNumberSelector

-- | @- setMajorNumber:@
setMajorNumber :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSNumber value) => mtrChannelClusterChannelInfo -> value -> IO ()
setMajorNumber mtrChannelClusterChannelInfo value =
  sendMessage mtrChannelClusterChannelInfo setMajorNumberSelector (toNSNumber value)

-- | @- minorNumber@
minorNumber :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSNumber)
minorNumber mtrChannelClusterChannelInfo =
  sendMessage mtrChannelClusterChannelInfo minorNumberSelector

-- | @- setMinorNumber:@
setMinorNumber :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSNumber value) => mtrChannelClusterChannelInfo -> value -> IO ()
setMinorNumber mtrChannelClusterChannelInfo value =
  sendMessage mtrChannelClusterChannelInfo setMinorNumberSelector (toNSNumber value)

-- | @- name@
name :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSString)
name mtrChannelClusterChannelInfo =
  sendMessage mtrChannelClusterChannelInfo nameSelector

-- | @- setName:@
setName :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSString value) => mtrChannelClusterChannelInfo -> value -> IO ()
setName mtrChannelClusterChannelInfo value =
  sendMessage mtrChannelClusterChannelInfo setNameSelector (toNSString value)

-- | @- callSign@
callSign :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSString)
callSign mtrChannelClusterChannelInfo =
  sendMessage mtrChannelClusterChannelInfo callSignSelector

-- | @- setCallSign:@
setCallSign :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSString value) => mtrChannelClusterChannelInfo -> value -> IO ()
setCallSign mtrChannelClusterChannelInfo value =
  sendMessage mtrChannelClusterChannelInfo setCallSignSelector (toNSString value)

-- | @- affiliateCallSign@
affiliateCallSign :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSString)
affiliateCallSign mtrChannelClusterChannelInfo =
  sendMessage mtrChannelClusterChannelInfo affiliateCallSignSelector

-- | @- setAffiliateCallSign:@
setAffiliateCallSign :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSString value) => mtrChannelClusterChannelInfo -> value -> IO ()
setAffiliateCallSign mtrChannelClusterChannelInfo value =
  sendMessage mtrChannelClusterChannelInfo setAffiliateCallSignSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @majorNumber@
majorNumberSelector :: Selector '[] (Id NSNumber)
majorNumberSelector = mkSelector "majorNumber"

-- | @Selector@ for @setMajorNumber:@
setMajorNumberSelector :: Selector '[Id NSNumber] ()
setMajorNumberSelector = mkSelector "setMajorNumber:"

-- | @Selector@ for @minorNumber@
minorNumberSelector :: Selector '[] (Id NSNumber)
minorNumberSelector = mkSelector "minorNumber"

-- | @Selector@ for @setMinorNumber:@
setMinorNumberSelector :: Selector '[Id NSNumber] ()
setMinorNumberSelector = mkSelector "setMinorNumber:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @callSign@
callSignSelector :: Selector '[] (Id NSString)
callSignSelector = mkSelector "callSign"

-- | @Selector@ for @setCallSign:@
setCallSignSelector :: Selector '[Id NSString] ()
setCallSignSelector = mkSelector "setCallSign:"

-- | @Selector@ for @affiliateCallSign@
affiliateCallSignSelector :: Selector '[] (Id NSString)
affiliateCallSignSelector = mkSelector "affiliateCallSign"

-- | @Selector@ for @setAffiliateCallSign:@
setAffiliateCallSignSelector :: Selector '[Id NSString] ()
setAffiliateCallSignSelector = mkSelector "setAffiliateCallSign:"

