{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChannelInfoStruct@.
module ObjC.Matter.MTRChannelClusterChannelInfoStruct
  ( MTRChannelClusterChannelInfoStruct
  , IsMTRChannelClusterChannelInfoStruct(..)
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
  , identifier
  , setIdentifier
  , type_
  , setType
  , affiliateCallSignSelector
  , callSignSelector
  , identifierSelector
  , majorNumberSelector
  , minorNumberSelector
  , nameSelector
  , setAffiliateCallSignSelector
  , setCallSignSelector
  , setIdentifierSelector
  , setMajorNumberSelector
  , setMinorNumberSelector
  , setNameSelector
  , setTypeSelector
  , typeSelector


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
majorNumber :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSNumber)
majorNumber mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct majorNumberSelector

-- | @- setMajorNumber:@
setMajorNumber :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSNumber value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setMajorNumber mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setMajorNumberSelector (toNSNumber value)

-- | @- minorNumber@
minorNumber :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSNumber)
minorNumber mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct minorNumberSelector

-- | @- setMinorNumber:@
setMinorNumber :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSNumber value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setMinorNumber mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setMinorNumberSelector (toNSNumber value)

-- | @- name@
name :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
name mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct nameSelector

-- | @- setName:@
setName :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setName mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setNameSelector (toNSString value)

-- | @- callSign@
callSign :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
callSign mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct callSignSelector

-- | @- setCallSign:@
setCallSign :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setCallSign mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setCallSignSelector (toNSString value)

-- | @- affiliateCallSign@
affiliateCallSign :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
affiliateCallSign mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct affiliateCallSignSelector

-- | @- setAffiliateCallSign:@
setAffiliateCallSign :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setAffiliateCallSign mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setAffiliateCallSignSelector (toNSString value)

-- | @- identifier@
identifier :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
identifier mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setIdentifier mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setIdentifierSelector (toNSString value)

-- | @- type@
type_ :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSNumber)
type_ mtrChannelClusterChannelInfoStruct =
  sendMessage mtrChannelClusterChannelInfoStruct typeSelector

-- | @- setType:@
setType :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSNumber value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setType mtrChannelClusterChannelInfoStruct value =
  sendMessage mtrChannelClusterChannelInfoStruct setTypeSelector (toNSNumber value)

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

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSNumber)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSNumber] ()
setTypeSelector = mkSelector "setType:"

