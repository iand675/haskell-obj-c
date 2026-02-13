{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterLineupInfo@.
module ObjC.Matter.MTRChannelClusterLineupInfo
  ( MTRChannelClusterLineupInfo
  , IsMTRChannelClusterLineupInfo(..)
  , operatorName
  , setOperatorName
  , lineupName
  , setLineupName
  , postalCode
  , setPostalCode
  , lineupInfoType
  , setLineupInfoType
  , lineupInfoTypeSelector
  , lineupNameSelector
  , operatorNameSelector
  , postalCodeSelector
  , setLineupInfoTypeSelector
  , setLineupNameSelector
  , setOperatorNameSelector
  , setPostalCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- operatorName@
operatorName :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSString)
operatorName mtrChannelClusterLineupInfo =
  sendMessage mtrChannelClusterLineupInfo operatorNameSelector

-- | @- setOperatorName:@
setOperatorName :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSString value) => mtrChannelClusterLineupInfo -> value -> IO ()
setOperatorName mtrChannelClusterLineupInfo value =
  sendMessage mtrChannelClusterLineupInfo setOperatorNameSelector (toNSString value)

-- | @- lineupName@
lineupName :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSString)
lineupName mtrChannelClusterLineupInfo =
  sendMessage mtrChannelClusterLineupInfo lineupNameSelector

-- | @- setLineupName:@
setLineupName :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSString value) => mtrChannelClusterLineupInfo -> value -> IO ()
setLineupName mtrChannelClusterLineupInfo value =
  sendMessage mtrChannelClusterLineupInfo setLineupNameSelector (toNSString value)

-- | @- postalCode@
postalCode :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSString)
postalCode mtrChannelClusterLineupInfo =
  sendMessage mtrChannelClusterLineupInfo postalCodeSelector

-- | @- setPostalCode:@
setPostalCode :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSString value) => mtrChannelClusterLineupInfo -> value -> IO ()
setPostalCode mtrChannelClusterLineupInfo value =
  sendMessage mtrChannelClusterLineupInfo setPostalCodeSelector (toNSString value)

-- | @- lineupInfoType@
lineupInfoType :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSNumber)
lineupInfoType mtrChannelClusterLineupInfo =
  sendMessage mtrChannelClusterLineupInfo lineupInfoTypeSelector

-- | @- setLineupInfoType:@
setLineupInfoType :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSNumber value) => mtrChannelClusterLineupInfo -> value -> IO ()
setLineupInfoType mtrChannelClusterLineupInfo value =
  sendMessage mtrChannelClusterLineupInfo setLineupInfoTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operatorName@
operatorNameSelector :: Selector '[] (Id NSString)
operatorNameSelector = mkSelector "operatorName"

-- | @Selector@ for @setOperatorName:@
setOperatorNameSelector :: Selector '[Id NSString] ()
setOperatorNameSelector = mkSelector "setOperatorName:"

-- | @Selector@ for @lineupName@
lineupNameSelector :: Selector '[] (Id NSString)
lineupNameSelector = mkSelector "lineupName"

-- | @Selector@ for @setLineupName:@
setLineupNameSelector :: Selector '[Id NSString] ()
setLineupNameSelector = mkSelector "setLineupName:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector '[Id NSString] ()
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @lineupInfoType@
lineupInfoTypeSelector :: Selector '[] (Id NSNumber)
lineupInfoTypeSelector = mkSelector "lineupInfoType"

-- | @Selector@ for @setLineupInfoType:@
setLineupInfoTypeSelector :: Selector '[Id NSNumber] ()
setLineupInfoTypeSelector = mkSelector "setLineupInfoType:"

