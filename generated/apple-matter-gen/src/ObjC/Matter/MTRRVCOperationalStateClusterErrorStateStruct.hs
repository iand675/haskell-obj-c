{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterErrorStateStruct@.
module ObjC.Matter.MTRRVCOperationalStateClusterErrorStateStruct
  ( MTRRVCOperationalStateClusterErrorStateStruct
  , IsMTRRVCOperationalStateClusterErrorStateStruct(..)
  , errorStateID
  , setErrorStateID
  , errorStateLabel
  , setErrorStateLabel
  , errorStateDetails
  , setErrorStateDetails
  , errorStateDetailsSelector
  , errorStateIDSelector
  , errorStateLabelSelector
  , setErrorStateDetailsSelector
  , setErrorStateIDSelector
  , setErrorStateLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- errorStateID@
errorStateID :: IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct => mtrrvcOperationalStateClusterErrorStateStruct -> IO (Id NSNumber)
errorStateID mtrrvcOperationalStateClusterErrorStateStruct =
  sendMessage mtrrvcOperationalStateClusterErrorStateStruct errorStateIDSelector

-- | @- setErrorStateID:@
setErrorStateID :: (IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct, IsNSNumber value) => mtrrvcOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateID mtrrvcOperationalStateClusterErrorStateStruct value =
  sendMessage mtrrvcOperationalStateClusterErrorStateStruct setErrorStateIDSelector (toNSNumber value)

-- | @- errorStateLabel@
errorStateLabel :: IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct => mtrrvcOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateLabel mtrrvcOperationalStateClusterErrorStateStruct =
  sendMessage mtrrvcOperationalStateClusterErrorStateStruct errorStateLabelSelector

-- | @- setErrorStateLabel:@
setErrorStateLabel :: (IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct, IsNSString value) => mtrrvcOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateLabel mtrrvcOperationalStateClusterErrorStateStruct value =
  sendMessage mtrrvcOperationalStateClusterErrorStateStruct setErrorStateLabelSelector (toNSString value)

-- | @- errorStateDetails@
errorStateDetails :: IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct => mtrrvcOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateDetails mtrrvcOperationalStateClusterErrorStateStruct =
  sendMessage mtrrvcOperationalStateClusterErrorStateStruct errorStateDetailsSelector

-- | @- setErrorStateDetails:@
setErrorStateDetails :: (IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct, IsNSString value) => mtrrvcOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateDetails mtrrvcOperationalStateClusterErrorStateStruct value =
  sendMessage mtrrvcOperationalStateClusterErrorStateStruct setErrorStateDetailsSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorStateID@
errorStateIDSelector :: Selector '[] (Id NSNumber)
errorStateIDSelector = mkSelector "errorStateID"

-- | @Selector@ for @setErrorStateID:@
setErrorStateIDSelector :: Selector '[Id NSNumber] ()
setErrorStateIDSelector = mkSelector "setErrorStateID:"

-- | @Selector@ for @errorStateLabel@
errorStateLabelSelector :: Selector '[] (Id NSString)
errorStateLabelSelector = mkSelector "errorStateLabel"

-- | @Selector@ for @setErrorStateLabel:@
setErrorStateLabelSelector :: Selector '[Id NSString] ()
setErrorStateLabelSelector = mkSelector "setErrorStateLabel:"

-- | @Selector@ for @errorStateDetails@
errorStateDetailsSelector :: Selector '[] (Id NSString)
errorStateDetailsSelector = mkSelector "errorStateDetails"

-- | @Selector@ for @setErrorStateDetails:@
setErrorStateDetailsSelector :: Selector '[Id NSString] ()
setErrorStateDetailsSelector = mkSelector "setErrorStateDetails:"

