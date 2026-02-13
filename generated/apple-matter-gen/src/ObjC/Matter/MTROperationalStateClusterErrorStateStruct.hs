{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterErrorStateStruct@.
module ObjC.Matter.MTROperationalStateClusterErrorStateStruct
  ( MTROperationalStateClusterErrorStateStruct
  , IsMTROperationalStateClusterErrorStateStruct(..)
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
errorStateID :: IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct => mtrOperationalStateClusterErrorStateStruct -> IO (Id NSNumber)
errorStateID mtrOperationalStateClusterErrorStateStruct =
  sendMessage mtrOperationalStateClusterErrorStateStruct errorStateIDSelector

-- | @- setErrorStateID:@
setErrorStateID :: (IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct, IsNSNumber value) => mtrOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateID mtrOperationalStateClusterErrorStateStruct value =
  sendMessage mtrOperationalStateClusterErrorStateStruct setErrorStateIDSelector (toNSNumber value)

-- | @- errorStateLabel@
errorStateLabel :: IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct => mtrOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateLabel mtrOperationalStateClusterErrorStateStruct =
  sendMessage mtrOperationalStateClusterErrorStateStruct errorStateLabelSelector

-- | @- setErrorStateLabel:@
setErrorStateLabel :: (IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateLabel mtrOperationalStateClusterErrorStateStruct value =
  sendMessage mtrOperationalStateClusterErrorStateStruct setErrorStateLabelSelector (toNSString value)

-- | @- errorStateDetails@
errorStateDetails :: IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct => mtrOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateDetails mtrOperationalStateClusterErrorStateStruct =
  sendMessage mtrOperationalStateClusterErrorStateStruct errorStateDetailsSelector

-- | @- setErrorStateDetails:@
setErrorStateDetails :: (IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateDetails mtrOperationalStateClusterErrorStateStruct value =
  sendMessage mtrOperationalStateClusterErrorStateStruct setErrorStateDetailsSelector (toNSString value)

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

