{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterErrorStateStruct@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterErrorStateStruct
  ( MTROvenCavityOperationalStateClusterErrorStateStruct
  , IsMTROvenCavityOperationalStateClusterErrorStateStruct(..)
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
errorStateID :: IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct => mtrOvenCavityOperationalStateClusterErrorStateStruct -> IO (Id NSNumber)
errorStateID mtrOvenCavityOperationalStateClusterErrorStateStruct =
  sendMessage mtrOvenCavityOperationalStateClusterErrorStateStruct errorStateIDSelector

-- | @- setErrorStateID:@
setErrorStateID :: (IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct, IsNSNumber value) => mtrOvenCavityOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateID mtrOvenCavityOperationalStateClusterErrorStateStruct value =
  sendMessage mtrOvenCavityOperationalStateClusterErrorStateStruct setErrorStateIDSelector (toNSNumber value)

-- | @- errorStateLabel@
errorStateLabel :: IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct => mtrOvenCavityOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateLabel mtrOvenCavityOperationalStateClusterErrorStateStruct =
  sendMessage mtrOvenCavityOperationalStateClusterErrorStateStruct errorStateLabelSelector

-- | @- setErrorStateLabel:@
setErrorStateLabel :: (IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOvenCavityOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateLabel mtrOvenCavityOperationalStateClusterErrorStateStruct value =
  sendMessage mtrOvenCavityOperationalStateClusterErrorStateStruct setErrorStateLabelSelector (toNSString value)

-- | @- errorStateDetails@
errorStateDetails :: IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct => mtrOvenCavityOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateDetails mtrOvenCavityOperationalStateClusterErrorStateStruct =
  sendMessage mtrOvenCavityOperationalStateClusterErrorStateStruct errorStateDetailsSelector

-- | @- setErrorStateDetails:@
setErrorStateDetails :: (IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOvenCavityOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateDetails mtrOvenCavityOperationalStateClusterErrorStateStruct value =
  sendMessage mtrOvenCavityOperationalStateClusterErrorStateStruct setErrorStateDetailsSelector (toNSString value)

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

