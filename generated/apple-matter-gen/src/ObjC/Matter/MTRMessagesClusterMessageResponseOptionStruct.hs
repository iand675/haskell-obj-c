{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageResponseOptionStruct@.
module ObjC.Matter.MTRMessagesClusterMessageResponseOptionStruct
  ( MTRMessagesClusterMessageResponseOptionStruct
  , IsMTRMessagesClusterMessageResponseOptionStruct(..)
  , messageResponseID
  , setMessageResponseID
  , label
  , setLabel
  , labelSelector
  , messageResponseIDSelector
  , setLabelSelector
  , setMessageResponseIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- messageResponseID@
messageResponseID :: IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct => mtrMessagesClusterMessageResponseOptionStruct -> IO (Id NSNumber)
messageResponseID mtrMessagesClusterMessageResponseOptionStruct =
  sendMessage mtrMessagesClusterMessageResponseOptionStruct messageResponseIDSelector

-- | @- setMessageResponseID:@
setMessageResponseID :: (IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct, IsNSNumber value) => mtrMessagesClusterMessageResponseOptionStruct -> value -> IO ()
setMessageResponseID mtrMessagesClusterMessageResponseOptionStruct value =
  sendMessage mtrMessagesClusterMessageResponseOptionStruct setMessageResponseIDSelector (toNSNumber value)

-- | @- label@
label :: IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct => mtrMessagesClusterMessageResponseOptionStruct -> IO (Id NSString)
label mtrMessagesClusterMessageResponseOptionStruct =
  sendMessage mtrMessagesClusterMessageResponseOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct, IsNSString value) => mtrMessagesClusterMessageResponseOptionStruct -> value -> IO ()
setLabel mtrMessagesClusterMessageResponseOptionStruct value =
  sendMessage mtrMessagesClusterMessageResponseOptionStruct setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageResponseID@
messageResponseIDSelector :: Selector '[] (Id NSNumber)
messageResponseIDSelector = mkSelector "messageResponseID"

-- | @Selector@ for @setMessageResponseID:@
setMessageResponseIDSelector :: Selector '[Id NSNumber] ()
setMessageResponseIDSelector = mkSelector "setMessageResponseID:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

