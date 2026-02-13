{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterPageTokenStruct@.
module ObjC.Matter.MTRChannelClusterPageTokenStruct
  ( MTRChannelClusterPageTokenStruct
  , IsMTRChannelClusterPageTokenStruct(..)
  , limit
  , setLimit
  , after
  , setAfter
  , before
  , setBefore
  , afterSelector
  , beforeSelector
  , limitSelector
  , setAfterSelector
  , setBeforeSelector
  , setLimitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- limit@
limit :: IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct => mtrChannelClusterPageTokenStruct -> IO (Id NSNumber)
limit mtrChannelClusterPageTokenStruct =
  sendMessage mtrChannelClusterPageTokenStruct limitSelector

-- | @- setLimit:@
setLimit :: (IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct, IsNSNumber value) => mtrChannelClusterPageTokenStruct -> value -> IO ()
setLimit mtrChannelClusterPageTokenStruct value =
  sendMessage mtrChannelClusterPageTokenStruct setLimitSelector (toNSNumber value)

-- | @- after@
after :: IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct => mtrChannelClusterPageTokenStruct -> IO (Id NSString)
after mtrChannelClusterPageTokenStruct =
  sendMessage mtrChannelClusterPageTokenStruct afterSelector

-- | @- setAfter:@
setAfter :: (IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct, IsNSString value) => mtrChannelClusterPageTokenStruct -> value -> IO ()
setAfter mtrChannelClusterPageTokenStruct value =
  sendMessage mtrChannelClusterPageTokenStruct setAfterSelector (toNSString value)

-- | @- before@
before :: IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct => mtrChannelClusterPageTokenStruct -> IO (Id NSString)
before mtrChannelClusterPageTokenStruct =
  sendMessage mtrChannelClusterPageTokenStruct beforeSelector

-- | @- setBefore:@
setBefore :: (IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct, IsNSString value) => mtrChannelClusterPageTokenStruct -> value -> IO ()
setBefore mtrChannelClusterPageTokenStruct value =
  sendMessage mtrChannelClusterPageTokenStruct setBeforeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @limit@
limitSelector :: Selector '[] (Id NSNumber)
limitSelector = mkSelector "limit"

-- | @Selector@ for @setLimit:@
setLimitSelector :: Selector '[Id NSNumber] ()
setLimitSelector = mkSelector "setLimit:"

-- | @Selector@ for @after@
afterSelector :: Selector '[] (Id NSString)
afterSelector = mkSelector "after"

-- | @Selector@ for @setAfter:@
setAfterSelector :: Selector '[Id NSString] ()
setAfterSelector = mkSelector "setAfter:"

-- | @Selector@ for @before@
beforeSelector :: Selector '[] (Id NSString)
beforeSelector = mkSelector "before"

-- | @Selector@ for @setBefore:@
setBeforeSelector :: Selector '[Id NSString] ()
setBeforeSelector = mkSelector "setBefore:"

