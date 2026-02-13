{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptExecutionContext@.
module ObjC.Foundation.NSScriptExecutionContext
  ( NSScriptExecutionContext
  , IsNSScriptExecutionContext(..)
  , sharedScriptExecutionContext
  , topLevelObject
  , setTopLevelObject
  , objectBeingTested
  , setObjectBeingTested
  , rangeContainerObject
  , setRangeContainerObject
  , objectBeingTestedSelector
  , rangeContainerObjectSelector
  , setObjectBeingTestedSelector
  , setRangeContainerObjectSelector
  , setTopLevelObjectSelector
  , sharedScriptExecutionContextSelector
  , topLevelObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sharedScriptExecutionContext@
sharedScriptExecutionContext :: IO (Id NSScriptExecutionContext)
sharedScriptExecutionContext  =
  do
    cls' <- getRequiredClass "NSScriptExecutionContext"
    sendClassMessage cls' sharedScriptExecutionContextSelector

-- | @- topLevelObject@
topLevelObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> IO RawId
topLevelObject nsScriptExecutionContext =
  sendMessage nsScriptExecutionContext topLevelObjectSelector

-- | @- setTopLevelObject:@
setTopLevelObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> RawId -> IO ()
setTopLevelObject nsScriptExecutionContext value =
  sendMessage nsScriptExecutionContext setTopLevelObjectSelector value

-- | @- objectBeingTested@
objectBeingTested :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> IO RawId
objectBeingTested nsScriptExecutionContext =
  sendMessage nsScriptExecutionContext objectBeingTestedSelector

-- | @- setObjectBeingTested:@
setObjectBeingTested :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> RawId -> IO ()
setObjectBeingTested nsScriptExecutionContext value =
  sendMessage nsScriptExecutionContext setObjectBeingTestedSelector value

-- | @- rangeContainerObject@
rangeContainerObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> IO RawId
rangeContainerObject nsScriptExecutionContext =
  sendMessage nsScriptExecutionContext rangeContainerObjectSelector

-- | @- setRangeContainerObject:@
setRangeContainerObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> RawId -> IO ()
setRangeContainerObject nsScriptExecutionContext value =
  sendMessage nsScriptExecutionContext setRangeContainerObjectSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedScriptExecutionContext@
sharedScriptExecutionContextSelector :: Selector '[] (Id NSScriptExecutionContext)
sharedScriptExecutionContextSelector = mkSelector "sharedScriptExecutionContext"

-- | @Selector@ for @topLevelObject@
topLevelObjectSelector :: Selector '[] RawId
topLevelObjectSelector = mkSelector "topLevelObject"

-- | @Selector@ for @setTopLevelObject:@
setTopLevelObjectSelector :: Selector '[RawId] ()
setTopLevelObjectSelector = mkSelector "setTopLevelObject:"

-- | @Selector@ for @objectBeingTested@
objectBeingTestedSelector :: Selector '[] RawId
objectBeingTestedSelector = mkSelector "objectBeingTested"

-- | @Selector@ for @setObjectBeingTested:@
setObjectBeingTestedSelector :: Selector '[RawId] ()
setObjectBeingTestedSelector = mkSelector "setObjectBeingTested:"

-- | @Selector@ for @rangeContainerObject@
rangeContainerObjectSelector :: Selector '[] RawId
rangeContainerObjectSelector = mkSelector "rangeContainerObject"

-- | @Selector@ for @setRangeContainerObject:@
setRangeContainerObjectSelector :: Selector '[RawId] ()
setRangeContainerObjectSelector = mkSelector "setRangeContainerObject:"

