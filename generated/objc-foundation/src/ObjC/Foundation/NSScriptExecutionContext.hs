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
  , sharedScriptExecutionContextSelector
  , topLevelObjectSelector
  , setTopLevelObjectSelector
  , objectBeingTestedSelector
  , setObjectBeingTestedSelector
  , rangeContainerObjectSelector
  , setRangeContainerObjectSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sharedScriptExecutionContext@
sharedScriptExecutionContext :: IO (Id NSScriptExecutionContext)
sharedScriptExecutionContext  =
  do
    cls' <- getRequiredClass "NSScriptExecutionContext"
    sendClassMsg cls' (mkSelector "sharedScriptExecutionContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- topLevelObject@
topLevelObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> IO RawId
topLevelObject nsScriptExecutionContext  =
  fmap (RawId . castPtr) $ sendMsg nsScriptExecutionContext (mkSelector "topLevelObject") (retPtr retVoid) []

-- | @- setTopLevelObject:@
setTopLevelObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> RawId -> IO ()
setTopLevelObject nsScriptExecutionContext  value =
  sendMsg nsScriptExecutionContext (mkSelector "setTopLevelObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- objectBeingTested@
objectBeingTested :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> IO RawId
objectBeingTested nsScriptExecutionContext  =
  fmap (RawId . castPtr) $ sendMsg nsScriptExecutionContext (mkSelector "objectBeingTested") (retPtr retVoid) []

-- | @- setObjectBeingTested:@
setObjectBeingTested :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> RawId -> IO ()
setObjectBeingTested nsScriptExecutionContext  value =
  sendMsg nsScriptExecutionContext (mkSelector "setObjectBeingTested:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- rangeContainerObject@
rangeContainerObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> IO RawId
rangeContainerObject nsScriptExecutionContext  =
  fmap (RawId . castPtr) $ sendMsg nsScriptExecutionContext (mkSelector "rangeContainerObject") (retPtr retVoid) []

-- | @- setRangeContainerObject:@
setRangeContainerObject :: IsNSScriptExecutionContext nsScriptExecutionContext => nsScriptExecutionContext -> RawId -> IO ()
setRangeContainerObject nsScriptExecutionContext  value =
  sendMsg nsScriptExecutionContext (mkSelector "setRangeContainerObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedScriptExecutionContext@
sharedScriptExecutionContextSelector :: Selector
sharedScriptExecutionContextSelector = mkSelector "sharedScriptExecutionContext"

-- | @Selector@ for @topLevelObject@
topLevelObjectSelector :: Selector
topLevelObjectSelector = mkSelector "topLevelObject"

-- | @Selector@ for @setTopLevelObject:@
setTopLevelObjectSelector :: Selector
setTopLevelObjectSelector = mkSelector "setTopLevelObject:"

-- | @Selector@ for @objectBeingTested@
objectBeingTestedSelector :: Selector
objectBeingTestedSelector = mkSelector "objectBeingTested"

-- | @Selector@ for @setObjectBeingTested:@
setObjectBeingTestedSelector :: Selector
setObjectBeingTestedSelector = mkSelector "setObjectBeingTested:"

-- | @Selector@ for @rangeContainerObject@
rangeContainerObjectSelector :: Selector
rangeContainerObjectSelector = mkSelector "rangeContainerObject"

-- | @Selector@ for @setRangeContainerObject:@
setRangeContainerObjectSelector :: Selector
setRangeContainerObjectSelector = mkSelector "setRangeContainerObject:"

