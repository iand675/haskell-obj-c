{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchDeleteRequest@.
module ObjC.CoreData.NSBatchDeleteRequest
  ( NSBatchDeleteRequest
  , IsNSBatchDeleteRequest(..)
  , init_
  , initWithFetchRequest
  , initWithObjectIDs
  , resultType
  , setResultType
  , fetchRequest
  , initSelector
  , initWithFetchRequestSelector
  , initWithObjectIDsSelector
  , resultTypeSelector
  , setResultTypeSelector
  , fetchRequestSelector

  -- * Enum types
  , NSBatchDeleteRequestResultType(NSBatchDeleteRequestResultType)
  , pattern NSBatchDeleteResultTypeStatusOnly
  , pattern NSBatchDeleteResultTypeObjectIDs
  , pattern NSBatchDeleteResultTypeCount

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> IO (Id NSBatchDeleteRequest)
init_ nsBatchDeleteRequest  =
  sendMsg nsBatchDeleteRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithFetchRequest:@
initWithFetchRequest :: (IsNSBatchDeleteRequest nsBatchDeleteRequest, IsNSFetchRequest fetch) => nsBatchDeleteRequest -> fetch -> IO (Id NSBatchDeleteRequest)
initWithFetchRequest nsBatchDeleteRequest  fetch =
withObjCPtr fetch $ \raw_fetch ->
    sendMsg nsBatchDeleteRequest (mkSelector "initWithFetchRequest:") (retPtr retVoid) [argPtr (castPtr raw_fetch :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithObjectIDs:@
initWithObjectIDs :: (IsNSBatchDeleteRequest nsBatchDeleteRequest, IsNSArray objects) => nsBatchDeleteRequest -> objects -> IO (Id NSBatchDeleteRequest)
initWithObjectIDs nsBatchDeleteRequest  objects =
withObjCPtr objects $ \raw_objects ->
    sendMsg nsBatchDeleteRequest (mkSelector "initWithObjectIDs:") (retPtr retVoid) [argPtr (castPtr raw_objects :: Ptr ())] >>= ownedObject . castPtr

-- | @- resultType@
resultType :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> IO NSBatchDeleteRequestResultType
resultType nsBatchDeleteRequest  =
  fmap (coerce :: CULong -> NSBatchDeleteRequestResultType) $ sendMsg nsBatchDeleteRequest (mkSelector "resultType") retCULong []

-- | @- setResultType:@
setResultType :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> NSBatchDeleteRequestResultType -> IO ()
setResultType nsBatchDeleteRequest  value =
  sendMsg nsBatchDeleteRequest (mkSelector "setResultType:") retVoid [argCULong (coerce value)]

-- | @- fetchRequest@
fetchRequest :: IsNSBatchDeleteRequest nsBatchDeleteRequest => nsBatchDeleteRequest -> IO (Id NSFetchRequest)
fetchRequest nsBatchDeleteRequest  =
  sendMsg nsBatchDeleteRequest (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFetchRequest:@
initWithFetchRequestSelector :: Selector
initWithFetchRequestSelector = mkSelector "initWithFetchRequest:"

-- | @Selector@ for @initWithObjectIDs:@
initWithObjectIDsSelector :: Selector
initWithObjectIDsSelector = mkSelector "initWithObjectIDs:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

