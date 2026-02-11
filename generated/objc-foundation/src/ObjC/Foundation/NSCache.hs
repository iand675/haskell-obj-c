{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCache@.
module ObjC.Foundation.NSCache
  ( NSCache
  , IsNSCache(..)
  , objectForKey
  , setObject_forKey
  , setObject_forKey_cost
  , removeObjectForKey
  , removeAllObjects
  , name
  , setName
  , totalCostLimit
  , setTotalCostLimit
  , countLimit
  , setCountLimit
  , evictsObjectsWithDiscardedContent
  , setEvictsObjectsWithDiscardedContent
  , objectForKeySelector
  , setObject_forKeySelector
  , setObject_forKey_costSelector
  , removeObjectForKeySelector
  , removeAllObjectsSelector
  , nameSelector
  , setNameSelector
  , totalCostLimitSelector
  , setTotalCostLimitSelector
  , countLimitSelector
  , setCountLimitSelector
  , evictsObjectsWithDiscardedContentSelector
  , setEvictsObjectsWithDiscardedContentSelector


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

-- | @- objectForKey:@
objectForKey :: IsNSCache nsCache => nsCache -> RawId -> IO RawId
objectForKey nsCache  key =
  fmap (RawId . castPtr) $ sendMsg nsCache (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- setObject:forKey:@
setObject_forKey :: IsNSCache nsCache => nsCache -> RawId -> RawId -> IO ()
setObject_forKey nsCache  obj_ key =
  sendMsg nsCache (mkSelector "setObject:forKey:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- setObject:forKey:cost:@
setObject_forKey_cost :: IsNSCache nsCache => nsCache -> RawId -> RawId -> CULong -> IO ()
setObject_forKey_cost nsCache  obj_ key g =
  sendMsg nsCache (mkSelector "setObject:forKey:cost:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ()), argCULong (fromIntegral g)]

-- | @- removeObjectForKey:@
removeObjectForKey :: IsNSCache nsCache => nsCache -> RawId -> IO ()
removeObjectForKey nsCache  key =
  sendMsg nsCache (mkSelector "removeObjectForKey:") retVoid [argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- removeAllObjects@
removeAllObjects :: IsNSCache nsCache => nsCache -> IO ()
removeAllObjects nsCache  =
  sendMsg nsCache (mkSelector "removeAllObjects") retVoid []

-- | @- name@
name :: IsNSCache nsCache => nsCache -> IO (Id NSString)
name nsCache  =
  sendMsg nsCache (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSCache nsCache, IsNSString value) => nsCache -> value -> IO ()
setName nsCache  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCache (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalCostLimit@
totalCostLimit :: IsNSCache nsCache => nsCache -> IO CULong
totalCostLimit nsCache  =
  sendMsg nsCache (mkSelector "totalCostLimit") retCULong []

-- | @- setTotalCostLimit:@
setTotalCostLimit :: IsNSCache nsCache => nsCache -> CULong -> IO ()
setTotalCostLimit nsCache  value =
  sendMsg nsCache (mkSelector "setTotalCostLimit:") retVoid [argCULong (fromIntegral value)]

-- | @- countLimit@
countLimit :: IsNSCache nsCache => nsCache -> IO CULong
countLimit nsCache  =
  sendMsg nsCache (mkSelector "countLimit") retCULong []

-- | @- setCountLimit:@
setCountLimit :: IsNSCache nsCache => nsCache -> CULong -> IO ()
setCountLimit nsCache  value =
  sendMsg nsCache (mkSelector "setCountLimit:") retVoid [argCULong (fromIntegral value)]

-- | @- evictsObjectsWithDiscardedContent@
evictsObjectsWithDiscardedContent :: IsNSCache nsCache => nsCache -> IO Bool
evictsObjectsWithDiscardedContent nsCache  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCache (mkSelector "evictsObjectsWithDiscardedContent") retCULong []

-- | @- setEvictsObjectsWithDiscardedContent:@
setEvictsObjectsWithDiscardedContent :: IsNSCache nsCache => nsCache -> Bool -> IO ()
setEvictsObjectsWithDiscardedContent nsCache  value =
  sendMsg nsCache (mkSelector "setEvictsObjectsWithDiscardedContent:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @setObject:forKey:cost:@
setObject_forKey_costSelector :: Selector
setObject_forKey_costSelector = mkSelector "setObject:forKey:cost:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @removeAllObjects@
removeAllObjectsSelector :: Selector
removeAllObjectsSelector = mkSelector "removeAllObjects"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @totalCostLimit@
totalCostLimitSelector :: Selector
totalCostLimitSelector = mkSelector "totalCostLimit"

-- | @Selector@ for @setTotalCostLimit:@
setTotalCostLimitSelector :: Selector
setTotalCostLimitSelector = mkSelector "setTotalCostLimit:"

-- | @Selector@ for @countLimit@
countLimitSelector :: Selector
countLimitSelector = mkSelector "countLimit"

-- | @Selector@ for @setCountLimit:@
setCountLimitSelector :: Selector
setCountLimitSelector = mkSelector "setCountLimit:"

-- | @Selector@ for @evictsObjectsWithDiscardedContent@
evictsObjectsWithDiscardedContentSelector :: Selector
evictsObjectsWithDiscardedContentSelector = mkSelector "evictsObjectsWithDiscardedContent"

-- | @Selector@ for @setEvictsObjectsWithDiscardedContent:@
setEvictsObjectsWithDiscardedContentSelector :: Selector
setEvictsObjectsWithDiscardedContentSelector = mkSelector "setEvictsObjectsWithDiscardedContent:"

