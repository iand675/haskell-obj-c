{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProxy@.
module ObjC.Foundation.NSProxy
  ( NSProxy
  , IsNSProxy(..)
  , alloc
  , allocWithZone
  , class_
  , forwardInvocation
  , methodSignatureForSelector
  , dealloc
  , finalize
  , respondsToSelector
  , allowsWeakReference
  , retainWeakReference
  , description
  , debugDescription
  , allocSelector
  , allocWithZoneSelector
  , classSelector
  , forwardInvocationSelector
  , methodSignatureForSelectorSelector
  , deallocSelector
  , finalizeSelector
  , respondsToSelectorSelector
  , allowsWeakReferenceSelector
  , retainWeakReferenceSelector
  , descriptionSelector
  , debugDescriptionSelector


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

-- | @+ alloc@
alloc :: IO RawId
alloc  =
  do
    cls' <- getRequiredClass "NSProxy"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "alloc") (retPtr retVoid) []

-- | @+ allocWithZone:@
allocWithZone :: Ptr () -> IO RawId
allocWithZone zone =
  do
    cls' <- getRequiredClass "NSProxy"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "allocWithZone:") (retPtr retVoid) [argPtr zone]

-- | @+ class@
class_ :: IO Class
class_  =
  do
    cls' <- getRequiredClass "NSProxy"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "class") (retPtr retVoid) []

-- | @- forwardInvocation:@
forwardInvocation :: (IsNSProxy nsProxy, IsNSInvocation invocation) => nsProxy -> invocation -> IO ()
forwardInvocation nsProxy  invocation =
withObjCPtr invocation $ \raw_invocation ->
    sendMsg nsProxy (mkSelector "forwardInvocation:") retVoid [argPtr (castPtr raw_invocation :: Ptr ())]

-- | @- methodSignatureForSelector:@
methodSignatureForSelector :: IsNSProxy nsProxy => nsProxy -> Selector -> IO (Id NSMethodSignature)
methodSignatureForSelector nsProxy  sel =
  sendMsg nsProxy (mkSelector "methodSignatureForSelector:") (retPtr retVoid) [argPtr (unSelector sel)] >>= retainedObject . castPtr

-- | @- dealloc@
dealloc :: IsNSProxy nsProxy => nsProxy -> IO ()
dealloc nsProxy  =
  sendMsg nsProxy (mkSelector "dealloc") retVoid []

-- | @- finalize@
finalize :: IsNSProxy nsProxy => nsProxy -> IO ()
finalize nsProxy  =
  sendMsg nsProxy (mkSelector "finalize") retVoid []

-- | @+ respondsToSelector:@
respondsToSelector :: Selector -> IO Bool
respondsToSelector aSelector =
  do
    cls' <- getRequiredClass "NSProxy"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "respondsToSelector:") retCULong [argPtr (unSelector aSelector)]

-- | @- allowsWeakReference@
allowsWeakReference :: IsNSProxy nsProxy => nsProxy -> IO Bool
allowsWeakReference nsProxy  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProxy (mkSelector "allowsWeakReference") retCULong []

-- | @- retainWeakReference@
retainWeakReference :: IsNSProxy nsProxy => nsProxy -> IO Bool
retainWeakReference nsProxy  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProxy (mkSelector "retainWeakReference") retCULong []

-- | @- description@
description :: IsNSProxy nsProxy => nsProxy -> IO (Id NSString)
description nsProxy  =
  sendMsg nsProxy (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- debugDescription@
debugDescription :: IsNSProxy nsProxy => nsProxy -> IO (Id NSString)
debugDescription nsProxy  =
  sendMsg nsProxy (mkSelector "debugDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alloc@
allocSelector :: Selector
allocSelector = mkSelector "alloc"

-- | @Selector@ for @allocWithZone:@
allocWithZoneSelector :: Selector
allocWithZoneSelector = mkSelector "allocWithZone:"

-- | @Selector@ for @class@
classSelector :: Selector
classSelector = mkSelector "class"

-- | @Selector@ for @forwardInvocation:@
forwardInvocationSelector :: Selector
forwardInvocationSelector = mkSelector "forwardInvocation:"

-- | @Selector@ for @methodSignatureForSelector:@
methodSignatureForSelectorSelector :: Selector
methodSignatureForSelectorSelector = mkSelector "methodSignatureForSelector:"

-- | @Selector@ for @dealloc@
deallocSelector :: Selector
deallocSelector = mkSelector "dealloc"

-- | @Selector@ for @finalize@
finalizeSelector :: Selector
finalizeSelector = mkSelector "finalize"

-- | @Selector@ for @respondsToSelector:@
respondsToSelectorSelector :: Selector
respondsToSelectorSelector = mkSelector "respondsToSelector:"

-- | @Selector@ for @allowsWeakReference@
allowsWeakReferenceSelector :: Selector
allowsWeakReferenceSelector = mkSelector "allowsWeakReference"

-- | @Selector@ for @retainWeakReference@
retainWeakReferenceSelector :: Selector
retainWeakReferenceSelector = mkSelector "retainWeakReference"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @debugDescription@
debugDescriptionSelector :: Selector
debugDescriptionSelector = mkSelector "debugDescription"

