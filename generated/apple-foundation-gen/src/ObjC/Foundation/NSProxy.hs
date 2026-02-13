{-# LANGUAGE DataKinds #-}
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
  , allowsWeakReferenceSelector
  , classSelector
  , deallocSelector
  , debugDescriptionSelector
  , descriptionSelector
  , finalizeSelector
  , forwardInvocationSelector
  , methodSignatureForSelectorSelector
  , respondsToSelectorSelector
  , retainWeakReferenceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ alloc@
alloc :: IO RawId
alloc  =
  do
    cls' <- getRequiredClass "NSProxy"
    sendOwnedClassMessage cls' allocSelector

-- | @+ allocWithZone:@
allocWithZone :: Ptr () -> IO RawId
allocWithZone zone =
  do
    cls' <- getRequiredClass "NSProxy"
    sendOwnedClassMessage cls' allocWithZoneSelector zone

-- | @+ class@
class_ :: IO Class
class_  =
  do
    cls' <- getRequiredClass "NSProxy"
    sendClassMessage cls' classSelector

-- | @- forwardInvocation:@
forwardInvocation :: (IsNSProxy nsProxy, IsNSInvocation invocation) => nsProxy -> invocation -> IO ()
forwardInvocation nsProxy invocation =
  sendMessage nsProxy forwardInvocationSelector (toNSInvocation invocation)

-- | @- methodSignatureForSelector:@
methodSignatureForSelector :: IsNSProxy nsProxy => nsProxy -> Sel -> IO (Id NSMethodSignature)
methodSignatureForSelector nsProxy sel =
  sendMessage nsProxy methodSignatureForSelectorSelector sel

-- | @- dealloc@
dealloc :: IsNSProxy nsProxy => nsProxy -> IO ()
dealloc nsProxy =
  sendMessage nsProxy deallocSelector

-- | @- finalize@
finalize :: IsNSProxy nsProxy => nsProxy -> IO ()
finalize nsProxy =
  sendMessage nsProxy finalizeSelector

-- | @+ respondsToSelector:@
respondsToSelector :: Sel -> IO Bool
respondsToSelector aSelector =
  do
    cls' <- getRequiredClass "NSProxy"
    sendClassMessage cls' respondsToSelectorSelector aSelector

-- | @- allowsWeakReference@
allowsWeakReference :: IsNSProxy nsProxy => nsProxy -> IO Bool
allowsWeakReference nsProxy =
  sendMessage nsProxy allowsWeakReferenceSelector

-- | @- retainWeakReference@
retainWeakReference :: IsNSProxy nsProxy => nsProxy -> IO Bool
retainWeakReference nsProxy =
  sendMessage nsProxy retainWeakReferenceSelector

-- | @- description@
description :: IsNSProxy nsProxy => nsProxy -> IO (Id NSString)
description nsProxy =
  sendMessage nsProxy descriptionSelector

-- | @- debugDescription@
debugDescription :: IsNSProxy nsProxy => nsProxy -> IO (Id NSString)
debugDescription nsProxy =
  sendMessage nsProxy debugDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alloc@
allocSelector :: Selector '[] RawId
allocSelector = mkSelector "alloc"

-- | @Selector@ for @allocWithZone:@
allocWithZoneSelector :: Selector '[Ptr ()] RawId
allocWithZoneSelector = mkSelector "allocWithZone:"

-- | @Selector@ for @class@
classSelector :: Selector '[] Class
classSelector = mkSelector "class"

-- | @Selector@ for @forwardInvocation:@
forwardInvocationSelector :: Selector '[Id NSInvocation] ()
forwardInvocationSelector = mkSelector "forwardInvocation:"

-- | @Selector@ for @methodSignatureForSelector:@
methodSignatureForSelectorSelector :: Selector '[Sel] (Id NSMethodSignature)
methodSignatureForSelectorSelector = mkSelector "methodSignatureForSelector:"

-- | @Selector@ for @dealloc@
deallocSelector :: Selector '[] ()
deallocSelector = mkSelector "dealloc"

-- | @Selector@ for @finalize@
finalizeSelector :: Selector '[] ()
finalizeSelector = mkSelector "finalize"

-- | @Selector@ for @respondsToSelector:@
respondsToSelectorSelector :: Selector '[Sel] Bool
respondsToSelectorSelector = mkSelector "respondsToSelector:"

-- | @Selector@ for @allowsWeakReference@
allowsWeakReferenceSelector :: Selector '[] Bool
allowsWeakReferenceSelector = mkSelector "allowsWeakReference"

-- | @Selector@ for @retainWeakReference@
retainWeakReferenceSelector :: Selector '[] Bool
retainWeakReferenceSelector = mkSelector "retainWeakReference"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @debugDescription@
debugDescriptionSelector :: Selector '[] (Id NSString)
debugDescriptionSelector = mkSelector "debugDescription"

