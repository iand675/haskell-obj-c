{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSearchQuery@.
module ObjC.CoreSpotlight.CSSearchQuery
  ( CSSearchQuery
  , IsCSSearchQuery(..)
  , init_
  , initWithQueryString_queryContext
  , initWithQueryString_attributes
  , start
  , cancel
  , cancelled
  , foundItemCount
  , completionHandler
  , setCompletionHandler
  , protectionClasses
  , setProtectionClasses
  , cancelSelector
  , cancelledSelector
  , completionHandlerSelector
  , foundItemCountSelector
  , initSelector
  , initWithQueryString_attributesSelector
  , initWithQueryString_queryContextSelector
  , protectionClassesSelector
  , setCompletionHandlerSelector
  , setProtectionClassesSelector
  , startSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO (Id CSSearchQuery)
init_ csSearchQuery =
  sendOwnedMessage csSearchQuery initSelector

-- | @- initWithQueryString:queryContext:@
initWithQueryString_queryContext :: (IsCSSearchQuery csSearchQuery, IsNSString queryString, IsCSSearchQueryContext queryContext) => csSearchQuery -> queryString -> queryContext -> IO (Id CSSearchQuery)
initWithQueryString_queryContext csSearchQuery queryString queryContext =
  sendOwnedMessage csSearchQuery initWithQueryString_queryContextSelector (toNSString queryString) (toCSSearchQueryContext queryContext)

-- | @- initWithQueryString:attributes:@
initWithQueryString_attributes :: (IsCSSearchQuery csSearchQuery, IsNSString queryString, IsNSArray attributes) => csSearchQuery -> queryString -> attributes -> IO (Id CSSearchQuery)
initWithQueryString_attributes csSearchQuery queryString attributes =
  sendOwnedMessage csSearchQuery initWithQueryString_attributesSelector (toNSString queryString) (toNSArray attributes)

-- | @- start@
start :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO ()
start csSearchQuery =
  sendMessage csSearchQuery startSelector

-- | @- cancel@
cancel :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO ()
cancel csSearchQuery =
  sendMessage csSearchQuery cancelSelector

-- | @- cancelled@
cancelled :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO Bool
cancelled csSearchQuery =
  sendMessage csSearchQuery cancelledSelector

-- | @- foundItemCount@
foundItemCount :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO CULong
foundItemCount csSearchQuery =
  sendMessage csSearchQuery foundItemCountSelector

-- | @- completionHandler@
completionHandler :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO (Ptr ())
completionHandler csSearchQuery =
  sendMessage csSearchQuery completionHandlerSelector

-- | @- setCompletionHandler:@
setCompletionHandler :: IsCSSearchQuery csSearchQuery => csSearchQuery -> Ptr () -> IO ()
setCompletionHandler csSearchQuery value =
  sendMessage csSearchQuery setCompletionHandlerSelector value

-- | @- protectionClasses@
protectionClasses :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO (Id NSArray)
protectionClasses csSearchQuery =
  sendMessage csSearchQuery protectionClassesSelector

-- | @- setProtectionClasses:@
setProtectionClasses :: (IsCSSearchQuery csSearchQuery, IsNSArray value) => csSearchQuery -> value -> IO ()
setProtectionClasses csSearchQuery value =
  sendMessage csSearchQuery setProtectionClassesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CSSearchQuery)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithQueryString:queryContext:@
initWithQueryString_queryContextSelector :: Selector '[Id NSString, Id CSSearchQueryContext] (Id CSSearchQuery)
initWithQueryString_queryContextSelector = mkSelector "initWithQueryString:queryContext:"

-- | @Selector@ for @initWithQueryString:attributes:@
initWithQueryString_attributesSelector :: Selector '[Id NSString, Id NSArray] (Id CSSearchQuery)
initWithQueryString_attributesSelector = mkSelector "initWithQueryString:attributes:"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @foundItemCount@
foundItemCountSelector :: Selector '[] CULong
foundItemCountSelector = mkSelector "foundItemCount"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector '[] (Ptr ())
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector '[Ptr ()] ()
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

-- | @Selector@ for @protectionClasses@
protectionClassesSelector :: Selector '[] (Id NSArray)
protectionClassesSelector = mkSelector "protectionClasses"

-- | @Selector@ for @setProtectionClasses:@
setProtectionClassesSelector :: Selector '[Id NSArray] ()
setProtectionClassesSelector = mkSelector "setProtectionClasses:"

