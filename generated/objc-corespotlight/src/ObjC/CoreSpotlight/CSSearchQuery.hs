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
  , initSelector
  , initWithQueryString_queryContextSelector
  , initWithQueryString_attributesSelector
  , startSelector
  , cancelSelector
  , cancelledSelector
  , foundItemCountSelector
  , completionHandlerSelector
  , setCompletionHandlerSelector
  , protectionClassesSelector
  , setProtectionClassesSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO (Id CSSearchQuery)
init_ csSearchQuery  =
  sendMsg csSearchQuery (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithQueryString:queryContext:@
initWithQueryString_queryContext :: (IsCSSearchQuery csSearchQuery, IsNSString queryString, IsCSSearchQueryContext queryContext) => csSearchQuery -> queryString -> queryContext -> IO (Id CSSearchQuery)
initWithQueryString_queryContext csSearchQuery  queryString queryContext =
withObjCPtr queryString $ \raw_queryString ->
  withObjCPtr queryContext $ \raw_queryContext ->
      sendMsg csSearchQuery (mkSelector "initWithQueryString:queryContext:") (retPtr retVoid) [argPtr (castPtr raw_queryString :: Ptr ()), argPtr (castPtr raw_queryContext :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithQueryString:attributes:@
initWithQueryString_attributes :: (IsCSSearchQuery csSearchQuery, IsNSString queryString, IsNSArray attributes) => csSearchQuery -> queryString -> attributes -> IO (Id CSSearchQuery)
initWithQueryString_attributes csSearchQuery  queryString attributes =
withObjCPtr queryString $ \raw_queryString ->
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg csSearchQuery (mkSelector "initWithQueryString:attributes:") (retPtr retVoid) [argPtr (castPtr raw_queryString :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= ownedObject . castPtr

-- | @- start@
start :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO ()
start csSearchQuery  =
  sendMsg csSearchQuery (mkSelector "start") retVoid []

-- | @- cancel@
cancel :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO ()
cancel csSearchQuery  =
  sendMsg csSearchQuery (mkSelector "cancel") retVoid []

-- | @- cancelled@
cancelled :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO Bool
cancelled csSearchQuery  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csSearchQuery (mkSelector "cancelled") retCULong []

-- | @- foundItemCount@
foundItemCount :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO CULong
foundItemCount csSearchQuery  =
  sendMsg csSearchQuery (mkSelector "foundItemCount") retCULong []

-- | @- completionHandler@
completionHandler :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO (Ptr ())
completionHandler csSearchQuery  =
  fmap castPtr $ sendMsg csSearchQuery (mkSelector "completionHandler") (retPtr retVoid) []

-- | @- setCompletionHandler:@
setCompletionHandler :: IsCSSearchQuery csSearchQuery => csSearchQuery -> Ptr () -> IO ()
setCompletionHandler csSearchQuery  value =
  sendMsg csSearchQuery (mkSelector "setCompletionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- protectionClasses@
protectionClasses :: IsCSSearchQuery csSearchQuery => csSearchQuery -> IO (Id NSArray)
protectionClasses csSearchQuery  =
  sendMsg csSearchQuery (mkSelector "protectionClasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProtectionClasses:@
setProtectionClasses :: (IsCSSearchQuery csSearchQuery, IsNSArray value) => csSearchQuery -> value -> IO ()
setProtectionClasses csSearchQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchQuery (mkSelector "setProtectionClasses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithQueryString:queryContext:@
initWithQueryString_queryContextSelector :: Selector
initWithQueryString_queryContextSelector = mkSelector "initWithQueryString:queryContext:"

-- | @Selector@ for @initWithQueryString:attributes:@
initWithQueryString_attributesSelector :: Selector
initWithQueryString_attributesSelector = mkSelector "initWithQueryString:attributes:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @foundItemCount@
foundItemCountSelector :: Selector
foundItemCountSelector = mkSelector "foundItemCount"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

-- | @Selector@ for @protectionClasses@
protectionClassesSelector :: Selector
protectionClassesSelector = mkSelector "protectionClasses"

-- | @Selector@ for @setProtectionClasses:@
setProtectionClassesSelector :: Selector
setProtectionClassesSelector = mkSelector "setProtectionClasses:"

