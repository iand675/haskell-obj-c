{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLQueryItem@.
module ObjC.Foundation.NSURLQueryItem
  ( NSURLQueryItem
  , IsNSURLQueryItem(..)
  , initWithName_value
  , queryItemWithName_value
  , name
  , value
  , initWithName_valueSelector
  , nameSelector
  , queryItemWithName_valueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithName:value:@
initWithName_value :: (IsNSURLQueryItem nsurlQueryItem, IsNSString name, IsNSString value) => nsurlQueryItem -> name -> value -> IO (Id NSURLQueryItem)
initWithName_value nsurlQueryItem name value =
  sendOwnedMessage nsurlQueryItem initWithName_valueSelector (toNSString name) (toNSString value)

-- | @+ queryItemWithName:value:@
queryItemWithName_value :: (IsNSString name, IsNSString value) => name -> value -> IO (Id NSURLQueryItem)
queryItemWithName_value name value =
  do
    cls' <- getRequiredClass "NSURLQueryItem"
    sendClassMessage cls' queryItemWithName_valueSelector (toNSString name) (toNSString value)

-- | @- name@
name :: IsNSURLQueryItem nsurlQueryItem => nsurlQueryItem -> IO (Id NSString)
name nsurlQueryItem =
  sendMessage nsurlQueryItem nameSelector

-- | @- value@
value :: IsNSURLQueryItem nsurlQueryItem => nsurlQueryItem -> IO (Id NSString)
value nsurlQueryItem =
  sendMessage nsurlQueryItem valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:value:@
initWithName_valueSelector :: Selector '[Id NSString, Id NSString] (Id NSURLQueryItem)
initWithName_valueSelector = mkSelector "initWithName:value:"

-- | @Selector@ for @queryItemWithName:value:@
queryItemWithName_valueSelector :: Selector '[Id NSString, Id NSString] (Id NSURLQueryItem)
queryItemWithName_valueSelector = mkSelector "queryItemWithName:value:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

