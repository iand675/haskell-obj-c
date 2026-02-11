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
  , queryItemWithName_valueSelector
  , nameSelector
  , valueSelector


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

-- | @- initWithName:value:@
initWithName_value :: (IsNSURLQueryItem nsurlQueryItem, IsNSString name, IsNSString value) => nsurlQueryItem -> name -> value -> IO (Id NSURLQueryItem)
initWithName_value nsurlQueryItem  name value =
withObjCPtr name $ \raw_name ->
  withObjCPtr value $ \raw_value ->
      sendMsg nsurlQueryItem (mkSelector "initWithName:value:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | @+ queryItemWithName:value:@
queryItemWithName_value :: (IsNSString name, IsNSString value) => name -> value -> IO (Id NSURLQueryItem)
queryItemWithName_value name value =
  do
    cls' <- getRequiredClass "NSURLQueryItem"
    withObjCPtr name $ \raw_name ->
      withObjCPtr value $ \raw_value ->
        sendClassMsg cls' (mkSelector "queryItemWithName:value:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSURLQueryItem nsurlQueryItem => nsurlQueryItem -> IO (Id NSString)
name nsurlQueryItem  =
  sendMsg nsurlQueryItem (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsNSURLQueryItem nsurlQueryItem => nsurlQueryItem -> IO (Id NSString)
value nsurlQueryItem  =
  sendMsg nsurlQueryItem (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:value:@
initWithName_valueSelector :: Selector
initWithName_valueSelector = mkSelector "initWithName:value:"

-- | @Selector@ for @queryItemWithName:value:@
queryItemWithName_valueSelector :: Selector
queryItemWithName_valueSelector = mkSelector "queryItemWithName:value:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

