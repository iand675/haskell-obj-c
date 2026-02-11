{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPropertyMapping@.
module ObjC.CoreData.NSPropertyMapping
  ( NSPropertyMapping
  , IsNSPropertyMapping(..)
  , name
  , setName
  , valueExpression
  , setValueExpression
  , userInfo
  , setUserInfo
  , nameSelector
  , setNameSelector
  , valueExpressionSelector
  , setValueExpressionSelector
  , userInfoSelector
  , setUserInfoSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsNSPropertyMapping nsPropertyMapping => nsPropertyMapping -> IO (Id NSString)
name nsPropertyMapping  =
  sendMsg nsPropertyMapping (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSPropertyMapping nsPropertyMapping, IsNSString value) => nsPropertyMapping -> value -> IO ()
setName nsPropertyMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyMapping (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueExpression@
valueExpression :: IsNSPropertyMapping nsPropertyMapping => nsPropertyMapping -> IO (Id NSExpression)
valueExpression nsPropertyMapping  =
  sendMsg nsPropertyMapping (mkSelector "valueExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueExpression:@
setValueExpression :: (IsNSPropertyMapping nsPropertyMapping, IsNSExpression value) => nsPropertyMapping -> value -> IO ()
setValueExpression nsPropertyMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyMapping (mkSelector "setValueExpression:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSPropertyMapping nsPropertyMapping => nsPropertyMapping -> IO (Id NSDictionary)
userInfo nsPropertyMapping  =
  sendMsg nsPropertyMapping (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSPropertyMapping nsPropertyMapping, IsNSDictionary value) => nsPropertyMapping -> value -> IO ()
setUserInfo nsPropertyMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyMapping (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @valueExpression@
valueExpressionSelector :: Selector
valueExpressionSelector = mkSelector "valueExpression"

-- | @Selector@ for @setValueExpression:@
setValueExpressionSelector :: Selector
setValueExpressionSelector = mkSelector "setValueExpression:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

