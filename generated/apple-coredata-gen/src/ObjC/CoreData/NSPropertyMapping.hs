{-# LANGUAGE DataKinds #-}
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
  , setUserInfoSelector
  , setValueExpressionSelector
  , userInfoSelector
  , valueExpressionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsNSPropertyMapping nsPropertyMapping => nsPropertyMapping -> IO (Id NSString)
name nsPropertyMapping =
  sendMessage nsPropertyMapping nameSelector

-- | @- setName:@
setName :: (IsNSPropertyMapping nsPropertyMapping, IsNSString value) => nsPropertyMapping -> value -> IO ()
setName nsPropertyMapping value =
  sendMessage nsPropertyMapping setNameSelector (toNSString value)

-- | @- valueExpression@
valueExpression :: IsNSPropertyMapping nsPropertyMapping => nsPropertyMapping -> IO (Id NSExpression)
valueExpression nsPropertyMapping =
  sendMessage nsPropertyMapping valueExpressionSelector

-- | @- setValueExpression:@
setValueExpression :: (IsNSPropertyMapping nsPropertyMapping, IsNSExpression value) => nsPropertyMapping -> value -> IO ()
setValueExpression nsPropertyMapping value =
  sendMessage nsPropertyMapping setValueExpressionSelector (toNSExpression value)

-- | @- userInfo@
userInfo :: IsNSPropertyMapping nsPropertyMapping => nsPropertyMapping -> IO (Id NSDictionary)
userInfo nsPropertyMapping =
  sendMessage nsPropertyMapping userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSPropertyMapping nsPropertyMapping, IsNSDictionary value) => nsPropertyMapping -> value -> IO ()
setUserInfo nsPropertyMapping value =
  sendMessage nsPropertyMapping setUserInfoSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @valueExpression@
valueExpressionSelector :: Selector '[] (Id NSExpression)
valueExpressionSelector = mkSelector "valueExpression"

-- | @Selector@ for @setValueExpression:@
setValueExpressionSelector :: Selector '[Id NSExpression] ()
setValueExpressionSelector = mkSelector "setValueExpression:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

