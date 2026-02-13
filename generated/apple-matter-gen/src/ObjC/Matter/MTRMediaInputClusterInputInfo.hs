{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaInputClusterInputInfo@.
module ObjC.Matter.MTRMediaInputClusterInputInfo
  ( MTRMediaInputClusterInputInfo
  , IsMTRMediaInputClusterInputInfo(..)
  , index
  , setIndex
  , inputType
  , setInputType
  , name
  , setName
  , descriptionString
  , setDescriptionString
  , descriptionStringSelector
  , indexSelector
  , inputTypeSelector
  , nameSelector
  , setDescriptionStringSelector
  , setIndexSelector
  , setInputTypeSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- index@
index :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSNumber)
index mtrMediaInputClusterInputInfo =
  sendMessage mtrMediaInputClusterInputInfo indexSelector

-- | @- setIndex:@
setIndex :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSNumber value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setIndex mtrMediaInputClusterInputInfo value =
  sendMessage mtrMediaInputClusterInputInfo setIndexSelector (toNSNumber value)

-- | @- inputType@
inputType :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSNumber)
inputType mtrMediaInputClusterInputInfo =
  sendMessage mtrMediaInputClusterInputInfo inputTypeSelector

-- | @- setInputType:@
setInputType :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSNumber value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setInputType mtrMediaInputClusterInputInfo value =
  sendMessage mtrMediaInputClusterInputInfo setInputTypeSelector (toNSNumber value)

-- | @- name@
name :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSString)
name mtrMediaInputClusterInputInfo =
  sendMessage mtrMediaInputClusterInputInfo nameSelector

-- | @- setName:@
setName :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSString value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setName mtrMediaInputClusterInputInfo value =
  sendMessage mtrMediaInputClusterInputInfo setNameSelector (toNSString value)

-- | @- descriptionString@
descriptionString :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSString)
descriptionString mtrMediaInputClusterInputInfo =
  sendMessage mtrMediaInputClusterInputInfo descriptionStringSelector

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSString value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setDescriptionString mtrMediaInputClusterInputInfo value =
  sendMessage mtrMediaInputClusterInputInfo setDescriptionStringSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @index@
indexSelector :: Selector '[] (Id NSNumber)
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector '[Id NSNumber] ()
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @inputType@
inputTypeSelector :: Selector '[] (Id NSNumber)
inputTypeSelector = mkSelector "inputType"

-- | @Selector@ for @setInputType:@
setInputTypeSelector :: Selector '[Id NSNumber] ()
setInputTypeSelector = mkSelector "setInputType:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector '[] (Id NSString)
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector '[Id NSString] ()
setDescriptionStringSelector = mkSelector "setDescriptionString:"

