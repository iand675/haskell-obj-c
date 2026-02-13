{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaInputClusterInputInfoStruct@.
module ObjC.Matter.MTRMediaInputClusterInputInfoStruct
  ( MTRMediaInputClusterInputInfoStruct
  , IsMTRMediaInputClusterInputInfoStruct(..)
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
index :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSNumber)
index mtrMediaInputClusterInputInfoStruct =
  sendMessage mtrMediaInputClusterInputInfoStruct indexSelector

-- | @- setIndex:@
setIndex :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSNumber value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setIndex mtrMediaInputClusterInputInfoStruct value =
  sendMessage mtrMediaInputClusterInputInfoStruct setIndexSelector (toNSNumber value)

-- | @- inputType@
inputType :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSNumber)
inputType mtrMediaInputClusterInputInfoStruct =
  sendMessage mtrMediaInputClusterInputInfoStruct inputTypeSelector

-- | @- setInputType:@
setInputType :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSNumber value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setInputType mtrMediaInputClusterInputInfoStruct value =
  sendMessage mtrMediaInputClusterInputInfoStruct setInputTypeSelector (toNSNumber value)

-- | @- name@
name :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSString)
name mtrMediaInputClusterInputInfoStruct =
  sendMessage mtrMediaInputClusterInputInfoStruct nameSelector

-- | @- setName:@
setName :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSString value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setName mtrMediaInputClusterInputInfoStruct value =
  sendMessage mtrMediaInputClusterInputInfoStruct setNameSelector (toNSString value)

-- | @- descriptionString@
descriptionString :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSString)
descriptionString mtrMediaInputClusterInputInfoStruct =
  sendMessage mtrMediaInputClusterInputInfoStruct descriptionStringSelector

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSString value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setDescriptionString mtrMediaInputClusterInputInfoStruct value =
  sendMessage mtrMediaInputClusterInputInfoStruct setDescriptionStringSelector (toNSString value)

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

