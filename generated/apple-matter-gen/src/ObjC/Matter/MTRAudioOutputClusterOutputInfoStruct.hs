{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAudioOutputClusterOutputInfoStruct@.
module ObjC.Matter.MTRAudioOutputClusterOutputInfoStruct
  ( MTRAudioOutputClusterOutputInfoStruct
  , IsMTRAudioOutputClusterOutputInfoStruct(..)
  , index
  , setIndex
  , outputType
  , setOutputType
  , name
  , setName
  , indexSelector
  , nameSelector
  , outputTypeSelector
  , setIndexSelector
  , setNameSelector
  , setOutputTypeSelector


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
index :: IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct => mtrAudioOutputClusterOutputInfoStruct -> IO (Id NSNumber)
index mtrAudioOutputClusterOutputInfoStruct =
  sendMessage mtrAudioOutputClusterOutputInfoStruct indexSelector

-- | @- setIndex:@
setIndex :: (IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct, IsNSNumber value) => mtrAudioOutputClusterOutputInfoStruct -> value -> IO ()
setIndex mtrAudioOutputClusterOutputInfoStruct value =
  sendMessage mtrAudioOutputClusterOutputInfoStruct setIndexSelector (toNSNumber value)

-- | @- outputType@
outputType :: IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct => mtrAudioOutputClusterOutputInfoStruct -> IO (Id NSNumber)
outputType mtrAudioOutputClusterOutputInfoStruct =
  sendMessage mtrAudioOutputClusterOutputInfoStruct outputTypeSelector

-- | @- setOutputType:@
setOutputType :: (IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct, IsNSNumber value) => mtrAudioOutputClusterOutputInfoStruct -> value -> IO ()
setOutputType mtrAudioOutputClusterOutputInfoStruct value =
  sendMessage mtrAudioOutputClusterOutputInfoStruct setOutputTypeSelector (toNSNumber value)

-- | @- name@
name :: IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct => mtrAudioOutputClusterOutputInfoStruct -> IO (Id NSString)
name mtrAudioOutputClusterOutputInfoStruct =
  sendMessage mtrAudioOutputClusterOutputInfoStruct nameSelector

-- | @- setName:@
setName :: (IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct, IsNSString value) => mtrAudioOutputClusterOutputInfoStruct -> value -> IO ()
setName mtrAudioOutputClusterOutputInfoStruct value =
  sendMessage mtrAudioOutputClusterOutputInfoStruct setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @index@
indexSelector :: Selector '[] (Id NSNumber)
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector '[Id NSNumber] ()
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @outputType@
outputTypeSelector :: Selector '[] (Id NSNumber)
outputTypeSelector = mkSelector "outputType"

-- | @Selector@ for @setOutputType:@
setOutputTypeSelector :: Selector '[Id NSNumber] ()
setOutputTypeSelector = mkSelector "setOutputType:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

