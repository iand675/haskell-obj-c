{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAudioOutputClusterOutputInfo@.
module ObjC.Matter.MTRAudioOutputClusterOutputInfo
  ( MTRAudioOutputClusterOutputInfo
  , IsMTRAudioOutputClusterOutputInfo(..)
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
index :: IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo => mtrAudioOutputClusterOutputInfo -> IO (Id NSNumber)
index mtrAudioOutputClusterOutputInfo =
  sendMessage mtrAudioOutputClusterOutputInfo indexSelector

-- | @- setIndex:@
setIndex :: (IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo, IsNSNumber value) => mtrAudioOutputClusterOutputInfo -> value -> IO ()
setIndex mtrAudioOutputClusterOutputInfo value =
  sendMessage mtrAudioOutputClusterOutputInfo setIndexSelector (toNSNumber value)

-- | @- outputType@
outputType :: IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo => mtrAudioOutputClusterOutputInfo -> IO (Id NSNumber)
outputType mtrAudioOutputClusterOutputInfo =
  sendMessage mtrAudioOutputClusterOutputInfo outputTypeSelector

-- | @- setOutputType:@
setOutputType :: (IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo, IsNSNumber value) => mtrAudioOutputClusterOutputInfo -> value -> IO ()
setOutputType mtrAudioOutputClusterOutputInfo value =
  sendMessage mtrAudioOutputClusterOutputInfo setOutputTypeSelector (toNSNumber value)

-- | @- name@
name :: IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo => mtrAudioOutputClusterOutputInfo -> IO (Id NSString)
name mtrAudioOutputClusterOutputInfo =
  sendMessage mtrAudioOutputClusterOutputInfo nameSelector

-- | @- setName:@
setName :: (IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo, IsNSString value) => mtrAudioOutputClusterOutputInfo -> value -> IO ()
setName mtrAudioOutputClusterOutputInfo value =
  sendMessage mtrAudioOutputClusterOutputInfo setNameSelector (toNSString value)

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

