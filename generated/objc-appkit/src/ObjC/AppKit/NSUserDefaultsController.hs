{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserDefaultsController@.
module ObjC.AppKit.NSUserDefaultsController
  ( NSUserDefaultsController
  , IsNSUserDefaultsController(..)
  , initWithDefaults_initialValues
  , initWithCoder
  , revert
  , save
  , revertToInitialValues
  , sharedUserDefaultsController
  , defaults
  , initialValues
  , setInitialValues
  , appliesImmediately
  , setAppliesImmediately
  , hasUnappliedChanges
  , values
  , initWithDefaults_initialValuesSelector
  , initWithCoderSelector
  , revertSelector
  , saveSelector
  , revertToInitialValuesSelector
  , sharedUserDefaultsControllerSelector
  , defaultsSelector
  , initialValuesSelector
  , setInitialValuesSelector
  , appliesImmediatelySelector
  , setAppliesImmediatelySelector
  , hasUnappliedChangesSelector
  , valuesSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDefaults:initialValues:@
initWithDefaults_initialValues :: (IsNSUserDefaultsController nsUserDefaultsController, IsNSUserDefaults defaults, IsNSDictionary initialValues) => nsUserDefaultsController -> defaults -> initialValues -> IO (Id NSUserDefaultsController)
initWithDefaults_initialValues nsUserDefaultsController  defaults initialValues =
withObjCPtr defaults $ \raw_defaults ->
  withObjCPtr initialValues $ \raw_initialValues ->
      sendMsg nsUserDefaultsController (mkSelector "initWithDefaults:initialValues:") (retPtr retVoid) [argPtr (castPtr raw_defaults :: Ptr ()), argPtr (castPtr raw_initialValues :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSUserDefaultsController nsUserDefaultsController, IsNSCoder coder) => nsUserDefaultsController -> coder -> IO (Id NSUserDefaultsController)
initWithCoder nsUserDefaultsController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsUserDefaultsController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- revert:@
revert :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> RawId -> IO ()
revert nsUserDefaultsController  sender =
  sendMsg nsUserDefaultsController (mkSelector "revert:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- save:@
save :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> RawId -> IO ()
save nsUserDefaultsController  sender =
  sendMsg nsUserDefaultsController (mkSelector "save:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- revertToInitialValues:@
revertToInitialValues :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> RawId -> IO ()
revertToInitialValues nsUserDefaultsController  sender =
  sendMsg nsUserDefaultsController (mkSelector "revertToInitialValues:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @+ sharedUserDefaultsController@
sharedUserDefaultsController :: IO (Id NSUserDefaultsController)
sharedUserDefaultsController  =
  do
    cls' <- getRequiredClass "NSUserDefaultsController"
    sendClassMsg cls' (mkSelector "sharedUserDefaultsController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defaults@
defaults :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO (Id NSUserDefaults)
defaults nsUserDefaultsController  =
  sendMsg nsUserDefaultsController (mkSelector "defaults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initialValues@
initialValues :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO (Id NSDictionary)
initialValues nsUserDefaultsController  =
  sendMsg nsUserDefaultsController (mkSelector "initialValues") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialValues:@
setInitialValues :: (IsNSUserDefaultsController nsUserDefaultsController, IsNSDictionary value) => nsUserDefaultsController -> value -> IO ()
setInitialValues nsUserDefaultsController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserDefaultsController (mkSelector "setInitialValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- appliesImmediately@
appliesImmediately :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO Bool
appliesImmediately nsUserDefaultsController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserDefaultsController (mkSelector "appliesImmediately") retCULong []

-- | @- setAppliesImmediately:@
setAppliesImmediately :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> Bool -> IO ()
setAppliesImmediately nsUserDefaultsController  value =
  sendMsg nsUserDefaultsController (mkSelector "setAppliesImmediately:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasUnappliedChanges@
hasUnappliedChanges :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO Bool
hasUnappliedChanges nsUserDefaultsController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserDefaultsController (mkSelector "hasUnappliedChanges") retCULong []

-- | @- values@
values :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO RawId
values nsUserDefaultsController  =
  fmap (RawId . castPtr) $ sendMsg nsUserDefaultsController (mkSelector "values") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDefaults:initialValues:@
initWithDefaults_initialValuesSelector :: Selector
initWithDefaults_initialValuesSelector = mkSelector "initWithDefaults:initialValues:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @revert:@
revertSelector :: Selector
revertSelector = mkSelector "revert:"

-- | @Selector@ for @save:@
saveSelector :: Selector
saveSelector = mkSelector "save:"

-- | @Selector@ for @revertToInitialValues:@
revertToInitialValuesSelector :: Selector
revertToInitialValuesSelector = mkSelector "revertToInitialValues:"

-- | @Selector@ for @sharedUserDefaultsController@
sharedUserDefaultsControllerSelector :: Selector
sharedUserDefaultsControllerSelector = mkSelector "sharedUserDefaultsController"

-- | @Selector@ for @defaults@
defaultsSelector :: Selector
defaultsSelector = mkSelector "defaults"

-- | @Selector@ for @initialValues@
initialValuesSelector :: Selector
initialValuesSelector = mkSelector "initialValues"

-- | @Selector@ for @setInitialValues:@
setInitialValuesSelector :: Selector
setInitialValuesSelector = mkSelector "setInitialValues:"

-- | @Selector@ for @appliesImmediately@
appliesImmediatelySelector :: Selector
appliesImmediatelySelector = mkSelector "appliesImmediately"

-- | @Selector@ for @setAppliesImmediately:@
setAppliesImmediatelySelector :: Selector
setAppliesImmediatelySelector = mkSelector "setAppliesImmediately:"

-- | @Selector@ for @hasUnappliedChanges@
hasUnappliedChangesSelector :: Selector
hasUnappliedChangesSelector = mkSelector "hasUnappliedChanges"

-- | @Selector@ for @values@
valuesSelector :: Selector
valuesSelector = mkSelector "values"

