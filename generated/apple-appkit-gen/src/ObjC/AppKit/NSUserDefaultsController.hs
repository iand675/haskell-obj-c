{-# LANGUAGE DataKinds #-}
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
  , appliesImmediatelySelector
  , defaultsSelector
  , hasUnappliedChangesSelector
  , initWithCoderSelector
  , initWithDefaults_initialValuesSelector
  , initialValuesSelector
  , revertSelector
  , revertToInitialValuesSelector
  , saveSelector
  , setAppliesImmediatelySelector
  , setInitialValuesSelector
  , sharedUserDefaultsControllerSelector
  , valuesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDefaults:initialValues:@
initWithDefaults_initialValues :: (IsNSUserDefaultsController nsUserDefaultsController, IsNSUserDefaults defaults, IsNSDictionary initialValues) => nsUserDefaultsController -> defaults -> initialValues -> IO (Id NSUserDefaultsController)
initWithDefaults_initialValues nsUserDefaultsController defaults initialValues =
  sendOwnedMessage nsUserDefaultsController initWithDefaults_initialValuesSelector (toNSUserDefaults defaults) (toNSDictionary initialValues)

-- | @- initWithCoder:@
initWithCoder :: (IsNSUserDefaultsController nsUserDefaultsController, IsNSCoder coder) => nsUserDefaultsController -> coder -> IO (Id NSUserDefaultsController)
initWithCoder nsUserDefaultsController coder =
  sendOwnedMessage nsUserDefaultsController initWithCoderSelector (toNSCoder coder)

-- | @- revert:@
revert :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> RawId -> IO ()
revert nsUserDefaultsController sender =
  sendMessage nsUserDefaultsController revertSelector sender

-- | @- save:@
save :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> RawId -> IO ()
save nsUserDefaultsController sender =
  sendMessage nsUserDefaultsController saveSelector sender

-- | @- revertToInitialValues:@
revertToInitialValues :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> RawId -> IO ()
revertToInitialValues nsUserDefaultsController sender =
  sendMessage nsUserDefaultsController revertToInitialValuesSelector sender

-- | @+ sharedUserDefaultsController@
sharedUserDefaultsController :: IO (Id NSUserDefaultsController)
sharedUserDefaultsController  =
  do
    cls' <- getRequiredClass "NSUserDefaultsController"
    sendClassMessage cls' sharedUserDefaultsControllerSelector

-- | @- defaults@
defaults :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO (Id NSUserDefaults)
defaults nsUserDefaultsController =
  sendMessage nsUserDefaultsController defaultsSelector

-- | @- initialValues@
initialValues :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO (Id NSDictionary)
initialValues nsUserDefaultsController =
  sendOwnedMessage nsUserDefaultsController initialValuesSelector

-- | @- setInitialValues:@
setInitialValues :: (IsNSUserDefaultsController nsUserDefaultsController, IsNSDictionary value) => nsUserDefaultsController -> value -> IO ()
setInitialValues nsUserDefaultsController value =
  sendMessage nsUserDefaultsController setInitialValuesSelector (toNSDictionary value)

-- | @- appliesImmediately@
appliesImmediately :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO Bool
appliesImmediately nsUserDefaultsController =
  sendMessage nsUserDefaultsController appliesImmediatelySelector

-- | @- setAppliesImmediately:@
setAppliesImmediately :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> Bool -> IO ()
setAppliesImmediately nsUserDefaultsController value =
  sendMessage nsUserDefaultsController setAppliesImmediatelySelector value

-- | @- hasUnappliedChanges@
hasUnappliedChanges :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO Bool
hasUnappliedChanges nsUserDefaultsController =
  sendMessage nsUserDefaultsController hasUnappliedChangesSelector

-- | @- values@
values :: IsNSUserDefaultsController nsUserDefaultsController => nsUserDefaultsController -> IO RawId
values nsUserDefaultsController =
  sendMessage nsUserDefaultsController valuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDefaults:initialValues:@
initWithDefaults_initialValuesSelector :: Selector '[Id NSUserDefaults, Id NSDictionary] (Id NSUserDefaultsController)
initWithDefaults_initialValuesSelector = mkSelector "initWithDefaults:initialValues:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSUserDefaultsController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @revert:@
revertSelector :: Selector '[RawId] ()
revertSelector = mkSelector "revert:"

-- | @Selector@ for @save:@
saveSelector :: Selector '[RawId] ()
saveSelector = mkSelector "save:"

-- | @Selector@ for @revertToInitialValues:@
revertToInitialValuesSelector :: Selector '[RawId] ()
revertToInitialValuesSelector = mkSelector "revertToInitialValues:"

-- | @Selector@ for @sharedUserDefaultsController@
sharedUserDefaultsControllerSelector :: Selector '[] (Id NSUserDefaultsController)
sharedUserDefaultsControllerSelector = mkSelector "sharedUserDefaultsController"

-- | @Selector@ for @defaults@
defaultsSelector :: Selector '[] (Id NSUserDefaults)
defaultsSelector = mkSelector "defaults"

-- | @Selector@ for @initialValues@
initialValuesSelector :: Selector '[] (Id NSDictionary)
initialValuesSelector = mkSelector "initialValues"

-- | @Selector@ for @setInitialValues:@
setInitialValuesSelector :: Selector '[Id NSDictionary] ()
setInitialValuesSelector = mkSelector "setInitialValues:"

-- | @Selector@ for @appliesImmediately@
appliesImmediatelySelector :: Selector '[] Bool
appliesImmediatelySelector = mkSelector "appliesImmediately"

-- | @Selector@ for @setAppliesImmediately:@
setAppliesImmediatelySelector :: Selector '[Bool] ()
setAppliesImmediatelySelector = mkSelector "setAppliesImmediately:"

-- | @Selector@ for @hasUnappliedChanges@
hasUnappliedChangesSelector :: Selector '[] Bool
hasUnappliedChangesSelector = mkSelector "hasUnappliedChanges"

-- | @Selector@ for @values@
valuesSelector :: Selector '[] RawId
valuesSelector = mkSelector "values"

