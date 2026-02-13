{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLModelCollection
--
-- A collection of models managed as part of Core ML Model Deployment.
--
-- Generated bindings for @MLModelCollection@.
module ObjC.CoreML.MLModelCollection
  ( MLModelCollection
  , IsMLModelCollection(..)
  , beginAccessingModelCollectionWithIdentifier_completionHandler
  , endAccessingModelCollectionWithIdentifier_completionHandler
  , init_
  , new
  , identifier
  , deploymentID
  , beginAccessingModelCollectionWithIdentifier_completionHandlerSelector
  , deploymentIDSelector
  , endAccessingModelCollectionWithIdentifier_completionHandlerSelector
  , identifierSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Request access to a model collection. If the collection is not downloaded on the device, it is requested  from Core ML Model Deployment.
--
-- When called, this method downloads the model collection if it is not already on the device. Once  all models are downloaded, an MLModelCollection instance is made available for use with the completion handler.
--
-- @identifier@ — The model collection identifier, as managed in Core ML Model Deployment.
--
-- @completionHandler@ — The completion handler, invoked with a valid MLModelCollection instance on success or NSError on failure.
--
-- Returns: NSProgress for updates during setup and download of the model collection
--
-- ObjC selector: @+ beginAccessingModelCollectionWithIdentifier:completionHandler:@
beginAccessingModelCollectionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO (Id NSProgress)
beginAccessingModelCollectionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "MLModelCollection"
    sendClassMessage cls' beginAccessingModelCollectionWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | End access to a model collection. This informs the system you have finished accessing the models within the collection.
--
-- Call this method as soon as you have finished using the models in this collection.
--
-- @identifier@ — The model collection identifier, as managed in Core ML Model Deployment.
--
-- @completionHandler@ — The completion handler, invoked with YES on success or NSError on failure.
--
-- ObjC selector: @+ endAccessingModelCollectionWithIdentifier:completionHandler:@
endAccessingModelCollectionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
endAccessingModelCollectionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "MLModelCollection"
    sendClassMessage cls' endAccessingModelCollectionWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | @- init@
init_ :: IsMLModelCollection mlModelCollection => mlModelCollection -> IO (Id MLModelCollection)
init_ mlModelCollection =
  sendOwnedMessage mlModelCollection initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLModelCollection"
    sendOwnedClassMessage cls' newSelector

-- | The identifier of the model collection you want to access, as configured in the Core ML Model Deployment dashboard.
--
-- ObjC selector: @- identifier@
identifier :: IsMLModelCollection mlModelCollection => mlModelCollection -> IO RawId
identifier mlModelCollection =
  sendMessage mlModelCollection identifierSelector

-- | The identifier for the currently downloaded deployment, corresponding to a recent deployment on the Core ML Model Deployment dashboard.
--
-- ObjC selector: @- deploymentID@
deploymentID :: IsMLModelCollection mlModelCollection => mlModelCollection -> IO RawId
deploymentID mlModelCollection =
  sendMessage mlModelCollection deploymentIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginAccessingModelCollectionWithIdentifier:completionHandler:@
beginAccessingModelCollectionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSProgress)
beginAccessingModelCollectionWithIdentifier_completionHandlerSelector = mkSelector "beginAccessingModelCollectionWithIdentifier:completionHandler:"

-- | @Selector@ for @endAccessingModelCollectionWithIdentifier:completionHandler:@
endAccessingModelCollectionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
endAccessingModelCollectionWithIdentifier_completionHandlerSelector = mkSelector "endAccessingModelCollectionWithIdentifier:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelCollection)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] RawId
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @deploymentID@
deploymentIDSelector :: Selector '[] RawId
deploymentIDSelector = mkSelector "deploymentID"

